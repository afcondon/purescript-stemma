-- | A `Drawing` as Halogen SVG.
-- |
-- | This draws only what `Graphviz.Layout` hands over, and reads a few
-- | attributes for what the eye needs: a node's shape, its fill and
-- | outline colours, `style` (filled, dashed, dotted, bold, rounded,
-- | invis), `fontsize`, an edge's direction and colour. Anything else in
-- | the DOT file is ignored, as `Graphviz.Svg` ignores it.
module Site.Picture
  ( picture
  , TurnedEdges(..)
  , arrowhead
  , node
  , edge
  , cluster
  , nameOf
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Graphviz.Attributes (Attributes)
import Graphviz.Attributes as Attributes
import Graphviz.Dot.Syntax (Text(..), text)
import Graphviz.Graph (NodeName(..))
import Graphviz.Label as Markup
import Graphviz.Layout (DrawnCluster, DrawnEdge, DrawnNode, Drawing, Outline(..), Point, outlineOf)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Baseline(..), Color(..), CommandPositionReference(..), MarkerUnit(..), Orient(..), TextAnchor(..), printColor)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Site.Colour (colour)
import Site.Label as Label

-- | Whether edges the layout turned round to break a cycle are picked
-- | out in the accent colour.
data TurnedEdges = Marked | Unmarked

derive instance eqTurnedEdges :: Eq TurnedEdges

picture :: forall w i. TurnedEdges -> Drawing -> HH.HTML w i
picture turnedEdges drawing =
  SE.svg
    [ SA.viewBox 0.0 0.0 drawing.width drawing.height
    -- The drawing is in points, and the page counts in CSS pixels, of
    -- which there are 96 to the inch against 72.
    , SA.width (drawing.width * drawing.scale * 96.0 / 72.0)
    , SA.height (drawing.height * drawing.scale * 96.0 / 72.0)
    , SA.class_ (ClassName "drawing")
    ]
    ( [ SE.defs [] [ arrowhead ] ]
        <> map cluster drawing.clusters
        <> map (edge turnedEdges drawing.directed) (Array.filter (\found -> not (hidden found.attributes)) drawing.edges)
        <> map node (Array.filter (\found -> not (hidden found.attributes)) drawing.nodes)
    )

-- The palette. Colours are set as attributes, never left to the
-- stylesheet, because CSS would override the colours a DOT file asks for.
ink :: Color
ink = Named "#1a1a1a"

paper :: Color
paper = Named "#ffffff"

rule :: Color
rule = Named "#9a9a9a"

accent :: Color
accent = Named "#d4351c"

-- | Graphviz fills a `filled` node or cluster with no colour of its own in
-- | light grey.
filledByDefault :: Color
filledByDefault = Named "lightgrey"

-- | Drawn in the edge's own colour wherever the browser supports
-- | `context-stroke`; the stylesheet says so.
arrowhead :: forall w i. HH.HTML w i
arrowhead =
  SE.marker
    [ SA.id "arrowhead"
    , SA.markerUnits StrokeWidth
    , SA.markerWidth 9.0
    , SA.markerHeight 7.0
    , SA.refX 9.0
    , SA.refY 3.5
    , SA.orient AutoStartReverse
    ]
    [ SE.path [ SA.class_ (ClassName "arrowhead"), SA.d [ SA.m Abs 0.0 0.0, SA.l Abs 9.0 3.5, SA.l Abs 0.0 7.0, SA.z ] ] ]

-- Styles ---------------------------------------------------------------

data Style = Filled | Dashed | Dotted | Bold | Rounded | Invisible

derive instance eqStyle :: Eq Style
derive instance ordStyle :: Ord Style

-- | `style` is a comma-separated list; words the page does not draw are
-- | dropped.
styles :: Attributes -> Set Style
styles attributes = case Attributes.lookup "style" attributes of
  Nothing -> Set.empty
  Just written -> Set.fromFoldable (Array.mapMaybe (trim >>> word) (split (Pattern ",") written))
  where
  word = case _ of
    "filled" -> Just Filled
    "dashed" -> Just Dashed
    "dotted" -> Just Dotted
    "bold" -> Just Bold
    "rounded" -> Just Rounded
    "invis" -> Just Invisible
    _ -> Nothing

hidden :: Attributes -> Boolean
hidden = styles >>> Set.member Invisible

-- | The first of the named attributes that holds a colour SVG can show.
colourOf :: Array String -> Attributes -> Maybe Color
colourOf keys attributes = Array.findMap (\key -> Attributes.lookup key attributes >>= colour) keys

strokeProperties
  :: forall r i
   . Set Style
  -> Color
  -> Array (HH.IProp (stroke :: String, strokeWidth :: Number, strokeDashArray :: String | r) i)
strokeProperties found strokeColour =
  [ SA.stroke strokeColour
  , SA.strokeWidth (if Set.member Bold found then 2.0 else 1.0)
  ]
    <> dashes
  where
  dashes
    | Set.member Dashed found = [ SA.strokeDashArray "5,3" ]
    | Set.member Dotted found = [ SA.strokeDashArray "1,3" ]
    | otherwise = []

-- Nodes ----------------------------------------------------------------

node :: forall w i. DrawnNode -> HH.HTML w i
node found =
  SE.g [ SA.class_ (ClassName "node") ]
    ( [ SE.title [] [ HH.text (nodeDescription found) ] ]
        <> outline (outlineOf found.attributes)
        <> if point then []
          else case Attributes.lookupText "label" found.attributes of
            -- Lines that may each be bold or have a size of their own.
            Just (Html markup) | not (Markup.hasTable markup) ->
              markedLabel (ClassName "node-label") (fontSizeOf Label.fontSize found.attributes) found.box.centre
                (fromMaybe ink (colourOf [ "fontcolor" ] found.attributes))
                (Markup.markupLines markup)
            _ ->
              labelText (ClassName "node-label") (fontSizeOf Label.fontSize found.attributes) found.box.centre
                (fromMaybe ink (colourOf [ "fontcolor" ] found.attributes))
                (shownLabel found)
    )
  where
  box = found.box
  drawnStyles = styles found.attributes
  -- Graphviz draws a `point` as a dot filled with its colour, and shows
  -- no label on it.
  point = Attributes.lookup "shape" found.attributes == Just "point"
  fillColour =
    if point then fromMaybe ink (colourOf [ "fillcolor", "color" ] found.attributes)
    else if Set.member Filled drawnStyles then fromMaybe filledByDefault (colourOf [ "fillcolor", "color" ] found.attributes)
    else paper
  strokeColour = fromMaybe ink (colourOf [ "color" ] found.attributes)
  outline = case _ of
    Ellipse -> [ ellipse 0.0 ]
    DoubleEllipse -> [ ellipse 0.0, ellipse 4.0 ]
    Rectangle -> [ rectangle ]
    Diamond -> [ diamond ]
    NoOutline -> []
  ellipse inset =
    SE.ellipse
      ( [ SA.cx box.centre.x
        , SA.cy box.centre.y
        , SA.rx (box.width / 2.0 - inset)
        , SA.ry (box.height / 2.0 - inset)
        , SA.fill (if inset > 0.0 then Named "none" else fillColour)
        ] <> strokeProperties drawnStyles strokeColour
      )
  rectangle =
    SE.rect
      ( [ SA.x (box.centre.x - box.width / 2.0)
        , SA.y (box.centre.y - box.height / 2.0)
        , SA.width box.width
        , SA.height box.height
        , SA.rx (if Set.member Rounded drawnStyles then 6.0 else 0.0)
        , SA.fill fillColour
        ] <> strokeProperties drawnStyles strokeColour
      )
  diamond =
    SE.polygon
      ( [ SA.points
            [ Tuple box.centre.x (box.centre.y - box.height / 2.0)
            , Tuple (box.centre.x + box.width / 2.0) box.centre.y
            , Tuple box.centre.x (box.centre.y + box.height / 2.0)
            , Tuple (box.centre.x - box.width / 2.0) box.centre.y
            ]
        -- halogen-svg-elems does not list `fill` among a polygon's
        -- attributes, though SVG allows it, so it is set by name here.
        , HP.attr (AttrName "fill") (printColor fillColour)
        ] <> strokeProperties drawnStyles strokeColour
      )

-- | An HTML label (`<...>`) is shown as its text, without the markup.
-- | The layout measured it with the markup, so its box is wider than the
-- | text drawn in it.
shownLabel :: DrawnNode -> String
shownLabel found = case Attributes.lookupText "label" found.attributes of
  Just (Html _) -> withoutTags found.label
  _ -> found.label

withoutTags :: String -> String
withoutTags written =
  CodeUnits.fromCharArray
    ( ( Array.foldl
          ( \state character -> case character, state.inTag of
              '<', _ -> state { inTag = true }
              '>', true -> state { inTag = false }
              _, true -> state
              _, false -> state { kept = Array.snoc state.kept character }
          )
          { inTag: false, kept: [] }
          (CodeUnits.toCharArray written)
      ).kept
    )

nodeDescription :: DrawnNode -> String
nodeDescription found =
  let
    name = nameOf found.name
  in
    if name == found.label then name else name <> " (“" <> found.label <> "”)"

nameOf :: NodeName -> String
nameOf (NodeName written) = text written

-- | A label's size in points: its `fontsize`, as the layout reads it when
-- | it makes room for the label, so that the text drawn fits that room.
fontSizeOf :: Number -> Attributes -> Number
fontSizeOf otherwise attributes = fromMaybe otherwise (Attributes.lookup "fontsize" attributes >>= Number.fromString)

-- | One `text` element per line, centred as a block on the point, at the
-- | given size in points with Label's spacing between lines. The size is
-- | set on the element, where it overrides the stylesheet.
-- | The lines of an HTML-like label, each at its own size and weight, one
-- | under another and centred as a block on the point. `usual` is the size
-- | of a line that gives none.
markedLabel :: forall w i. ClassName -> Number -> Point -> Color -> Array Markup.Line -> Array (HH.HTML w i)
markedLabel className usual centre fillColour lines =
  (Array.foldl place { top: centre.y - total / 2.0, drawn: [] } lines).drawn
  where
  sizeOf line = fromMaybe usual line.size
  heightOf line = Label.lineHeight * sizeOf line / Label.fontSize
  total = Array.foldl (\soFar line -> soFar + heightOf line) 0.0 lines
  place state line =
    { top: state.top + heightOf line
    , drawn: Array.snoc state.drawn
        ( SE.text
            [ SA.class_ className
            , SA.x centre.x
            , SA.y (state.top + heightOf line / 2.0)
            , SA.textAnchor AnchorMiddle
            , SA.dominantBaseline Central
            , SA.fill fillColour
            , HP.attr (AttrName "font-size") (show (sizeOf line))
            , HP.attr (AttrName "font-weight") (if line.bold then "700" else "400")
            ]
            [ HH.text line.text ]
        )
    }

labelText :: forall w i. ClassName -> Number -> Point -> Color -> String -> Array (HH.HTML w i)
labelText className size centre fillColour written =
  let
    lines = Label.lines written
    count = Array.length lines
    spacing = Label.lineHeight * size / Label.fontSize
  in
    Array.mapWithIndex
      ( \position line ->
          SE.text
            [ SA.class_ className
            , SA.x centre.x
            , SA.y (centre.y + spacing * (Int.toNumber position - Int.toNumber (count - 1) / 2.0))
            , SA.textAnchor AnchorMiddle
            , SA.dominantBaseline Central
            , SA.fill fillColour
            , HP.attr (AttrName "font-size") (show size)
            ]
            [ HH.text line ]
      )
      lines

-- Edges ----------------------------------------------------------------

data Direction = Forward | Back | BothWays | Neither

directionOf :: Boolean -> Attributes -> Direction
directionOf directed attributes = case Attributes.lookup "dir" attributes of
  Just "forward" -> Forward
  Just "back" -> Back
  Just "both" -> BothWays
  Just "none" -> Neither
  _ -> if directed then Forward else Neither

edge :: forall w i. TurnedEdges -> Boolean -> DrawnEdge -> HH.HTML w i
edge turnedEdges directed found =
  SE.g [ SA.class_ (ClassName (if picked then "edge edge--turned" else "edge")) ]
    ( [ SE.title [] [ HH.text description ]
      , SE.path
          ( [ SA.d (curve found.path), SA.fill SA.NoColor ]
              <> strokeProperties (styles found.attributes) strokeColour
              <> arrows
          )
      ]
        <> case Attributes.lookup "label" found.attributes, found.labelAt of
          Just written, Just centre -> labelText (ClassName "edge-label") (fontSizeOf Label.fontSize found.attributes) centre (if picked then accent else ink) written
          _, _ -> []
    )
  where
  picked = found.turned && turnedEdges == Marked
  strokeColour = if picked then accent else fromMaybe ink (colourOf [ "color" ] found.attributes)
  description =
    nameOf found.tail <> (if directed then " → " else " — ") <> nameOf found.head
      <> (if found.turned then ", turned round to break a cycle" else "")
  arrows = case directionOf directed found.attributes of
    Forward -> [ SA.markerEnd "url(#arrowhead)" ]
    Back -> [ SA.markerStart "url(#arrowhead)" ]
    BothWays -> [ SA.markerStart "url(#arrowhead)", SA.markerEnd "url(#arrowhead)" ]
    Neither -> []
  -- The start, then three points for each piece of the curve.
  curve path = case Array.uncons path of
    Nothing -> []
    Just first -> [ SA.m Abs first.head.x first.head.y ] <> pieces first.tail
  pieces rest = case Array.take 3 rest of
    [ one, two, end ] -> [ SA.c Abs one.x one.y two.x two.y end.x end.y ] <> pieces (Array.drop 3 rest)
    _ -> []

-- Clusters -------------------------------------------------------------

cluster :: forall w i. DrawnCluster -> HH.HTML w i
cluster found =
  SE.g [ SA.class_ (ClassName "cluster") ]
    ( [ SE.rect
          ( [ SA.x (box.centre.x - box.width / 2.0)
            , SA.y top
            , SA.width box.width
            , SA.height box.height
            , SA.rx (if Set.member Rounded drawnStyles then 6.0 else 0.0)
            , SA.fill fillColour
            ] <> strokeProperties drawnStyles strokeColour
          )
      ]
        <> case found.label of
          Just written -> labelText (ClassName "cluster-label") (fontSizeOf 12.0 found.attributes) { x: box.centre.x, y: top + 9.0 }
            (fromMaybe ink (colourOf [ "fontcolor" ] found.attributes))
            written
          Nothing -> []
    )
  where
  box = found.box
  top = box.centre.y - box.height / 2.0
  drawnStyles = styles found.attributes
  fillColour =
    if Set.member Filled drawnStyles then fromMaybe filledByDefault (colourOf [ "fillcolor", "color", "bgcolor" ] found.attributes)
    else fromMaybe (Named "none") (colourOf [ "bgcolor" ] found.attributes)
  strokeColour = fromMaybe rule (colourOf [ "pencolor", "color" ] found.attributes)
