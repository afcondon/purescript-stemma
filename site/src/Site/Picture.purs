-- | A `Drawing` as Halogen SVG.
-- |
-- | This draws only what `Graphviz.Layout` hands over, and reads a few
-- | attributes for what the eye needs: a node's shape, its fill and
-- | outline colours, `style` (filled, dashed, dotted, bold, rounded,
-- | invis), an edge's direction and colour. Anything else in the DOT file
-- | is ignored, as `Graphviz.Svg` ignores it.
-- |
-- | Two things are the page's own and not the layout's: where an edge
-- | stops at a node's outline, and where an edge's label is put (at the
-- | middle of its path, since the layout does not place edge labels yet).
module Site.Picture
  ( picture
  , TurnedEdges(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Graphviz.Attributes (Attributes)
import Graphviz.Attributes as Attributes
import Graphviz.Dot.Syntax (Text(..), text)
import Graphviz.Graph (NodeName(..))
import Graphviz.Layout (Box, DrawnCluster, DrawnEdge, DrawnNode, Drawing, Point)
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
    , SA.width drawing.width
    , SA.height drawing.height
    , SA.class_ (ClassName "drawing")
    ]
    ( [ SE.defs [] [ arrowhead ] ]
        <> map cluster drawing.clusters
        <> map (edge turnedEdges nodesByName drawing.directed) (Array.filter (\found -> not (hidden found.attributes)) drawing.edges)
        <> map node (Array.filter (\found -> not (hidden found.attributes)) drawing.nodes)
    )
  where
  nodesByName = Map.fromFoldable (map (\found -> Tuple found.name found) drawing.nodes)

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

-- | The outlines the page can draw. Graphviz has many more shapes; each
-- | is drawn as the nearest of these.
data Outline = Ellipse | DoubleEllipse | Rectangle | Diamond | NoOutline

outlineOf :: Attributes -> Outline
outlineOf attributes = case Attributes.lookup "shape" attributes of
  Just "box" -> Rectangle
  Just "rect" -> Rectangle
  Just "rectangle" -> Rectangle
  Just "square" -> Rectangle
  Just "Msquare" -> Rectangle
  Just "record" -> Rectangle
  Just "Mrecord" -> Rectangle
  Just "component" -> Rectangle
  Just "folder" -> Rectangle
  Just "tab" -> Rectangle
  Just "note" -> Rectangle
  Just "diamond" -> Diamond
  Just "Mdiamond" -> Diamond
  Just "doublecircle" -> DoubleEllipse
  Just "doubleoctagon" -> DoubleEllipse
  Just "plaintext" -> NoOutline
  Just "plain" -> NoOutline
  Just "none" -> NoOutline
  _ -> Ellipse

node :: forall w i. DrawnNode -> HH.HTML w i
node found =
  SE.g [ SA.class_ (ClassName "node") ]
    ( [ SE.title [] [ HH.text (nodeDescription found) ] ]
        <> outline (outlineOf found.attributes)
        <> labelText (ClassName "node-label") Label.lineHeight found.box.centre
          (fromMaybe ink (colourOf [ "fontcolor" ] found.attributes))
          (shownLabel found)
    )
  where
  box = found.box
  drawnStyles = styles found.attributes
  fillColour =
    if Set.member Filled drawnStyles then fromMaybe filledByDefault (colourOf [ "fillcolor", "color" ] found.attributes)
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

-- | One `text` element per line, centred as a block on the point.
labelText :: forall w i. ClassName -> Number -> Point -> Color -> String -> Array (HH.HTML w i)
labelText className spacing centre fillColour written =
  let
    lines = Label.lines written
    count = Array.length lines
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

edge :: forall w i. TurnedEdges -> Map NodeName DrawnNode -> Boolean -> DrawnEdge -> HH.HTML w i
edge turnedEdges nodesByName directed found =
  SE.g [ SA.class_ (ClassName (if picked then "edge edge--turned" else "edge")) ]
    ( [ SE.title [] [ HH.text description ]
      , SE.polyline
          ( [ SA.points (map (\point -> Tuple point.x point.y) trimmed) ]
              <> strokeProperties (styles found.attributes) strokeColour
              <> arrows
          )
      ]
        <> case Attributes.lookup "label" found.attributes, middle trimmed of
          Just written, Just centre -> labelText (ClassName "edge-label") (0.8 * Label.lineHeight) centre (if picked then accent else ink) written
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
  trimmed = trimEnds (Map.lookup found.tail nodesByName) (Map.lookup found.head nodesByName) found.path

-- | The layout's paths run from centre to centre. Cut each end back to
-- | where it crosses its node's outline, so that arrowheads touch the
-- | node instead of hiding under it. An end that does not start at its
-- | node's centre (a loop) is left alone.
trimEnds :: Maybe DrawnNode -> Maybe DrawnNode -> Array Point -> Array Point
trimEnds tail head path = case Array.uncons path, Array.unsnoc path of
  Just first, Just last | Array.length path >= 2 ->
    let
      start = case tail, Array.head first.tail of
        Just tailNode, Just next | atCentre tailNode first.head -> leaving tailNode next
        _, _ -> first.head
      end = case head, Array.last last.init of
        Just headNode, Just previous | atCentre headNode last.last -> leaving headNode previous
        _, _ -> last.last
    in
      [ start ] <> Array.slice 1 (Array.length path - 1) path <> [ end ]
  _, _ -> path
  where
  atCentre found point = abs (point.x - found.box.centre.x) < 0.5 && abs (point.y - found.box.centre.y) < 0.5

-- | Where a line from a node's centre towards a point leaves its outline.
leaving :: DrawnNode -> Point -> Point
leaving found towards =
  let
    box = found.box
    across = towards.x - box.centre.x
    down = towards.y - box.centre.y
    halfWidth = box.width / 2.0
    halfHeight = box.height / 2.0
    reach = case outlineOf found.attributes of
      Diamond -> 1.0 / (abs across / halfWidth + abs down / halfHeight)
      Rectangle -> boxReach box across down
      NoOutline -> boxReach box across down
      _ -> 1.0 / sqrt ((across / halfWidth) * (across / halfWidth) + (down / halfHeight) * (down / halfHeight))
  in
    if across == 0.0 && down == 0.0 || reach >= 1.0 then box.centre
    else { x: box.centre.x + across * reach, y: box.centre.y + down * reach }

boxReach :: Box -> Number -> Number -> Number
boxReach box across down =
  min (if across == 0.0 then infinity else box.width / 2.0 / abs across)
    (if down == 0.0 then infinity else box.height / 2.0 / abs down)
  where
  infinity = 1.0e300

-- | The middle of a path by its points: the middle point, or halfway
-- | along the middle piece.
middle :: Array Point -> Maybe Point
middle path =
  let
    count = Array.length path
    half = count / 2
  in
    if count == 0 then Nothing
    else if Int.odd count then Array.index path half
    else do
      before <- Array.index path (half - 1)
      after <- Array.index path half
      pure { x: (before.x + after.x) / 2.0, y: (before.y + after.y) / 2.0 }

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
          Just written -> labelText (ClassName "cluster-label") (0.85 * Label.lineHeight) { x: box.centre.x, y: top + 9.0 }
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
