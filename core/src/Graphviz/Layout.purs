-- | DOT text in, a drawing out.
-- |
-- | This is the front door: `layout` runs every pass and returns where
-- | things go, as plain data. It draws nothing itself; `Graphviz.Svg` turns
-- | a `Drawing` into SVG text, and a caller with its own renderer can use
-- | the `Drawing` directly.
-- |
-- | Units are points, 72 to the inch, as in Graphviz. The origin is the top
-- | left corner and y grows downwards.
-- |
-- | Ranks, the order within each rank and the place of everything across
-- | the page come from the real passes (`Graphviz.Layout.Rank`,
-- | `Graphviz.Layout.Order`, and the coordinates in this module).
-- |
-- | An edge is a smooth curve from its tail's outline to its head's,
-- | through the places the ordering kept for it in the ranks between. It
-- | is not yet routed round what lies beside those places, so a curve can
-- | clip a node or a cluster it passes.
module Graphviz.Layout
  ( Drawing
  , Point
  , Box
  , DrawnNode
  , DrawnEdge
  , DrawnCluster
  , Options
  , defaults
  , layout
  , layoutWith
  , draw
  , Outline(..)
  , outlineOf
  , leaving
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldl, maximum, minimum, sum)
import Data.Int (ceil, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (abs, sqrt)
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, ignoreCase)
import Data.List (List(..))
import Data.List as List
import Data.Tuple (Tuple(..))
import Graphviz.Attributes (Attributes)
import Graphviz.Attributes as Attributes
import Graphviz.Dot.Parser (parse)
import Graphviz.Dot.Syntax (Text(..), text)
import Graphviz.Graph (EdgeId, Graph, NodeName(..), SubgraphId, fromDot)
import Graphviz.Graph as Graph
import Graphviz.Layout.NetworkSimplex (Requirement, solve, value)
import Graphviz.Layout.Order (Link, Vertex(..), layered, order)
import Graphviz.Layout.Rank (Rank(..), describe, rank, rankOf, ranking, reversed)

type Point = { x :: Number, y :: Number }

-- | A rectangle, by its centre.
type Box = { centre :: Point, width :: Number, height :: Number }

-- | `label` is the text to show: the node's `label` attribute if it has
-- | one, otherwise its name.
type DrawnNode = { name :: NodeName, label :: String, box :: Box, attributes :: Attributes }

-- | `path` runs from the tail node's outline to the head node's, as the
-- | control points of cubic Bezier pieces joined end to end: the start,
-- | then three points for each piece (two to steer by and the point where
-- | the piece ends), which is how Graphviz writes an edge's `pos`.
-- | `labelAt` is the centre of the edge's label, if it has one. `turned` says the
-- | layout had to run this edge against the flow to break a cycle, so it
-- | points up the page; it is still drawn from tail to head.
type DrawnEdge =
  { id :: EdgeId
  , tail :: NodeName
  , head :: NodeName
  , path :: Array Point
  , labelAt :: Maybe Point
  , turned :: Boolean
  , attributes :: Attributes
  }

type DrawnCluster = { id :: SubgraphId, label :: Maybe String, box :: Box, attributes :: Attributes }

-- | `width`, `height` and every place in the drawing are in points at
-- | full size. `scale` is what the graph's `size` attribute asks the whole
-- | picture to be shown at, 1 when it asks nothing: Graphviz shrinks a
-- | drawing that is larger than `size` (in inches) to fit it, and with
-- | `size="w,h!"` enlarges a smaller one too. The layout leaves that to
-- | whoever draws, so that text is still measured at its real size.
-- |
-- | Clusters are in drawing order: an outer cluster comes before the ones
-- | inside it.
type Drawing =
  { width :: Number
  , height :: Number
  , scale :: Number
  , directed :: Boolean
  , nodes :: Array DrawnNode
  , edges :: Array DrawnEdge
  , clusters :: Array DrawnCluster
  }

-- | What the layout cannot know by itself.
-- |
-- |   - `measure`: how wide and high a piece of text is when drawn. That
-- |     depends on the font and on whatever draws it, so the caller says.
-- |   - `nodeSeparation`: the least gap between neighbours in a rank.
-- |   - `rankSeparation`: the gap between one rank and the next.
type Options =
  { measure :: String -> { width :: Number, height :: Number }
  , nodeSeparation :: Number
  , rankSeparation :: Number
  }

-- | Graphviz's own spacing, and a rough guess at text: 14 point type at a
-- | little under half its height per character, which is about what
-- | Graphviz's Times comes to, and a line for each `\n` or `\l`.
defaults :: Options
defaults =
  { measure: \written ->
      let
        lines = Array.concatMap (String.split (String.Pattern "\\l")) (String.split (String.Pattern "\\n") written)
      in
        { width: 6.5 * toNumber (foldl (\longest line -> max longest (String.length line)) 0 lines)
        , height: 17.0 * toNumber (Array.length lines)
        }
  , nodeSeparation: 18.0
  , rankSeparation: 36.0
  }

-- | Lay out the first graph in a DOT file.
layout :: String -> Either String Drawing
layout = layoutWith defaults

layoutWith :: Options -> String -> Either String Drawing
layoutWith options written = parse written >>= (fromDot >>> draw options)

-- | Lay out a graph already read.
draw :: Options -> Graph -> Either String Drawing
draw options graph = case rank graph of
  Left reason -> Left ("this graph cannot be laid out yet: " <> describe reason)
  Right solved ->
    let
      ranks = ranking solved
      arranged = order (layered graph (rankOf ranks))
    in
      Right (place options graph (\node -> case rankOf ranks node of
        Rank r -> r) (reversed solved) arranged.ranks arranged.links arranged.clusters)

-- The size of a record label: fields side by side, and each pair of
-- braces turns the fields inside it the other way.
recordSize :: (String -> { width :: Number, height :: Number }) -> Boolean -> String -> { width :: Number, height :: Number }
recordSize measure acrossFirst written = (fields acrossFirst (List.fromFoldable (toCharArray written))).size
  where
  fields across input =
    let
      first = field across input
    in
      case List.dropWhile (_ == ' ') first.rest of
        Cons '|' rest ->
          let
            others = fields across rest
          in
            { size: beside across first.size others.size, rest: others.rest }
        _ -> first

  field across input = case List.dropWhile (_ == ' ') input of
    Cons '{' rest ->
      let
        within = fields (not across) rest
      in
        { size: within.size
        , rest: case List.dropWhile (_ == ' ') within.rest of
            Cons '}' after -> after
            other -> other
        }
    other -> leaf other ""

  -- A field's text, without the name of its port.
  leaf input soFar = case input of
    Cons '\\' (Cons escaped rest) -> leaf rest (soFar <> "\\" <> singleton escaped)
    Cons '<' rest -> leaf (List.drop 1 (List.dropWhile (_ /= '>') rest)) soFar
    Cons '|' _ -> ended input soFar
    Cons '}' _ -> ended input soFar
    Nil -> ended input soFar
    Cons character rest -> leaf rest (soFar <> singleton character)
  ended input soFar =
    let
      needed = measure (String.trim soFar)
    in
      { size: { width: needed.width + 16.0, height: max 24.0 (needed.height + 8.0) }, rest: input }

  beside across one other =
    if across then { width: one.width + other.width, height: max one.height other.height }
    else { width: max one.width other.width, height: one.height + other.height }

-- The size of an HTML label, roughly: a table is as wide as its widest
-- row of cells and as high as its rows together, and anything else is its
-- text with the tags taken out.
markupSize :: (String -> { width :: Number, height :: Number }) -> String -> { width :: Number, height :: Number }
markupSize measure markup = case Array.drop 1 (pieces "<tr[^>]*>" markup) of
  [] -> measure (shown markup)
  rows ->
    let
      sizes = map row rows
    in
      { width: foldl (\widest size -> max widest size.width) 0.0 sizes, height: sum (map _.height sizes) }
  where
  row written =
    let
      cells = map (shown >>> measure) (Array.drop 1 (pieces "<td[^>]*>" written))
    in
      { width: sum (map (\cell -> cell.width + 12.0) cells), height: foldl (\tallest cell -> max tallest cell.height) 0.0 cells + 10.0 }
  pieces pattern written = case regex pattern ignoreCase of
    Right found -> Regex.split found written
    Left _ -> [ written ]
  swap pattern with written = case regex pattern (ignoreCase <> global) of
    Right found -> Regex.replace found with written
    Left _ -> written
  shown = swap "<br[^>]*>" "\\n" >>> swap "<[^>]*>" "" >>> String.trim

-- | The outlines an edge can end on. Graphviz has many more shapes; each
-- | is taken as the nearest of these.
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

-- | Where a line from a node's centre towards a point leaves its outline.
-- | A point inside the outline gives the centre.
leaving :: DrawnNode -> Point -> Point
leaving found towards =
  let
    box = found.box
    across = towards.x - box.centre.x
    down = towards.y - box.centre.y
    halfWidth = box.width / 2.0
    halfHeight = box.height / 2.0
    boxReach = min (if across == 0.0 then 1.0e300 else halfWidth / abs across) (if down == 0.0 then 1.0e300 else halfHeight / abs down)
    reach = case outlineOf found.attributes of
      Diamond -> 1.0 / (abs across / halfWidth + abs down / halfHeight)
      Rectangle -> boxReach
      NoOutline -> boxReach
      Ellipse -> 1.0 / sqrt ((across / halfWidth) * (across / halfWidth) + (down / halfHeight) * (down / halfHeight))
      DoubleEllipse -> 1.0 / sqrt ((across / halfWidth) * (across / halfWidth) + (down / halfHeight) * (down / halfHeight))
  in
    if across == 0.0 && down == 0.0 || reach >= 1.0 then box.centre
    else { x: box.centre.x + across * reach, y: box.centre.y + down * reach }

-- The last pass: each edge's run of points, node centre to bends to node
-- centre, becomes a curve between the two outlines. An edge from a node
-- to itself is a curve already.
curved :: Options -> Boolean -> Drawing -> Drawing
curved options sideways drawing = drawing { edges = map bend drawing.edges }
  where
  nodesByName = Map.fromFoldable (map (\node -> Tuple node.name node) drawing.nodes)

  bend found =
    if found.tail == found.head then case Map.lookup found.tail nodesByName of
      Just node -> loop node found
      Nothing -> found
    else found { path = smooth (trimmed found) }

  -- An edge from a node to itself: a loop on the node's right, or on top
  -- when the drawing runs left to right. A node's loops are put one
  -- outside the other, and the label goes just beyond its loop.
  loop node found =
    let
      earlier = Array.length (Array.takeWhile (\other -> other.id /= found.id) (Array.filter (\other -> other.tail == found.tail && other.head == found.tail) drawing.edges))
      reach = 18.0 + 22.0 * toNumber earlier
      box = node.box
      written = Attributes.lookup "label" found.attributes
    in
      if sideways then
        let
          top = box.centre.y - box.height / 2.0
          start = leaving node { x: box.centre.x - box.width / 4.0, y: box.centre.y - box.height }
          end = leaving node { x: box.centre.x + box.width / 4.0, y: box.centre.y - box.height }
        in
          found
            { path = [ start, { x: start.x - 6.0, y: top - reach * 1.3 }, { x: end.x + 6.0, y: top - reach * 1.3 }, end ]
            , labelAt = map (\text -> { x: box.centre.x, y: top - reach - (options.measure text).height / 2.0 }) written
            }
      else
        let
          right = box.centre.x + box.width / 2.0
          start = leaving node { x: box.centre.x + box.width, y: box.centre.y - box.height / 2.0 }
          end = leaving node { x: box.centre.x + box.width, y: box.centre.y + box.height / 2.0 }
        in
          found
            { path = [ start, { x: right + reach * 1.3, y: start.y - 6.0 }, { x: right + reach * 1.3, y: end.y + 6.0 }, end ]
            , labelAt = map (\text -> { x: right + reach + 4.0 + (options.measure text).width / 2.0, y: box.centre.y }) written
            }

  -- The ends are cut back to the outlines. A point of the way that lies
  -- inside the node at either end is dropped first, so that the edge
  -- leaves towards the first place it is going that is outside.
  trimmed found = case Array.head found.path, Array.last found.path, Map.lookup found.tail nodesByName, Map.lookup found.head nodesByName of
    Just first, Just last, Just tail, Just head | Array.length found.path >= 2 ->
      let
        between = Array.slice 1 (Array.length found.path - 1) found.path
        clear = Array.reverse (Array.dropWhile (covers head) (Array.reverse (Array.dropWhile (covers tail) between)))
        start = case Array.head clear of
          Just next -> leaving tail next
          Nothing -> leaving tail last
        end = case Array.last clear of
          Just previous -> leaving head previous
          Nothing -> leaving head first
      in
        [ start ] <> clear <> [ end ]
    _, _, _, _ -> found.path

  covers node point = leaving node point == node.box.centre

-- The shortest way from one point to another that crosses each gate, a
-- level stretch of the page, in turn: a string pulled tight. Each
-- crossing is moved in turn to where a straight line between the ones
-- either side would cross, or as near as its gate allows, until they
-- settle. Only the corners are returned, where the string is held off
-- the straight by the end of a gate.
taut :: Point -> Array { y :: Number, low :: Number, high :: Number } -> Point -> Array Point
taut start gates end = [ start ] <> corners <> [ end ]
  where
  count = Array.length gates
  first = map (\gate -> max gate.low (min gate.high start.x)) gates
  settled = foldl (\crossings _ -> sweep crossings) first (Array.range 1 60)

  pointAt crossings index =
    if index < 0 then start
    else if index >= count then end
    else case Array.index crossings index, Array.index gates index of
      Just x, Just gate -> { x, y: gate.y }
      _, _ -> end

  -- Where the straight line between two points is at a height.
  straight before after y =
    if after.y == before.y then before.x
    else before.x + (after.x - before.x) * (y - before.y) / (after.y - before.y)

  -- One pass: each crossing uses the new place of the one before it and
  -- the old place of the one after.
  sweep crossings = foldl
    ( \moved index -> case Array.index gates index of
        Just gate ->
          Array.snoc moved
            (max gate.low (min gate.high (straight (if index == 0 then start else pointAt moved (index - 1)) (pointAt crossings (index + 1)) gate.y)))
        Nothing -> moved
    )
    []
    (Array.range 0 (count - 1))

  corners = Array.mapMaybe
    ( \index ->
        let
          here = pointAt settled index
          before = pointAt settled (index - 1)
          after = pointAt settled (index + 1)
        in
          if abs (here.x - straight before after here.y) > 0.5 then Just here else Nothing
    )
    (Array.range 0 (count - 1))

-- A curve through every point given. At each point it heads the way from
-- the point before to the point after, and each piece steers by a third
-- of its own length and no more than half an inch, so that a long piece
-- runs nearly straight and turns near its ends, and does not swing wide
-- of the room it was given. A run of points in a line gives a straight
-- line.
smooth :: Array Point -> Array Point
smooth points = case Array.head points of
  Nothing -> []
  Just start -> [ start ] <> Array.concat (Array.zipWith piece (Array.range 0 (count - 2)) (Array.drop 1 points))
  where
  count = Array.length points
  at index = case Array.index points (max 0 (min (count - 1) index)) of
    Just found -> found
    Nothing -> { x: 0.0, y: 0.0 }
  heading index =
    let
      across = (at (index + 1)).x - (at (index - 1)).x
      down = (at (index + 1)).y - (at (index - 1)).y
      length = sqrt (across * across + down * down)
    in
      if length == 0.0 then { x: 0.0, y: 0.0 } else { x: across / length, y: down / length }
  piece index end =
    if count < 2 then []
    else
      let
        from = at index
        third = min 36.0 (sqrt ((end.x - from.x) * (end.x - from.x) + (end.y - from.y) * (end.y - from.y)) / 3.0)
      in
        [ { x: from.x + (heading index).x * third, y: from.y + (heading index).y * third }
        , { x: end.x - (heading (index + 1)).x * third, y: end.y - (heading (index + 1)).y * third }
        , end
        ]

-- What the optimisation across the page chooses a number for.
data Across
  = Centre Vertex
  | Pull Int Int
  | LeftSide SubgraphId
  | RightSide SubgraphId

derive instance eqAcross :: Eq Across
derive instance ordAcross :: Ord Across

-- The geometry.
place
  :: Options
  -> Graph
  -> (NodeName -> Int)
  -> Array EdgeId
  -> Array (Array Vertex)
  -> Array (Array Link)
  -> Map Vertex (Array SubgraphId)
  -> Drawing
place options graph rankNumber turned ranks links inClusters =
  fitted $ curved options sideways $ turnedRound
    { width: extent.width
    , height: extent.height
    , scale: 1.0
    , directed: Graph.isDirected graph
    , nodes: map (\node -> { name: node.name, label: labelOf node, box: boxOf (Node node.name), attributes: node.attributes }) (Graph.nodes graph)
    , edges: map edge (Graph.edges graph)
    , clusters: Array.sortWith (\found -> negate (found.box.width * found.box.height)) (Array.mapMaybe cluster (Graph.clusters graph))
    }
  where
  -- Room round the outside, enough for the boxes of nested clusters.
  margin = 8.0 + 8.0 * toNumber (foldl (\deepest path -> max deepest (Array.length path)) 0 inClusters)
  nodesByName = Map.fromFoldable (map (\node -> Tuple node.name node) (Graph.nodes graph))

  labelOf node = case Attributes.lookup "label" node.attributes of
    Just "\\N" -> nameOf node.name
    Just given -> given
    Nothing -> nameOf node.name
  nameOf (NodeName written) = text written

  -- `size`: one number of inches for both ways or two with a comma, and
  -- an exclamation mark to say the drawing should be enlarged to it too.
  fitted drawing = case Attributes.lookup "size" (Graph.attributes graph) of
    Nothing -> drawing
    Just written ->
      let
        enlarge = String.contains (String.Pattern "!") written
        asked = Array.mapMaybe (String.trim >>> Number.fromString) (String.split (String.Pattern ",") (String.replaceAll (String.Pattern "!") (String.Replacement "") written))
        fit = case asked of
          [ both ] -> Just (min (both * 72.0 / drawing.width) (both * 72.0 / drawing.height))
          [ wide, high ] -> Just (min (wide * 72.0 / drawing.width) (high * 72.0 / drawing.height))
          _ -> Nothing
      in
        case fit of
          Just factor | factor > 0.0 && (factor < 1.0 || enlarge) -> drawing { scale = factor }
          _ -> drawing

  -- The drawing is made top to bottom and turned afterwards if `rankdir`
  -- asks, so until then a size is held the way it will lie once turned.
  sideways = case Attributes.lookup "rankdir" (Graph.attributes graph) of
    Just "LR" -> true
    Just "RL" -> true
    _ -> false
  lying size = if sideways then { width: size.height, height: size.width } else size

  -- Labelled edges, by the bend that makes room for their labels: the one
  -- in the middle rank of the edge. (When any edge has a label the
  -- ranking puts a rank between every two, so each labelled edge has one.)
  -- Edges between the same two nodes share a bend and their labels are
  -- stacked beside it.
  carried :: Map Vertex (Array { id :: EdgeId, size :: { width :: Number, height :: Number } })
  carried = foldl carry Map.empty (Graph.edges graph)
    where
    carry soFar found = case Attributes.lookup "label" found.attributes of
      Just written | abs (toNumber (rankNumber found.tail - rankNumber found.head)) >= 2.0 ->
        Map.insertWith (flip append) (labelBend found) [ { id: found.id, size: inType found.attributes (options.measure written) } ] soFar
      _ -> soFar
  labelBend found =
    let
      from = rankNumber found.tail
      to = rankNumber found.head
    in
      Bend (if from <= to then found.tail else found.head) (if from <= to then found.head else found.tail) ((from + to) / 2)

  -- How much of the rank a label takes up, across the page as it is being
  -- made: its width and a gap, or its height if the drawing is to be
  -- turned on its side.
  along label = if sideways then label.size.height else label.size.width + 6.0
  -- Each labelled edge of a bend runs down the left of its own label, so
  -- it is moved right past the labels before it. This is how far, for an
  -- edge that has a label there.
  pastLabels found = case Map.lookup (labelBend found) carried of
    Just labels | Array.any (\label -> label.id == found.id) labels ->
      Just (sum (map along (Array.takeWhile (\label -> label.id /= found.id) labels)))
    _ -> Nothing
  -- The room that takes, kept beside every bend of those edges and not
  -- only the one with the labels.
  labelRoom :: Map (Tuple NodeName NodeName) Number
  labelRoom = Map.fromFoldable
    ( Array.mapMaybe
        ( \(Tuple bend labels) -> case bend of
            Bend upper lower _ -> Just (Tuple (Tuple upper lower) (sum (map along labels)))
            Node _ -> Nothing
        )
        (Map.toUnfoldable carried)
    )

  loopsOn name = Array.filter (\found -> found.tail == name && found.head == name) (Graph.edges graph)

  -- A node is as big as Graphviz's default (three quarters of an inch by
  -- half an inch) or as its label needs, whichever is more. A bend takes
  -- up no room unless it carries labels.
  sizeOf = case _ of
    Bend upper lower at -> case Map.lookup (Bend upper lower at) carried of
      Just labels ->
        { width: sum (map along labels)
        , height: foldl (\most label -> max most (if sideways then label.size.width else label.size.height)) 0.0 labels
        }
      Nothing -> { width: 0.0, height: 0.0 }
    Node name -> case Map.lookup name nodesByName of
      Nothing -> lying { width: 54.0, height: 36.0 }
      Just node ->
        let
          needed = labelSize node
        in
          lying (shaped node.attributes
            { width: max (inches "width" 54.0 node.attributes) (needed.width + 16.0)
            , height: max (inches "height" 36.0 node.attributes) (needed.height + 8.0)
            })

  -- Text is measured at Graphviz's 14 points; `fontsize` says otherwise.
  inType attributes size = case Attributes.lookup "fontsize" attributes >>= Number.fromString of
    Just points -> { width: size.width * points / 14.0, height: size.height * points / 14.0 }
    Nothing -> size

  -- The room a node's label needs. A record is a row of fields, and an
  -- HTML label a table of cells, so their written text is far longer
  -- than what is shown.
  labelSize node = if labelOf node == "" then { width: 0.0, height: 0.0 } else inType node.attributes case Attributes.lookupText "label" node.attributes of
    Just (Html markup) -> markupSize options.measure markup
    _ -> case Attributes.lookup "shape" node.attributes of
      Just "record" -> recordSize options.measure (not sideways) (labelOf node)
      Just "Mrecord" -> recordSize options.measure (not sideways) (labelOf node)
      _ -> options.measure (labelOf node)

  -- `width` and `height` are the least a node may be, in inches.
  inches key otherwise attributes = case Attributes.lookup key attributes >>= Number.fromString of
    Just given -> given * 72.0
    Nothing -> otherwise
  -- A circle is as wide as it is high, and a double one has a second
  -- outline four points outside the first.
  shaped attributes size = case Attributes.lookup "shape" attributes of
    -- A point is a dot a twentieth of an inch across, whatever its label.
    Just "point" -> { width: inches "width" 3.6 attributes, height: inches "width" 3.6 attributes }
    Just "circle" -> { width: max size.width size.height, height: max size.width size.height }
    Just "Mcircle" -> { width: max size.width size.height, height: max size.width size.height }
    Just "doublecircle" -> { width: max size.width size.height + 8.0, height: max size.width size.height + 8.0 }
    _ -> size

  -- Edges that join the same two nodes, either way round, would lie on
  -- one line. They are spread out instead: this is how many such edges
  -- there are for an edge, and which of them it is.
  together :: Map (Tuple NodeName NodeName) (Array EdgeId)
  together = foldl
    (\soFar found -> if found.tail == found.head then soFar else Map.insertWith (flip append) (pairOf found) [ found.id ] soFar)
    Map.empty
    (Graph.edges graph)
  pairOf found = Tuple (min found.tail found.head) (max found.tail found.head)
  placeAmong found = case Map.lookup (pairOf found) together of
    Just ids -> { count: Array.length ids, index: Array.length (Array.takeWhile (_ /= found.id) ids) }
    Nothing -> { count: 1, index: 0 }
  spread = 10
  sharing upper lower = case Map.lookup (Tuple (min upper lower) (max upper lower)) together of
    Just ids -> Array.length ids
    Nothing -> 1

  -- How far a vertex reaches each side of its centre. A node is centred,
  -- with room on one side for edges from it to itself. A bend with labels
  -- has them all on its right, so that the edge runs down their left.
  loopRoom name = sum
    ( map
        ( \found -> 22 + case Attributes.lookup "label" found.attributes of
            Just written | not sideways -> ceil (options.measure written).width
            _ -> 0
        )
        (loopsOn name)
    )
  leftHalf = case _ of
    Node name -> ceil ((sizeOf (Node name)).width / 2.0) + (if sideways then loopRoom name else 0)
    -- Edges sharing a bend are spread out to its left.
    Bend upper lower _ -> spread * (sharing upper lower - 1)
  rightHalf = case _ of
    Node name -> ceil ((sizeOf (Node name)).width / 2.0) + (if sideways then 0 else loopRoom name)
    Bend upper lower _ -> case Map.lookup (Tuple upper lower) labelRoom of
      Just kept -> ceil kept
      Nothing -> 0

  -- Across the page. Every vertex gets a whole number of points for its
  -- centre, chosen by the same optimisation that chose the ranks (section
  -- 4.2 of the paper):
  --
  --   - neighbours in a rank stay apart by half their widths and the node
  --     separation, and keep their order;
  --   - each edge between two ranks pulls its ends towards being one above
  --     the other, harder between two bends than between two nodes, so
  --     that long edges run straight;
  --   - each cluster has a left and a right side, which stay outside
  --     everything in it and outside its neighbours, and pull together.
  separation = ceil (inches "nodesep" options.nodeSeparation (Graph.attributes graph))
  room = 8
  pathOf vertex = case Map.lookup vertex inClusters of
    Just path -> path
    Nothing -> []

  requirements :: Array (Requirement Across)
  requirements = Array.concat
    [ Array.concatMap (\vertices -> Array.zipWith neighbours vertices (Array.drop 1 vertices)) ranks
    , Array.concat (Array.mapWithIndex (\upper -> Array.concat <<< Array.mapWithIndex (pull upper)) links)
    , Array.concatMap within (Map.toUnfoldable inClusters)
    , Array.concatMap nested (Set.toUnfoldable nestings)
    , map (\inner -> { tail: LeftSide inner, head: RightSide inner, weight: 4, minimum: 0 }) (Set.toUnfoldable (Set.fromFoldable (Array.concat (Array.fromFoldable (Map.values inClusters)))))
    ]

  -- Where the two part company in the nesting of clusters decides whose
  -- sides have to stay apart.
  neighbours left right =
    let
      shared = Array.length (Array.takeWhile identity (Array.zipWith eq (pathOf left) (pathOf right)))
    in
      case Array.index (pathOf left) shared, Array.index (pathOf right) shared of
        Just from, Just entering -> { tail: RightSide from, head: LeftSide entering, weight: 0, minimum: separation }
        Just from, Nothing -> { tail: RightSide from, head: Centre right, weight: 0, minimum: leftHalf right + separation }
        Nothing, Just entering -> { tail: Centre left, head: LeftSide entering, weight: 0, minimum: rightHalf left + separation }
        Nothing, Nothing -> { tail: Centre left, head: Centre right, weight: 0, minimum: rightHalf left + leftHalf right + separation }

  -- A variable that must be left of both ends: the cost of its two
  -- requirements together is how far apart the ends are.
  pull upper index link =
    let
      weight = stiffness link * max 1 link.count
    in
      [ { tail: Pull upper index, head: Centre link.upper, weight, minimum: 0 }
      , { tail: Pull upper index, head: Centre link.lower, weight, minimum: 0 }
      ]
  stiffness link = case link.upper, link.lower of
    Bend _ _ _, Bend _ _ _ -> 8
    Node _, Node _ -> 1
    _, _ -> 2

  within (Tuple vertex path) = case Array.last path of
    Just innermost ->
      [ { tail: LeftSide innermost, head: Centre vertex, weight: 0, minimum: leftHalf vertex + room }
      , { tail: Centre vertex, head: RightSide innermost, weight: 0, minimum: rightHalf vertex + room }
      ]
    Nothing -> []
  nestings = Set.fromFoldable (Array.concatMap (\path -> Array.zip path (Array.drop 1 path)) (Array.fromFoldable (Map.values inClusters)))
  nested (Tuple outer inner) =
    [ { tail: LeftSide outer, head: LeftSide inner, weight: 0, minimum: room }
    , { tail: RightSide inner, head: RightSide outer, weight: 0, minimum: room }
    ]

  across = (solve requirements).assignment
  -- Everything is moved so that the leftmost thing sits at the margin.
  leftmost = foldl min 0
    ( map (\vertex -> value across (Centre vertex) - leftHalf vertex) (Array.concat ranks)
        <> map (\inner -> value across (LeftSide inner)) (Array.concat (Array.fromFoldable (Map.values inClusters)))
    )
  rightmost = foldl max 0
    ( map (\vertex -> value across (Centre vertex) + rightHalf vertex) (Array.concat ranks)
        <> map (\inner -> value across (RightSide inner)) (Array.concat (Array.fromFoldable (Map.values inClusters)))
    )
  xOf variable = margin + toNumber (value across variable - leftmost)

  -- Down the page: ranks are stacked with the rank separation between
  -- them, each as tall as its tallest node. When edges have labels there
  -- are twice the ranks, at half the separation.
  rankGap = if any (_.attributes >>> Attributes.hasLabel) (Graph.edges graph) then rankSeparation / 2.0 else rankSeparation
  rankSeparation = inches "ranksep" options.rankSeparation (Graph.attributes graph)
  stacked = foldl
    ( \state vertices ->
        let
          height = foldl (\tallest vertex -> max tallest (sizeOf vertex).height) 0.0 vertices
        in
          { top: state.top + height + rankGap
          , found: Array.snoc state.found { vertices, top: state.top, height }
          }
    )
    { top: margin, found: [] }
    ranks
  boxes :: Map Vertex Box
  boxes = Map.fromFoldable
    ( Array.concatMap
        ( \row -> map
            ( \vertex -> Tuple vertex
                { centre: { x: xOf (Centre vertex), y: row.top + row.height / 2.0 }
                , width: (sizeOf vertex).width
                , height: (sizeOf vertex).height
                }
            )
            row.vertices
        )
        stacked.found
    )
  boxOf vertex = case Map.lookup vertex boxes of
    Just found -> found
    Nothing -> { centre: { x: 0.0, y: 0.0 }, width: 0.0, height: 0.0 }
  extent =
    { width: toNumber (rightmost - leftmost) + 2.0 * margin
    , height: max (2.0 * margin) (stacked.top - rankGap + margin)
    }

  -- The room an edge has where it passes through a rank: from the top of
  -- the rank to the bottom, between whatever is on either side of its
  -- bend there. Beside a node it may come within eight points; beside
  -- another edge's bend, half way.
  lanes :: Map Vertex { top :: Number, bottom :: Number, low :: Number, high :: Number }
  lanes = Map.fromFoldable (Array.concatMap (\row -> Array.mapWithIndex (lane row) row.vertices) stacked.found)
  lane row index vertex =
    let
      here = (boxOf vertex).centre.x
      limit side beside = case beside of
        Nothing -> side * 1.0e9
        Just (Node name) -> (boxOf (Node name)).centre.x - side * ((boxOf (Node name)).width / 2.0 + 8.0)
        Just bend -> ((boxOf bend).centre.x + here) / 2.0
      -- Labels beside the edge here come with it, so it stops short of
      -- its right-hand neighbour by their width.
      kept = case vertex of
        Bend upper lower _ -> case Map.lookup (Tuple upper lower) labelRoom of
          Just beside -> beside
          Nothing -> 0.0
        Node _ -> 0.0
    in
      Tuple vertex
        { top: row.top
        , bottom: row.top + row.height
        , low: min here (limit (-1.0) (Array.index row.vertices (index - 1)))
        , high: max here (limit 1.0 (Array.index row.vertices (index + 1)) - kept)
        }
  gatesAt goingDown vertex = case Map.lookup vertex lanes of
    Nothing -> []
    Just found ->
      if found.top == found.bottom then [ { y: found.top, low: found.low, high: found.high } ]
      else if goingDown then [ { y: found.top, low: found.low, high: found.high }, { y: found.bottom, low: found.low, high: found.high } ]
      else [ { y: found.bottom, low: found.low, high: found.high }, { y: found.top, low: found.low, high: found.high } ]

  -- An edge leaves its own rank, and comes into the rank of its other
  -- end, between that node's neighbours, so that it cannot cut through
  -- one of them on a long slant. `below` says the edge is on the side of
  -- the node that is further down the page.
  doorOf below vertex = case Map.lookup vertex lanes of
    Nothing -> []
    Just found -> [ { y: if below then found.bottom else found.top, low: found.low, high: found.high } ]

  -- An edge goes from its tail to its head through the bends between
  -- them. An edge from a node to itself is a small loop on its right.
  edge found =
    { id: found.id
    , tail: found.tail
    , head: found.head
    , turned: Array.elem found.id turned
    , attributes: found.attributes
    , labelAt:
        if found.tail == found.head || not (Attributes.hasLabel found.attributes) then Nothing
        else case Map.lookup (labelBend found) carried of
          Just labels ->
            let
              bend = boxOf (labelBend found)
              mine = sum (map along (Array.filter (\label -> label.id == found.id) labels))
              deep = sum (map (\label -> if sideways then label.size.width else label.size.height) (Array.filter (\label -> label.id == found.id) labels))
              route = routeOf found
              -- Where the edge is as it passes the middle of that rank.
              passing = Array.findMap
                ( \(Tuple before after) ->
                    if (before.y - bend.centre.y) * (after.y - bend.centre.y) > 0.0 then Nothing
                    else if after.y == before.y then Just { x: before.x, lean: 0.0 }
                    else Just
                      { x: before.x + (after.x - before.x) * (bend.centre.y - before.y) / (after.y - before.y)
                      , lean: abs ((after.x - before.x) / (after.y - before.y))
                      }
                )
                (Array.zip route (Array.drop 1 route))
            in
              Just
                -- An edge that leans is further over at the label's top or
                -- bottom than at its middle, so the label stands off by
                -- that much more.
                { x: mine / 2.0 + case passing of
                    Just at -> at.x + min 24.0 (at.lean * deep / 2.0)
                    Nothing -> bend.centre.x
                , y: bend.centre.y
                }
          Nothing ->
            let
              from = (boxOf (Node found.tail)).centre
              to = (boxOf (Node found.head)).centre
            in
              Just { x: (from.x + to.x) / 2.0, y: (from.y + to.y) / 2.0 - 8.0 }
    , path: routeOf found
    }

  -- The way an edge goes, before it is made into a curve.
  routeOf found =
        if found.tail == found.head then []
        else
          let
            from = rankNumber found.tail
            to = rankNumber found.head
            upper = if from <= to then found.tail else found.head
            lower = if from <= to then found.head else found.tail
            passed = if max from to - min from to > 1 then Array.range (min from to + 1) (max from to - 1) else []
            downwards = [ Node upper ] <> map (Bend upper lower) passed <> [ Node lower ]
            between = if from == to then [ Node found.tail, Node found.head ] else if from < to then downwards else Array.reverse downwards
            among = placeAmong found
            centres = map (boxOf >>> _.centre) between
            ends = Array.length centres - 1
            tailBox = boxOf (Node found.tail)
            headBox = boxOf (Node found.head)
            -- An edge along a rank with a node standing between its ends
            -- goes round by the gap beside the rank: above it, or below
            -- for the first rank, which has nothing above.
            blocked = from == to && Array.any
              ( \node ->
                  let
                    box = boxOf (Node node.name)
                  in
                    box.centre.y == tailBox.centre.y && box.centre.x > min tailBox.centre.x headBox.centre.x && box.centre.x < max tailBox.centre.x headBox.centre.x
              )
              (Graph.nodes graph)
            share = rankGap * toNumber (among.index + 1) / toNumber (among.count + 1)
            level = case Map.lookup (Node found.tail) lanes of
              Just gate -> if from == 0 then gate.bottom + share else gate.top - share
              Nothing -> tailBox.centre.y
          in
            if blocked then
              [ tailBox.centre, { x: tailBox.centre.x, y: level }, { x: headBox.centre.x, y: level }, headBox.centre ]
            else if among.count < 2 then
              case Array.head centres, Array.last centres of
                Just first, Just last | from /= to ->
                  taut first
                    ( doorOf (from < to) (Node found.tail)
                        <> Array.concatMap (gatesAt (from < to)) (Array.slice 1 ends between)
                        <> doorOf (from > to) (Node found.head)
                    )
                    last
                _, _ -> centres
            else if ends < 2 then
              -- No bend to move: bow out through a point half way. Edges
              -- along a rank share the gap to the next rank between them.
              case Array.head centres, Array.last centres of
                Just first, Just last ->
                  [ first
                  , { x: (first.x + last.x) / 2.0 + (if from == to then 0.0 else 14.0 * (toNumber among.index - toNumber (among.count - 1) / 2.0))
                    , y: (first.y + last.y) / 2.0 + (if from == to then min 14.0 (rankGap / toNumber among.count) * (toNumber among.index - toNumber (among.count - 1) / 2.0) else 0.0)
                    }
                  , last
                  ]
                _, _ -> centres
            else Array.mapWithIndex
              ( \index point ->
                  if index == 0 || index == ends then point
                  else case pastLabels found of
                    Just past -> point { x = point.x + past }
                    Nothing -> point { x = point.x - toNumber (spread * (among.count - 1 - among.index)) }
              )
              centres

  -- A cluster's box goes round everything in it, with a little room, and
  -- more room the more clusters it has inside it.
  cluster subgraph =
    let
      inside = Array.mapMaybe (\(Tuple vertex path) -> if Array.elem subgraph.id path then Just { box: boxOf vertex, depth: Array.length path } else Nothing)
        (Map.toUnfoldable inClusters)
      own = Array.length (Array.takeWhile (_ /= subgraph.id) (case Array.find (\(Tuple _ path) -> Array.elem subgraph.id path) (Map.toUnfoldable inClusters :: Array (Tuple Vertex (Array SubgraphId))) of
        Just (Tuple _ path) -> path
        Nothing -> [])) + 1
      padding item = 8.0 * toNumber (item.depth - own + 1)
    in
      do
        let left = xOf (LeftSide subgraph.id)
        let right = xOf (RightSide subgraph.id)
        top <- minimum (map (\item -> item.box.centre.y - item.box.height / 2.0 - padding item) inside)
        bottom <- maximum (map (\item -> item.box.centre.y + item.box.height / 2.0 + padding item) inside)
        pure
          { id: subgraph.id
          , label: Attributes.lookup "label" subgraph.attributes
          , attributes: subgraph.attributes
          , box: { centre: { x: (left + right) / 2.0, y: (top + bottom) / 2.0 }, width: right - left, height: bottom - top }
          }

  -- `rankdir`: the drawing is made top to bottom and then turned.
  turnedRound drawing = case Attributes.lookup "rankdir" (Graph.attributes graph) of
    Just "BT" -> mapPoints (\point -> point { y = drawing.height - point.y }) identity drawing
    Just "LR" -> onItsSide (mapPoints (\point -> { x: point.y, y: point.x }) swap drawing)
    Just "RL" -> onItsSide (mapPoints (\point -> { x: drawing.height - point.y, y: point.x }) swap drawing)
    _ -> drawing
  onItsSide drawing = drawing { width = drawing.height, height = drawing.width }
  swap box = box { width = box.height, height = box.width }
  mapPoints move reshape drawing = drawing
    { nodes = map (\node -> node { box = reshape (moveBox move node.box) }) drawing.nodes
    , edges = map (\found -> found { path = map move found.path, labelAt = map move found.labelAt }) drawing.edges
    , clusters = map (\found -> found { box = reshape (moveBox move found.box) }) drawing.clusters
    }
  moveBox move box = box { centre = move box.centre }
