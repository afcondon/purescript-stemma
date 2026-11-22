-- | Stemma as a layout engine of `hylograph-flow-render`.
-- |
-- | `stemma metrics` is an `Engine`: given a picture and a flow's
-- | projection it makes the renderer's `Layout`. Every node is a box, an
-- | open group is a cluster, and an edge is the curve Stemma routes from
-- | one box to the other. The flow runs down the page.
-- |
-- | HOW. The projection becomes a DOT syntax tree (not text: nothing is
-- | printed and parsed back), Stemma lays that out, and the drawing is read
-- | back by the names given on the way in: node `n3` is the projection's
-- | node 3, and a cluster carries the number of its group in an attribute.
-- |
-- | WHAT IT PROMISES. `LayersAligned`: a node's `layer` is its rank, and
-- | the nodes of a rank are centred on one line.
-- |
-- | WHAT IT DOES NOT PROMISE, AND WHY. `FlowsForward` holds of every
-- | layout with no group open (the tests hold it to that): Stemma turns an
-- | edge back only to break a cycle, and an edge from a node to itself is
-- | a loop marked `Backward`. With a group open it can fail where the
-- | projection has no cycle. Stemma ranks a cluster as Graphviz does, on
-- | its own and then as one rigid block, so an edge across a cluster's
-- | edge can be left no rank to point down into: `f -> g` in a group with
-- | `f -> m -> g` outside it puts `m` beside `g`, in Graphviz too. Such an
-- | edge is marked `Backward` where it points up the page and is `Forward`
-- | where it runs along a rank. A promise is of every layout, so this one
-- | is not made.
-- |
-- | WHAT IT DOES NOT DO. It places no ports: an edge meets its node's box
-- | wherever the curve arrives. It knows nothing of an edge's width.
-- |
-- | Units: Stemma works in points and the renderer in pixels; one is taken
-- | for the other.
module Stemma.Flow
  ( Metrics
  , defaultMetrics
  , stemma
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))
import Hylograph.Flow (ElementKey, GroupId, Node, Projection)
import Hylograph.Flow.Render.Engine (Box, Course(..), Direction(..), Engine(..), Layout, Picture, PlacedEdge, PlacedNode, PlacedOutline, Point, Promise(..), openPaths)
import Hylograph.Flow.Render.Route (Curve, Route(..))
import Hylograph.Flow.Render.Text (SetWords, estimateWidth, heightOf, setWords)
import Stemma.Attributes as Attributes
import Stemma.Dot.Syntax (Endpoint(..), GraphKind(..), Statement(..), Subgraph(..), Text(..))
import Stemma.Graph (NodeName(..), fromDot)
import Stemma.Layout as Stemma

-- | `nodeBreadth`: how wide every node is. `nodeHeight`: how tall a node
-- | is at least; one with words is as tall as its words need where that is
-- | more. `nodeSeparation`: the least gap between neighbours in a rank.
-- | `rankSeparation`: the gap between one rank and the next.
type Metrics =
  { nodeBreadth :: Number
  , nodeHeight :: Number
  , nodeSeparation :: Number
  , rankSeparation :: Number
  }

defaultMetrics :: Metrics
defaultMetrics = { nodeBreadth: 120.0, nodeHeight: 28.0, nodeSeparation: 18.0, rankSeparation: 36.0 }

stemma :: forall wire. Metrics -> Engine wire
stemma metrics = Engine
  { name: "stemma"
  , promises: [ LayersAligned ]
  , layout: layoutOf metrics
  }

layoutOf :: forall wire. Metrics -> Picture -> Projection wire -> Layout wire
layoutOf metrics picture projection = case Stemma.draw options (fromDot dot) of
  Right drawing -> fromDrawing drawing
  Left _ -> stackedUp
  where
  -- ---- what is known of each node before Stemma is asked ------------------
  home = picture.groups <#> openPaths

  pathOf :: ElementKey -> Array GroupId
  pathOf key = case home of
    Just found -> found.pathOf key
    Nothing -> []

  given :: Array { index :: Int, node :: Node, words :: Maybe SetWords, height :: Number, path :: Array GroupId }
  given = projection.nodes # Array.mapWithIndex \index node ->
    let
      words = picture.words <#> \say -> setWords picture.lettering metrics.nodeBreadth (say node)
    in
      { index
      , node
      , words
      , height: max metrics.nodeHeight (fromMaybe 0.0 (words <#> heightOf picture.lettering))
      , path: pathOf node.key
      }

  indexOf :: Map ElementKey Int
  indexOf = Map.fromFoldable (given <#> \found -> Tuple found.node.key found.index)

  -- the open groups that hold a node, each with a number
  groups :: Array GroupId
  groups = Array.nub (Array.concatMap _.path given)

  numberOf :: GroupId -> Int
  numberOf group = fromMaybe 0 (Array.elemIndex group groups)

  -- ---- the projection as DOT ----------------------------------------------
  nameOf :: Int -> Text
  nameOf index = Plain ("n" <> show index)

  attribute key value = { key, value: Plain value }

  inches pixels = toString (pixels / 72.0)

  nodeStatement found = NodeStatement { name: nameOf found.index, port: Nothing }
    [ attribute "shape" "box"
    , attribute "label" ""
    , attribute "width" (inches metrics.nodeBreadth)
    , attribute "height" (inches found.height)
    ]

  -- the nodes and groups directly inside the groups of `prefix`
  statementsIn :: Array GroupId -> Array Statement
  statementsIn prefix =
    let
      depth = Array.length prefix
      under = Array.filter (\found -> Array.take depth found.path == prefix) given
      own = Array.filter (\found -> Array.length found.path == depth) under
      inner = Array.nub (Array.mapMaybe (\found -> Array.index found.path depth) under)
    in
      map nodeStatement own <> map (clusterStatement prefix) inner

  clusterStatement prefix group = SubgraphStatement $ Subgraph
    { name: Just (Plain ("cluster_" <> show (numberOf group)))
    , statements:
        [ GraphAttribute (attribute "label" (fromMaybe "" (home <#> \found -> found.nameOf group)))
        , GraphAttribute (attribute groupKey (show (numberOf group)))
        ]
          <> statementsIn (Array.snoc prefix group)
    }

  groupKey = "flowgroup"

  -- each edge of the projection, with the numbers of its two nodes where
  -- both are drawn
  ends :: Array { index :: Int, from :: Maybe Int, to :: Maybe Int }
  ends = projection.edges # Array.mapWithIndex \index edge ->
    { index, from: Map.lookup edge.from indexOf, to: Map.lookup edge.to indexOf }

  -- the edges Stemma is given, in the order it is given them
  asked :: Array { index :: Int, from :: Int, to :: Int }
  asked = ends # Array.mapMaybe \found -> case found.from, found.to of
    Just from, Just to -> Just { index: found.index, from, to }
    _, _ -> Nothing

  edgeStatement found = EdgeStatement
    [ NodeEndpoint { name: nameOf found.from, port: Nothing }
    , NodeEndpoint { name: nameOf found.to, port: Nothing }
    ]
    []

  dot =
    { strict: false
    , kind: Directed
    , name: Nothing
    , statements: statementsIn [] <> map edgeStatement asked
    }

  -- A node's label is empty and takes no room, so a node is as large as it
  -- is asked to be. The only other text is a group's name, which Stemma
  -- keeps a strip for: eight points more than the text is high.
  options =
    { measure: \written ->
        if written == "" then { width: 0.0, height: 0.0 }
        else { width: estimateWidth picture.lettering.titleSize written, height: max 0.0 (picture.outline.header - 8.0) }
    , nodeSeparation: metrics.nodeSeparation
    , rankSeparation: metrics.rankSeparation
    }

  -- ---- the drawing as a layout ----------------------------------------------
  cornered :: Stemma.Box -> Box
  cornered box = { x: box.centre.x - box.width / 2.0, y: box.centre.y - box.height / 2.0, width: box.width, height: box.height }

  fromDrawing :: Stemma.Drawing -> Layout wire
  fromDrawing drawing =
    let
      boxes :: Map NodeName Box
      boxes = Map.fromFoldable (drawing.nodes <#> \node -> Tuple node.name (cornered node.box))

      boxOf index = fromMaybe { x: 0.0, y: 0.0, width: metrics.nodeBreadth, height: metrics.nodeHeight } (Map.lookup (NodeName (nameOf index)) boxes)

      -- a rank is a line across the page: the nodes of one have one middle
      lines = Array.sort (Array.nub (map (\found -> middle (boxOf found.index)) given))
      middle box = box.y + box.height / 2.0

      nodes :: Array PlacedNode
      nodes = given <#> \found ->
        let
          box = boxOf found.index
        in
          { index: found.index
          , node: found.node
          , box
          , layer: Array.elemIndex (middle box) lines
          , words: found.words
          , ports: []
          }

      -- Stemma numbers its edges in the order it was given them
      routes :: Map Int { path :: Array Point, turned :: Boolean }
      routes = Map.fromFoldable (Array.zipWith (\want drawn -> Tuple want.index { path: drawn.path, turned: drawn.turned }) asked drawing.edges)

      edges :: Array (PlacedEdge wire)
      edges = Array.zipWith
        ( \found edge ->
            let
              from = boxOf (fromMaybe 0 found.from)
              to = boxOf (fromMaybe 0 found.to)
              drawn = Map.lookup found.index routes
            in
              { index: found.index
              , edge
              , route: routeOf from to (fromMaybe [] (drawn <#> _.path))
              , course: if (drawn <#> _.turned) == Just true || found.from == found.to then Backward else Forward
              }
        )
        ends
        projection.edges

      outlines :: Array PlacedOutline
      outlines = Array.sortWith _.depth $ drawing.clusters # Array.mapMaybe \cluster -> do
        number <- Attributes.lookup groupKey cluster.attributes >>= Int.fromString
        group <- Array.index groups number
        path <- Array.head (Array.mapMaybe (\found -> Array.elemIndex group found.path <#> \place -> Array.take place found.path) given)
        pure
          { group
          , name: fromMaybe "" (home <#> \found -> found.nameOf group)
          , box: cornered cluster.box
          , header: picture.outline.header
          , depth: Array.length path
          , parent: Array.last path
          , members: Array.sort (map _.index (Array.filter (\found -> Array.elem group found.path) given))
          }
    in
      { direction: TopDown
      , nodes
      , edges
      , outlines
      , width: drawing.width
      , height: drawing.height
      }

  -- Should Stemma refuse the graph (it does not refuse one made here), the
  -- nodes one under another and every edge straight down.
  stackedUp :: Layout wire
  stackedUp =
    let
      placed = foldl
        ( \state found ->
            { top: state.top + found.height + metrics.rankSeparation
            , boxes: Array.snoc state.boxes { x: 8.0, y: state.top, width: metrics.nodeBreadth, height: found.height }
            }
        )
        { top: 8.0, boxes: [] }
        given
      boxOf index = fromMaybe { x: 8.0, y: 8.0, width: metrics.nodeBreadth, height: metrics.nodeHeight } (Array.index placed.boxes index)
    in
      { direction: TopDown
      , nodes: given <#> \found -> { index: found.index, node: found.node, box: boxOf found.index, layer: Nothing, words: found.words, ports: [] }
      , edges: Array.zipWith
          (\found edge -> { index: found.index, edge, route: routeOf (boxOf (fromMaybe 0 found.from)) (boxOf (fromMaybe 0 found.to)) [], course: Backward })
          ends
          projection.edges
      , outlines: []
      , width: metrics.nodeBreadth + 16.0
      , height: placed.top
      }

-- | Stemma's path (the start, then three points for each piece) as a
-- | route from one box to the other, its two ends moved onto the boxes'
-- | edges where they are a hair off. A path with no piece: straight from
-- | the bottom of the one box to the top of the other.
routeOf :: Box -> Box -> Array Point -> Route
routeOf from to path = case Array.uncons path of
  Just { head: first, tail: rest } | Just curves <- NonEmptyArray.fromArray (pieces rest) ->
    Spline
      { from: onto from first
      , curves: NonEmptyArray.snoc' (NonEmptyArray.init curves) ((NonEmptyArray.last curves) { to = onto to (NonEmptyArray.last curves).to })
      }
  _ ->
    let
      start = { x: from.x + from.width / 2.0, y: from.y + from.height }
      finish = { x: to.x + to.width / 2.0, y: to.y }
    in
      Spline { from: start, curves: NonEmptyArray.singleton { leaving: start, arriving: finish, to: finish } }
  where
  pieces :: Array Point -> Array Curve
  pieces points = case Array.take 3 points of
    [ leaving, arriving, end ] -> Array.cons { leaving, arriving, to: end } (pieces (Array.drop 3 points))
    _ -> []

-- | The point of a box's edge nearest a point.
onto :: Box -> Point -> Point
onto box point =
  let
    x = max box.x (min (box.x + box.width) point.x)
    y = max box.y (min (box.y + box.height) point.y)
    toLeft = abs (x - box.x)
    toRight = abs (box.x + box.width - x)
    toTop = abs (y - box.y)
    toBottom = abs (box.y + box.height - y)
    least = min (min toLeft toRight) (min toTop toBottom)
  in
    if least == toTop then { x, y: box.y }
    else if least == toBottom then { x, y: box.y + box.height }
    else if least == toLeft then { x: box.x, y }
    else { x: box.x + box.width, y }
