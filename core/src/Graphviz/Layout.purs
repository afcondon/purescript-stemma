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
-- | **The geometry is interim.** Ranks and the order within each rank come
-- | from the real passes (`Graphviz.Layout.Rank`, `Graphviz.Layout.Order`).
-- | The coordinates do not yet: each rank is simply spaced out left to
-- | right and centred, and an edge is straight pieces from node to bend to
-- | node. The passes that will replace this (coordinates by network
-- | simplex, edges as curves routed round the nodes) will fill the same
-- | `Drawing`, so what is built on this type should not need to change.
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
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, maximum, minimum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Graphviz.Attributes (Attributes)
import Graphviz.Attributes as Attributes
import Graphviz.Dot.Parser (parse)
import Graphviz.Dot.Syntax (text)
import Graphviz.Graph (EdgeId, Graph, NodeName(..), SubgraphId, fromDot)
import Graphviz.Graph as Graph
import Graphviz.Layout.Order (Vertex(..), layered, order)
import Graphviz.Layout.Rank (Rank(..), describe, rank, rankOf, ranking, reversed)

type Point = { x :: Number, y :: Number }

-- | A rectangle, by its centre.
type Box = { centre :: Point, width :: Number, height :: Number }

-- | `label` is the text to show: the node's `label` attribute if it has
-- | one, otherwise its name.
type DrawnNode = { name :: NodeName, label :: String, box :: Box, attributes :: Attributes }

-- | `path` runs from the tail node to the head node. `turned` says the
-- | layout had to run this edge against the flow to break a cycle, so it
-- | points up the page; it is still drawn from tail to head.
type DrawnEdge =
  { id :: EdgeId
  , tail :: NodeName
  , head :: NodeName
  , path :: Array Point
  , turned :: Boolean
  , attributes :: Attributes
  }

type DrawnCluster = { id :: SubgraphId, label :: Maybe String, box :: Box, attributes :: Attributes }

-- | Clusters are in drawing order: an outer cluster comes before the ones
-- | inside it.
type Drawing =
  { width :: Number
  , height :: Number
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
-- | little over half its height per character.
defaults :: Options
defaults =
  { measure: \written -> { width: 7.5 * toNumber (String.length written), height: 14.0 }
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
        Rank r -> r) (reversed solved) arranged.ranks arranged.clusters)

-- The interim geometry.
place
  :: Options
  -> Graph
  -> (NodeName -> Int)
  -> Array EdgeId
  -> Array (Array Vertex)
  -> Map Vertex (Array SubgraphId)
  -> Drawing
place options graph rankNumber turned ranks inClusters =
  turnedRound
    { width: extent.width
    , height: extent.height
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

  -- A node is as big as Graphviz's default (three quarters of an inch by
  -- half an inch) or as its label needs, whichever is more. A bend takes
  -- up no room of its own.
  sizeOf = case _ of
    Bend _ _ _ -> { width: 0.0, height: 0.0 }
    Node name -> case Map.lookup name nodesByName of
      Nothing -> { width: 54.0, height: 36.0 }
      Just node ->
        let
          needed = options.measure (labelOf node)
        in
          { width: max 54.0 (needed.width + 16.0), height: max 36.0 (needed.height + 16.0) }

  -- Each rank is laid out left to right from 0, then centred on the
  -- widest; ranks are stacked with the rank separation between them.
  rows = map
    ( \vertices ->
        let
          laid = foldl
            ( \state vertex ->
                let
                  size = sizeOf vertex
                in
                  { next: state.next + size.width + options.nodeSeparation
                  , found: Array.snoc state.found { vertex, left: state.next, size }
                  }
            )
            { next: 0.0, found: [] }
            vertices
        in
          { items: laid.found
          , width: max 0.0 (laid.next - options.nodeSeparation)
          , height: foldl (\tallest item -> max tallest item.size.height) 0.0 laid.found
          }
    )
    ranks
  widest = foldl (\most row -> max most row.width) 0.0 rows
  stacked = foldl
    ( \state row ->
        { top: state.top + row.height + options.rankSeparation
        , found: Array.snoc state.found { row, top: state.top }
        }
    )
    { top: margin, found: [] }
    rows
  boxes :: Map Vertex Box
  boxes = Map.fromFoldable
    ( Array.concatMap
        ( \placed -> map
            ( \item -> Tuple item.vertex
                { centre:
                    { x: margin + (widest - placed.row.width) / 2.0 + item.left + item.size.width / 2.0
                    , y: placed.top + placed.row.height / 2.0
                    }
                , width: item.size.width
                , height: item.size.height
                }
            )
            placed.row.items
        )
        stacked.found
    )
  boxOf vertex = case Map.lookup vertex boxes of
    Just found -> found
    Nothing -> { centre: { x: 0.0, y: 0.0 }, width: 0.0, height: 0.0 }
  extent =
    { width: widest + 2.0 * margin
    , height: max (2.0 * margin) (stacked.top - options.rankSeparation + margin)
    }

  -- An edge goes from its tail to its head through the bends between
  -- them. An edge from a node to itself is a small loop on its right.
  edge found =
    { id: found.id
    , tail: found.tail
    , head: found.head
    , turned: Array.elem found.id turned
    , attributes: found.attributes
    , path:
        if found.tail == found.head then loop (boxOf (Node found.tail))
        else
          let
            from = rankNumber found.tail
            to = rankNumber found.head
            upper = if from <= to then found.tail else found.head
            lower = if from <= to then found.head else found.tail
            passed = if max from to - min from to > 1 then Array.range (min from to + 1) (max from to - 1) else []
            downwards = [ Node upper ] <> map (Bend upper lower) passed <> [ Node lower ]
            between = if from == to then [ Node found.tail, Node found.head ] else if from < to then downwards else Array.reverse downwards
          in
            map (boxOf >>> _.centre) between
    }
  loop box =
    [ { x: box.centre.x + box.width / 2.0, y: box.centre.y - box.height / 4.0 }
    , { x: box.centre.x + box.width / 2.0 + 18.0, y: box.centre.y - box.height / 4.0 }
    , { x: box.centre.x + box.width / 2.0 + 18.0, y: box.centre.y + box.height / 4.0 }
    , { x: box.centre.x + box.width / 2.0, y: box.centre.y + box.height / 4.0 }
    ]

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
        left <- minimum (map (\item -> item.box.centre.x - item.box.width / 2.0 - padding item) inside)
        right <- maximum (map (\item -> item.box.centre.x + item.box.width / 2.0 + padding item) inside)
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
    Just "LR" -> sideways (mapPoints (\point -> { x: point.y, y: point.x }) swap drawing)
    Just "RL" -> sideways (mapPoints (\point -> { x: drawing.height - point.y, y: point.x }) swap drawing)
    _ -> drawing
  sideways drawing = drawing { width = drawing.height, height = drawing.width }
  swap box = box { width = box.height, height = box.width }
  mapPoints move reshape drawing = drawing
    { nodes = map (\node -> node { box = keepSize (moveBox move node.box) }) drawing.nodes
    , edges = map (\found -> found { path = map move found.path }) drawing.edges
    , clusters = map (\found -> found { box = reshape (moveBox move found.box) }) drawing.clusters
    }
  moveBox move box = box { centre = move box.centre }
  -- A node keeps its shape when the drawing is turned; only its place
  -- changes.
  keepSize = identity
