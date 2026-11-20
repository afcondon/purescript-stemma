-- | The second pass of `dot`: the order of the nodes within each rank,
-- | chosen so that few edges cross.
-- |
-- | An edge that spans several ranks is first cut into a chain of short
-- | pieces, with a bend on each rank it passes through; a bend takes a
-- | place in its rank's order like a node. Several edges between the same
-- | two nodes share one chain. After that every piece joins two
-- | neighbouring ranks, and a crossing is two pieces between the same two
-- | ranks whose ends are in opposite orders.
-- |
-- | Finding the order with the fewest crossings is hard (even for two
-- | ranks), so this is a search that settles for a good one, the search of
-- | Gansner, Koutsofios, North and Vo (1993), section 3:
-- |
-- |   1. Start from the order a breadth-first walk reaches the nodes in.
-- |   2. Sweep down the ranks, then up, then down again. On each rank put
-- |      every node at the median position of its neighbours on the rank
-- |      just passed.
-- |   3. After each sweep, swap neighbours in a rank wherever that removes
-- |      crossings, until no swap helps.
-- |   4. Keep the best order seen.
-- |
-- | It is run briefly from a walk started at the top and from one started
-- | at the bottom, and then at length from the better of the two.
-- |
-- | Unlike ranking, this has no optimum to be held to. `dot`'s own order is
-- | a different run of a similar search, so the two are compared by the
-- | number of crossings each leaves.
module Graphviz.Layout.Order
  ( Vertex(..)
  , Link
  , Layered
  , Unsupported(..)
  , describe
  , layered
  , order
  , crossings
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldl, sum)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Graphviz.Graph (Graph, NodeName)
import Graphviz.Graph as Graph
import Graphviz.Layout.Rank (Rank(..))

-- | Something with a place in a rank: a node, or a bend in the edges from
-- | one node down to another, on the rank given.
data Vertex = Node NodeName | Bend NodeName NodeName Int

derive instance eqVertex :: Eq Vertex
derive instance ordVertex :: Ord Vertex

-- | A piece of edge between a rank and the next one down. `count` is how
-- | many of the graph's edges run along it.
type Link = { upper :: Vertex, lower :: Vertex, count :: Int }

-- | A graph ready to be ordered: the vertices of each rank, top rank
-- | first, and the links below each rank (so one fewer than the ranks).
-- | Used as an answer, `ranks` is each rank from left to right.
type Layered = { ranks :: Array (Array Vertex), links :: Array (Array Link) }

-- | What `dot` does in this pass that is not written here yet.
data Unsupported = Clusters | FlatEdges

derive instance eqUnsupported :: Eq Unsupported

describe :: Unsupported -> String
describe = case _ of
  Clusters -> "it has clusters, whose nodes must stay side by side"
  FlatEdges -> "it has an edge between two nodes on the same rank"

-- | Cut the graph's edges into links, given the rank of every node.
layered :: Graph -> (NodeName -> Rank) -> Either Unsupported Layered
layered graph rankOf =
  if not (Array.null (Graph.clusters graph)) then Left Clusters
  else if any (\edge -> edge.tail /= edge.head && rankOf edge.tail == rankOf edge.head) (Graph.edges graph) then Left FlatEdges
  else Right
    { ranks: map (\r -> nodesOn r <> bendsOn r) depths
    , links: map (\r -> Array.concatMap (linksBelow r) bundles) (Array.dropEnd 1 depths)
    }
  where
  number node = case rankOf node of
    Rank r -> r
  names = map _.name (Graph.nodes graph)
  top = foldl (\best node -> min best (number node)) 0 names
  bottom = foldl (\best node -> max best (number node)) 0 names
  depths = Array.range top bottom

  -- The edges that run between the same two nodes, as one: its upper
  -- node, its lower node, and how many there are.
  bundles :: Array { upper :: NodeName, lower :: NodeName, count :: Int }
  bundles = map (\(Tuple (Tuple upper lower) count) -> { upper, lower, count })
    ( Map.toUnfoldable
        ( foldl
            ( \soFar edge ->
                if edge.tail == edge.head then soFar
                else if number edge.tail < number edge.head then Map.insertWith add (Tuple edge.tail edge.head) 1 soFar
                else Map.insertWith add (Tuple edge.head edge.tail) 1 soFar
            )
            Map.empty
            (Graph.edges graph)
        )
    )

  nodesOn r = map Node (Array.filter (\node -> number node == r) names)
  bendsOn r = Array.mapMaybe
    (\bundle -> if number bundle.upper < r && r < number bundle.lower then Just (Bend bundle.upper bundle.lower r) else Nothing)
    bundles

  -- The bundle's vertex on a rank it touches or passes through.
  vertexOn bundle r =
    if r == number bundle.upper then Node bundle.upper
    else if r == number bundle.lower then Node bundle.lower
    else Bend bundle.upper bundle.lower r

  linksBelow r bundle =
    if number bundle.upper <= r && r < number bundle.lower then
      [ { upper: vertexOn bundle r, lower: vertexOn bundle (r + 1), count: bundle.count } ]
    else []

-- | How many crossings an order leaves. Two links cross if their upper
-- | ends are one way round and their lower ends the other; links that
-- | carry several edges count for the product of their numbers.
crossings :: Layered -> Int
crossings arranged = sum (map between arranged.links)
  where
  places = positions arranged.ranks
  placed link = do
    upper <- Map.lookup link.upper places
    lower <- Map.lookup link.lower places
    pure { upper, lower, count: link.count }
  between links =
    let
      found = Array.mapMaybe placed links
    in
      sum (map (\a -> sum (map (\b -> if a.upper < b.upper && a.lower > b.lower then a.count * b.count else 0) found)) found)

-- The place of every vertex in its rank, counting from 0 at the left.
positions :: Array (Array Vertex) -> Map Vertex Int
positions = foldl (\soFar rank -> foldl (\m (Tuple place vertex) -> Map.insert vertex place m) soFar (Array.mapWithIndex Tuple rank)) Map.empty

-- | Order the ranks to leave few crossings.
order :: Layered -> Layered
order given = arranged best
  where
  arranged ranks = given { ranks = ranks }

  below = foldl (\m link -> Map.insertWith (flip append) link.upper [ link ] m) Map.empty (Array.concat given.links)
  above = foldl (\m link -> Map.insertWith (flip append) link.lower [ link ] m) Map.empty (Array.concat given.links)
  linksBelow vertex = case Map.lookup vertex below of
    Just found -> found
    Nothing -> []
  linksAbove vertex = case Map.lookup vertex above of
    Just found -> found
    Nothing -> []

  rankOfVertex :: Map Vertex Int
  rankOfVertex = foldl (\soFar (Tuple r rank) -> foldl (\m vertex -> Map.insert vertex r m) soFar rank) Map.empty
    (Array.mapWithIndex Tuple given.ranks)

  -- The search, on dot's schedule: four rounds from each of two starting
  -- orders, then up to twenty-four more from the best order so far,
  -- stopping early once eight rounds in a row have not helped.
  fromTop = rounds 4 (start (breadthFirst (Array.concat given.ranks) (map _.upper <<< linksAbove) (map _.lower <<< linksBelow)))
  fromBottom = rounds 4 (start (breadthFirst (Array.concat given.ranks) (map _.lower <<< linksBelow) (map _.upper <<< linksAbove)))
  best = (rounds 24 (start (if fromBottom.fewest <= fromTop.fewest then fromBottom.best else fromTop.best))).best

  start ranks = { now: ranks, best: ranks, fewest: crossings (arranged ranks), idle: 0 }

  -- Step 1: the order a breadth-first walk reaches the vertices in, rank
  -- by rank, starting from the vertices that have nothing behind them.
  breadthFirst :: Array Vertex -> (Vertex -> Array Vertex) -> (Vertex -> Array Vertex) -> Array (Array Vertex)
  breadthFirst vertices behind ahead =
    let
      visit state vertex =
        if Set.member vertex state.seen then state
        else spread (Cons vertex Nil) Nil state { seen = Set.insert vertex state.seen }
      -- A queue as two lists: taken from the front, added to the back.
      spread front back state = case front of
        Cons vertex rest ->
          let
            fresh = Array.filter (\next -> not (Set.member next state.seen)) (Array.nub (ahead vertex))
          in
            spread rest (foldl (flip Cons) back fresh)
              { seen: foldl (flip Set.insert) state.seen fresh, newestFirst: Cons vertex state.newestFirst }
        Nil -> case back of
          Nil -> state
          _ -> spread (List.reverse back) Nil state
      reached = foldl visit { seen: Set.empty, newestFirst: Nil }
        (Array.filter (behind >>> Array.null) vertices <> vertices)
      inOrder = Array.fromFoldable (List.reverse reached.newestFirst)
    in
      Array.mapWithIndex (\r _ -> Array.filter (\vertex -> Map.lookup vertex rankOfVertex == Just r) inOrder) given.ranks

  -- Steps 2 to 4.
  rounds limit begun = foldl sweep begun (Array.range 0 (limit - 1))
    where
    sweep state round =
      if state.fewest == 0 || state.idle >= 8 then state
      else
        let
          now = transpose (round `mod` 4 >= 2) (medians round state.now)
          left = crossings (arranged now)
        in
          if left <= state.fewest then
            { now, best: now, fewest: left, idle: if toNumber left < 0.995 * toNumber state.fewest then 0 else state.idle + 1 }
          else state { now = now, idle = state.idle + 1 }

  -- Step 2: one sweep. Even rounds go down, odd rounds up, and every
  -- other pair of rounds leaves equal medians the other way round, so the
  -- search does not keep making the same choice.
  medians :: Int -> Array (Array Vertex) -> Array (Array Vertex)
  medians round ranks =
    if round `mod` 2 == 0 then
      foldl (\soFar r -> place r (map _.upper <<< linksAbove) soFar) ranks (Array.range 1 (Array.length ranks - 1))
    else
      foldl (\soFar r -> place r (map _.lower <<< linksBelow) soFar) ranks (Array.reverse (Array.range 0 (Array.length ranks - 2)))
    where
    flipTies = round `mod` 4 < 2
    place r neighbours soFar = case Array.index soFar r of
      Nothing -> soFar
      Just rank ->
        let
          places = positions soFar
          keyed = map (\vertex -> { vertex, median: median (Array.sort (Array.mapMaybe (\other -> Map.lookup other places) (neighbours vertex))) }) rank
          -- A vertex with no neighbours on that side stays where it is;
          -- the others are sorted into the places that remain.
          movable = Array.sortBy (comparing _.median) ((if flipTies then Array.reverse else identity) (Array.filter (_.median >>> (_ >= 0.0)) keyed))
          refill state entry =
            if entry.median < 0.0 then state { placed = Array.snoc state.placed entry.vertex }
            else case Array.uncons state.waiting of
              Just next -> { placed: Array.snoc state.placed next.head.vertex, waiting: next.tail }
              Nothing -> state
        in
          case Array.updateAt r (foldl refill { placed: [], waiting: movable } keyed).placed soFar of
            Just updated -> updated
            Nothing -> soFar

  -- The middle of some places: the middle one, the mean of two, and for
  -- an even number more than two, a point between the middle two pulled
  -- towards the side where the places are packed closer.
  median :: Array Int -> Number
  median places = case Array.length places of
    0 -> -1.0
    n ->
      let
        middle = n / 2
        at i = case Array.index places i of
          Just found -> toNumber found
          Nothing -> 0.0
      in
        if n `mod` 2 == 1 then at middle
        else if n == 2 then (at 0 + at 1) / 2.0
        else
          let
            left = at (middle - 1) - at 0
            right = at (n - 1) - at middle
          in
            if left + right == 0.0 then (at (middle - 1) + at middle) / 2.0
            else (at (middle - 1) * right + at middle * left) / (left + right)

  -- Step 3: swap neighbours wherever that removes crossings, rank after
  -- rank, until a whole pass removes none. On some rounds two neighbours
  -- are also swapped when it makes no difference (and they have crossings
  -- between them), to shake the order out of a rut.
  transpose :: Boolean -> Array (Array Vertex) -> Array (Array Vertex)
  transpose swapTies = go 50
    where
    go left ranks =
      if left <= 0 then ranks
      else
        let
          pass = foldl swapRank { ranks, removed: 0 } (Array.range 0 (Array.length ranks - 1))
        in
          if pass.removed > 0 then go (left - 1) pass.ranks else pass.ranks

    swapRank state r = case Array.index state.ranks r of
      Nothing -> state
      Just rank ->
        let
          places = positions state.ranks
          ends vertex = Array.mapMaybe (\link -> { place: _, count: link.count } <$> Map.lookup link.upper places) (linksAbove vertex)
          endsBelow vertex = Array.mapMaybe (\link -> { place: _, count: link.count } <$> Map.lookup link.lower places) (linksBelow vertex)
          -- Crossings between the links of two vertices when the first is
          -- on the left.
          crossed leftEnds rightEnds = sum (map (\a -> sum (map (\b -> if a.place > b.place then a.count * b.count else 0) rightEnds)) leftEnds)
          cost left right = crossed (ends left) (ends right) + crossed (endsBelow left) (endsBelow right)
          step acc vertex = case Array.unsnoc acc.done of
            Just { init, last } ->
              let
                asIs = cost last vertex
                swapped' = cost vertex last
              in
                if swapped' < asIs || (swapTies && asIs > 0 && swapped' == asIs) then
                  { done: Array.snoc (Array.snoc init vertex) last, removed: acc.removed + (asIs - swapped'), moved: true }
                else acc { done = Array.snoc acc.done vertex }
            Nothing -> acc { done = Array.snoc acc.done vertex }
          swapped = foldl step { done: [], removed: 0, moved: false } rank
        in
          if swapped.moved then
            { ranks: case Array.updateAt r swapped.done state.ranks of
                Just updated -> updated
                Nothing -> state.ranks
            , removed: state.removed + swapped.removed
            }
          else state
