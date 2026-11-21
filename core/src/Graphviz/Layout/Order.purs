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
-- | Two things hold in every order it considers:
-- |
-- |   - The nodes of a cluster that share a rank are side by side, with the
-- |     bends of the edges inside the cluster among them, and so on for
-- |     clusters inside clusters. A rank is treated as a tree: at each
-- |     level the things ordered are single vertices and whole clusters.
-- |   - An edge between two nodes on the same rank has its tail on the
-- |     left, as far as that can be had: where such edges go round in a
-- |     circle, or ask for a node to stand between two of a cluster, one
-- |     is let go.
-- |
-- | Unlike ranking, this has no optimum to be held to. `dot`'s own order is
-- | a different run of a similar search, so the two are compared by the
-- | number of crossings each leaves.
module Graphviz.Layout.Order
  ( Vertex(..)
  , Link
  , Layered
  , layered
  , order
  , crossings
  , split
  , backwards
  , backwardsBetweenClusters
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, any, foldl, sum)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Graphviz.Attributes as Attributes
import Graphviz.Graph (Graph, NodeName, SubgraphId)
import Graphviz.Graph as Graph
import Graphviz.Layout.Acyclic as Acyclic
import Graphviz.Layout.Rank (Rank(..), clusterPaths)

-- | Something with a place in a rank: a node, or a bend in the edges from
-- | one node down to another, on the rank given.
data Vertex = Node NodeName | Bend NodeName NodeName Int

derive instance eqVertex :: Eq Vertex
derive instance ordVertex :: Ord Vertex

-- | A piece of edge between a rank and the next one down. `count` is how
-- | many of the graph's edges run along it, not counting those marked
-- | `constraint=false`: as with dot, such an edge has no say in the order
-- | and its crossings are not counted, so a link that carries only those
-- | has a count of 0.
type Link = { upper :: Vertex, lower :: Vertex, count :: Int }

-- | A graph ready to be ordered, or once ordered:
-- |
-- |   - `ranks`: the vertices of each rank, top rank first; in an answer,
-- |     each rank from left to right.
-- |   - `links`: the links below each rank (so one fewer than the ranks).
-- |   - `clusters`: the clusters each vertex is in, outermost first.
-- |   - `leftOf`: for a vertex, those on its rank that must be to its
-- |     right.
type Layered =
  { ranks :: Array (Array Vertex)
  , links :: Array (Array Link)
  , clusters :: Map Vertex (Array SubgraphId)
  , leftOf :: Map Vertex (Set Vertex)
  }

-- | Cut the graph's edges into links, given the rank of every node.
layered :: Graph -> (NodeName -> Rank) -> Layered
layered graph rankOf =
  { ranks: map (\r -> nodesOn r <> bendsOn r) depths
  , links: map (\r -> Array.concatMap (linksBelow r) bundles) (Array.dropEnd 1 depths)
  , clusters: Map.fromFoldable
      ( map (\node -> Tuple (Node node) (pathOf node)) names
          <> Array.concatMap
            (\bundle -> map (\r -> Tuple (Bend bundle.upper bundle.lower r) (shared (pathOf bundle.upper) (pathOf bundle.lower))) (between bundle))
            bundles
      )
  , leftOf: reachable
  }
  where
  number node = case rankOf node of
    Rank r -> r
  names = map _.name (Graph.nodes graph)
  top = foldl (\best node -> min best (number node)) 0 names
  bottom = foldl (\best node -> max best (number node)) 0 names
  depths = Array.range top bottom

  paths = clusterPaths graph
  pathOf node = case Map.lookup node paths of
    Just found -> found
    Nothing -> []
  -- The clusters two nodes are both in: where their paths agree.
  shared a b = map (\(Tuple x _) -> x) (Array.takeWhile (\(Tuple x y) -> x == y) (Array.zip a b))

  -- The edges that run between the same two nodes on different ranks, as
  -- one: its upper node, its lower node, and how many there are.
  bundles :: Array { upper :: NodeName, lower :: NodeName, count :: Int }
  bundles = map (\(Tuple (Tuple upper lower) count) -> { upper, lower, count })
    ( Map.toUnfoldable
        ( foldl
            ( \soFar edge ->
                let
                  counts = if Attributes.constrains edge.attributes then 1 else 0
                in
                  if number edge.tail < number edge.head then Map.insertWith add (Tuple edge.tail edge.head) counts soFar
                  else if number edge.tail > number edge.head then Map.insertWith add (Tuple edge.head edge.tail) counts soFar
                  else soFar
            )
            Map.empty
            (Graph.edges graph)
        )
    )

  between bundle = Array.filter (\r -> number bundle.upper < r && r < number bundle.lower) depths
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

  -- Edges along a rank: tail on the left (on the right when the drawing
  -- is turned on its side, `rankdir` LR or RL, so that the tail is still
  -- the upper of the two once it is turned). Ones with no weight, or
  -- marked `constraint=false`, ask for nothing, as with dot.
  sideways = any (\direction -> direction == "LR" || direction == "RL") (Attributes.lookup "rankdir" (Graph.attributes graph))
  along = map (\edge -> if sideways then Tuple edge.head edge.tail else Tuple edge.tail edge.head)
    ( Array.filter
        ( \edge -> edge.tail /= edge.head && number edge.tail == number edge.head
            && Attributes.weight edge.attributes > 0
            && Attributes.constrains edge.attributes
        )
        (Graph.edges graph)
    )

  -- What those edges ask, once made consistent with keeping clusters
  -- together. At each level of a rank's tree the things ordered are
  -- single nodes and whole clusters, so an edge between nodes in two
  -- different things asks for one thing to be left of the other. Where
  -- that goes round in a circle (a cluster cannot be both sides of a
  -- node), one request is turned round, as a depth-first walk finds it.
  -- What is left is carried along chains, and applies to every node of
  -- the things concerned. Then the same inside each cluster.
  settled :: Array (Tuple Vertex Vertex)
  settled = Array.concatMap
    ( \r -> level 0 (nodesOn r <> bendsOn r)
        (map (\(Tuple l right) -> Tuple (Node l) (Node right)) (Array.filter (\(Tuple l _) -> number l == r) along))
    )
    depths
    where
    pathOfVertex = case _ of
      Node node -> pathOf node
      Bend upper lower _ -> shared (pathOf upper) (pathOf lower)
    thing depth vertex = case Array.index (pathOfVertex vertex) depth of
      Just cluster -> Whole cluster
      Nothing -> Alone vertex
    -- One level of one rank: its vertices, and the requests among them.
    level depth vertices asked =
      let
        across = Array.filter (\(Tuple l r) -> thing depth l /= thing depth r) asked
        asWritten = foldl (\fast (Tuple l r) -> Acyclic.connect (thing depth l) (thing depth r) fast) Acyclic.empty across
        things = Array.nub (map (thing depth) vertices)
        turned = Acyclic.breakCycles things asWritten
        directly = foldl
          ( \soFar (Tuple l r) ->
              if Set.member (Tuple (thing depth l) (thing depth r)) turned then Map.insertWith append (thing depth r) [ thing depth l ] soFar
              else Map.insertWith append (thing depth l) [ thing depth r ] soFar
          )
          Map.empty
          across
        reach pending found = case pending of
          Nil -> found
          Cons from rest ->
            let
              fresh = Array.filter (\next -> not (Set.member next found)) (case Map.lookup from directly of
                Just next -> next
                Nothing -> [])
            in
              reach (foldl (flip Cons) rest fresh) (foldl (flip Set.insert) found fresh)
        membersOf what = Array.filter (\vertex -> thing depth vertex == what) vertices
        here = Array.concatMap
          ( \left -> Array.concatMap
              (\right -> Array.concatMap (\l -> map (Tuple l) (membersOf right)) (membersOf left))
              (Array.fromFoldable (reach (Cons left Nil) Set.empty))
          )
          (Array.fromFoldable (Map.keys directly))
        inside = Array.concatMap
          ( case _ of
              Whole cluster ->
                level (depth + 1) (membersOf (Whole cluster))
                  (Array.filter (\(Tuple l r) -> thing depth l == Whole cluster && thing depth r == Whole cluster) asked)
              Alone _ -> []
          )
          things
      in
        if Array.null asked then [] else here <> inside

  reachable = foldl (\soFar (Tuple l r) -> Map.insertWith Set.union l (Set.singleton r) soFar) Map.empty settled

-- One of the things ordered at a level of a rank's tree.
data Thing = Alone Vertex | Whole SubgraphId

derive instance eqThing :: Eq Thing
derive instance ordThing :: Ord Thing

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

-- | How many times an order splits a cluster: on some rank, a vertex that
-- | is not in the cluster stands between two that are.
split :: Layered -> Int
split arranged = sum (map onRank arranged.ranks)
  where
  pathOf vertex = case Map.lookup vertex arranged.clusters of
    Just found -> found
    Nothing -> []
  onRank rank =
    let
      clusters = Array.nub (Array.concatMap pathOf rank)
      strangers cluster =
        let
          member = map (pathOf >>> Array.elem cluster) rank
          first = Array.findIndex identity member
          last = Array.findLastIndex identity member
        in
          case first, last of
            Just from, Just to -> Array.length (Array.filter not (Array.slice from (to + 1) member))
            _, _ -> 0
    in
      sum (map strangers clusters)

-- | How many same-rank edges an order has with the tail on the right.
backwards :: Layered -> Int
backwards = wrongWay (\_ _ -> true)

-- | Of those, the ones whose two vertices are in different clusters that
-- | stand side by side. These cannot always be kept: the clusters have one
-- | order for the whole drawing, and edges along two ranks can ask for
-- | both orders.
backwardsBetweenClusters :: Layered -> Int
backwardsBetweenClusters arranged = wrongWay parted arranged
  where
  pathOf vertex = case Map.lookup vertex arranged.clusters of
    Just found -> found
    Nothing -> []
  parted l r =
    let
      agreed = Array.length (Array.takeWhile identity (Array.zipWith eq (pathOf l) (pathOf r)))
    in
      Array.length (pathOf l) > agreed && Array.length (pathOf r) > agreed

wrongWay :: (Vertex -> Vertex -> Boolean) -> Layered -> Int
wrongWay counts arranged = sum (map wrong (Map.toUnfoldable arranged.leftOf :: Array (Tuple Vertex (Set Vertex))))
  where
  places = positions arranged.ranks
  wrong (Tuple left rights) = case Map.lookup left places of
    Nothing -> 0
    Just here -> Array.length (Array.filter (\right -> counts left right && any (_ < here) (Map.lookup right places)) (Array.fromFoldable rights))

-- The place of every vertex in its rank, counting from 0 at the left.
positions :: Array (Array Vertex) -> Map Vertex Int
positions = foldl (\soFar rank -> foldl (\m (Tuple place vertex) -> Map.insert vertex place m) soFar (Array.mapWithIndex Tuple rank)) Map.empty

-- A rank as a tree: at each level, single vertices and whole clusters.
data Item = Single Vertex | Group SubgraphId (Array Item)

members :: Item -> Array Vertex
members = case _ of
  Single vertex -> [ vertex ]
  Group _ inner -> Array.concatMap members inner

-- | Order the ranks to leave few crossings.
-- |
-- | Each rank keeps a cluster's vertices together, but ordering the ranks
-- | freely can leave cluster A left of cluster B in one rank and right of
-- | it in another, and then no box can be drawn round each. When that
-- | happens the clusters are put in one line for the whole drawing, by
-- | where their vertices came to stand, without going against any edge
-- | along a rank, and the ranks are ordered again with that line as
-- | something every rank must keep.
order :: Layered -> Layered
order given =
  if all Array.null given.clusters || agrees first.ranks then first
  else if agrees again.ranks then again
  else again { ranks = map (straighten 0) again.ranks }
  where
  first = orderOnce given
  agrees ranks =
    let
      found = Set.fromFoldable (map (\parted -> Tuple parted.left parted.right) (Array.concatMap apart ranks))
    in
      not (any (\(Tuple a b) -> Set.member (Tuple b a) found) found)
  -- The line comes before an edge along a rank that asks otherwise.
  withLine = foldl (\m (Tuple l r) -> Map.insertWith Set.union l (Set.singleton r) m)
    (map (\_ -> Set.empty) given.leftOf)
    (kept <> Array.concatMap
      ( \(Tuple l rights) -> Array.mapMaybe
          ( \r -> case parting l r of
              Just parted | Map.lookup parted.left placeInLine > Map.lookup parted.right placeInLine -> Nothing
              _ -> Just (Tuple l r)
          )
          (Set.toUnfoldable rights)
      )
      (Map.toUnfoldable given.leftOf :: Array (Tuple Vertex (Set Vertex))))
  again = (orderOnce (given { leftOf = withLine })) { leftOf = given.leftOf }
  -- If the ranks still disagree, the clusters standing side by side in a
  -- rank change places among themselves until they are in line.
  straighten depth rank =
    let
      at vertex = Array.index (pathOf vertex) depth
      runs = map NonEmptyArray.toArray (Array.groupBy (\a b -> at a == at b && at a /= Nothing) rank)
      clusterOf run = Array.head run >>= at
      groups = Array.sortWith (\run -> clusterOf run >>= \cluster -> Map.lookup cluster placeInLine) (Array.filter (\run -> clusterOf run /= Nothing) runs)
      fill state run = case clusterOf run of
        Nothing -> state { placed = state.placed <> run }
        Just _ -> case Array.index groups state.next of
          Just group -> { next: state.next + 1, placed: state.placed <> straighten (depth + 1) group }
          Nothing -> state { placed = state.placed <> run }
    in
      (foldl fill { next: 0, placed: [] } runs).placed
  pathOf vertex = case Map.lookup vertex given.clusters of
    Just found -> found
    Nothing -> []
  -- The two clusters, side by side in some larger one, that part two
  -- vertices.
  parting l r =
    let
      agreed = Array.length (Array.takeWhile identity (Array.zipWith eq (pathOf l) (pathOf r)))
    in
      case Array.index (pathOf l) agreed, Array.index (pathOf r) agreed of
        Just a, Just b -> Just { left: a, right: b, l, r }
        _, _ -> Nothing
  apart rank = Array.concat (Array.mapWithIndex (\place l -> Array.mapMaybe (parting l) (Array.drop (place + 1) rank)) rank)
  -- Which must be left of which, because of an edge along a rank.
  asked = Set.fromFoldable
    ( Array.concatMap
        (\(Tuple l rights) -> map (\found -> Tuple found.left found.right) (Array.mapMaybe (parting l) (Set.toUnfoldable rights)))
        (Map.toUnfoldable given.leftOf :: Array (Tuple Vertex (Set Vertex)))
    )
  standing = foldl
    ( \soFar rank -> foldl
        ( \m (Tuple place vertex) -> foldl
            ( \inner cluster -> Map.insertWith (\old new -> { total: old.total + new.total, count: old.count + new.count }) cluster
                { total: toNumber place / toNumber (max 1 (Array.length rank - 1)), count: 1.0 }
                inner
            )
            m
            (pathOf vertex)
        )
        soFar
        (Array.mapWithIndex Tuple rank)
    )
    Map.empty
    first.ranks
  score cluster = case Map.lookup cluster standing of
    Just found -> Tuple (found.total / found.count) cluster
    Nothing -> Tuple 0.0 cluster
  -- The line: the leftmost-standing cluster that nothing still waiting
  -- must be left of, again and again.
  line remaining = case Array.head (Array.sortWith score (Array.filter free remaining)), Array.head (Array.sortWith score remaining) of
    Just next, _ -> Array.cons next (line (Array.delete next remaining))
    Nothing, Just next -> Array.cons next (line (Array.delete next remaining))
    Nothing, Nothing -> []
    where
    free cluster = not (any (\other -> other /= cluster && Set.member (Tuple other cluster) asked) remaining)
  placeInLine = Map.fromFoldable (Array.mapWithIndex (\place cluster -> Tuple cluster place) (line (Array.nub (Array.concat (Array.fromFoldable (Map.values given.clusters))))))
  kept = map
    (\found -> if Map.lookup found.left placeInLine <= Map.lookup found.right placeInLine then Tuple found.l found.r else Tuple found.r found.l)
    (Array.concatMap apart given.ranks)

orderOnce :: Layered -> Layered
orderOnce given = arranged best
  where
  arranged ranks = given { ranks = ranks }

  -- Only links that count have a say.
  counting = Array.filter (_.count >>> (_ > 0)) (Array.concat given.links)
  below = foldl (\m link -> Map.insertWith (flip append) link.upper [ link ] m) Map.empty counting
  above = foldl (\m link -> Map.insertWith (flip append) link.lower [ link ] m) Map.empty counting
  linksBelow vertex = case Map.lookup vertex below of
    Just found -> found
    Nothing -> []
  linksAbove vertex = case Map.lookup vertex above of
    Just found -> found
    Nothing -> []

  rankOfVertex :: Map Vertex Int
  rankOfVertex = foldl (\soFar (Tuple r rank) -> foldl (\m vertex -> Map.insert vertex r m) soFar rank) Map.empty
    (Array.mapWithIndex Tuple given.ranks)

  pathOf vertex = case Map.lookup vertex given.clusters of
    Just found -> found
    Nothing -> []
  mustBeRightOf vertex = case Map.lookup vertex given.leftOf of
    Just found -> found
    Nothing -> Set.empty
  noClusters = all Array.null given.clusters

  -- A rank as a tree. Each cluster's vertices are gathered where the
  -- first of them stands.
  nest :: Array Vertex -> Array Item
  nest = if noClusters then map Single else foldl (\items vertex -> insert (pathOf vertex) vertex items) []
    where
    insert path vertex items = case Array.uncons path of
      Nothing -> Array.snoc items (Single vertex)
      Just { head: cluster, tail: deeper } ->
        case Array.findIndex (isGroup cluster) items of
          Just at -> case Array.modifyAt at (into deeper vertex) items of
            Just changed -> changed
            Nothing -> items
          Nothing -> Array.snoc items (Group cluster (insert deeper vertex []))
    into deeper vertex = case _ of
      Group cluster inner -> Group cluster (insert deeper vertex inner)
      single -> single
    isGroup cluster = case _ of
      Group other _ -> other == cluster
      Single _ -> false

  -- Put right the two things every order must have: each same-rank edge
  -- with its tail on the left (the leftmost vertex that is waiting for
  -- nothing goes next), and each cluster's vertices together.
  tidy :: Array Vertex -> Array Vertex
  tidy rank =
    if noClusters && Map.isEmpty given.leftOf then rank
    else Array.concatMap members
      (nest (if any (\vertex -> Map.member vertex given.leftOf) rank then inTurn rank [] else rank))
    where
    onRank = Set.fromFoldable rank
    waitsFor vertex = Array.filter (\other -> Set.member other onRank && Set.member vertex (mustBeRightOf other)) rank
    inTurn waiting placed = case Array.findIndex (\vertex -> all (\other -> Array.elem other placed) (waitsFor vertex)) waiting of
      Just at -> case Array.index waiting at, Array.deleteAt at waiting of
        Just vertex, Just rest -> inTurn rest (Array.snoc placed vertex)
        _, _ -> placed <> waiting
      Nothing -> placed <> waiting

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
      Array.mapWithIndex (\r _ -> tidy (Array.filter (\vertex -> Map.lookup vertex rankOfVertex == Just r) inOrder)) given.ranks

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
          wanted = Map.fromFoldable
            (map (\vertex -> Tuple vertex (median (Array.sort (Array.mapMaybe (\other -> Map.lookup other places) (neighbours vertex))))) rank)
          wants vertex = case Map.lookup vertex wanted of
            Just found -> found
            Nothing -> -1.0
        in
          case Array.updateAt r (tidy (Array.concatMap members (settle wants (nest rank)))) soFar of
            Just updated -> updated
            Nothing -> soFar

    -- Sort the things at one level by where they want to be, and then
    -- each cluster's own things the same way. A cluster wants to be at
    -- the mean of where its vertices want to be. A thing that wants
    -- nothing stays where it is; the others are sorted into the places
    -- that remain.
    settle :: (Vertex -> Number) -> Array Item -> Array Item
    settle wants items = (foldl refill { placed: [], waiting: movable } keyed).placed
      where
      keyed = map (\item -> { item: within item, median: want item }) items
      within = case _ of
        Group cluster inner -> Group cluster (settle wants inner)
        single -> single
      want item = case Array.filter (_ >= 0.0) (map wants (members item)) of
        [] -> -1.0
        found -> sum found / toNumber (Array.length found)
      movable = Array.sortBy (comparing _.median) ((if flipTies then Array.reverse else identity) (Array.filter (_.median >>> (_ >= 0.0)) keyed))
      refill state entry =
        if entry.median < 0.0 then state { placed = Array.snoc state.placed entry.item }
        else case Array.uncons state.waiting of
          Just next -> { placed: Array.snoc state.placed next.head.item, waiting: next.tail }
          Nothing -> state

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
  -- rank, until a whole pass removes none. Neighbours are the things at
  -- one level of the rank's tree, so a cluster is swapped whole, and two
  -- things are never swapped against a same-rank edge. On some rounds
  -- two neighbours are also swapped when it makes no difference (and
  -- they have crossings between them), to shake the order out of a rut.
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
          ends item = Array.concatMap
            (\vertex -> Array.mapMaybe (\link -> { place: _, count: link.count } <$> Map.lookup link.upper places) (linksAbove vertex))
            (members item)
          endsBelow item = Array.concatMap
            (\vertex -> Array.mapMaybe (\link -> { place: _, count: link.count } <$> Map.lookup link.lower places) (linksBelow vertex))
            (members item)
          -- Crossings between the links of two things when the first is
          -- on the left.
          crossed leftEnds rightEnds = sum (map (\a -> sum (map (\b -> if a.place > b.place then a.count * b.count else 0) rightEnds)) leftEnds)
          cost left right = crossed (ends left) (ends right) + crossed (endsBelow left) (endsBelow right)
          held left right = any (\l -> any (\r' -> Set.member r' (mustBeRightOf l)) (members right)) (members left)

          level items =
            let
              step acc item = case Array.unsnoc acc.done of
                Just { init, last } ->
                  let
                    asIs = cost last item
                    other = cost item last
                  in
                    if not (held last item) && (other < asIs || (swapTies && asIs > 0 && other == asIs)) then
                      { done: Array.snoc (Array.snoc init item) last, removed: acc.removed + (asIs - other), moved: true }
                    else acc { done = Array.snoc acc.done item }
                Nothing -> acc { done = Array.snoc acc.done item }
              here = foldl step { done: [], removed: 0, moved: false } items
              deeper = map
                ( case _ of
                    Group cluster inner ->
                      let
                        found = level inner
                      in
                        { item: Group cluster found.items, removed: found.removed, moved: found.moved }
                    single -> { item: single, removed: 0, moved: false }
                )
                here.done
            in
              { items: map _.item deeper
              , removed: here.removed + sum (map _.removed deeper)
              , moved: here.moved || any _.moved deeper
              }
          swapped = level (nest rank)
        in
          if swapped.moved then
            { ranks: case Array.updateAt r (Array.concatMap members swapped.items) state.ranks of
                Just updated -> updated
                Nothing -> state.ranks
            , removed: state.removed + swapped.removed
            }
          else state
