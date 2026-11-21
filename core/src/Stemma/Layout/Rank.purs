-- | The first pass of `dot`: which rank (layer) each node is on.
-- |
-- | Every edge asks for its head to be at least `minlen` ranks below its
-- | tail, and pulls the two together with a strength of `weight`. Where the
-- | edges form a cycle some must be turned round first, since no ranking
-- | can put every head below its tail. What is left is exactly the problem
-- | `Stemma.Layout.NetworkSimplex` solves.
-- |
-- | An edge with `constraint=false` asks for nothing, and so does an edge
-- | from a node to itself. If any edge has a label, every `minlen` is
-- | doubled, to leave a rank for the label to sit in.
-- |
-- | A subgraph can ask for more (its `rank` attribute):
-- |
-- |   - `same`: its nodes share a rank. They are ranked as one thing, and
-- |     an edge between two of them asks for nothing. Sets that share a
-- |     node are one set.
-- |   - `min`: its nodes are on the top rank; `source`: and alone there.
-- |     All such subgraphs together make one set, `source` if any is.
-- |   - `max` and `sink`: the same at the bottom.
-- |
-- | The top set is made the top by turning round every edge that comes
-- | into it, and by asking everything that then has nothing above it to be
-- | below the set (at least one rank below, for `source`), with no weight.
-- |
-- | A cluster is ranked on its own first, from the edges inside it, and
-- | then moves as one rigid block: the graph around it sees a single
-- | thing, and each node of the cluster keeps its place within the block.
-- | An edge that crosses into or out of a cluster is not a requirement.
-- | It is a preference: it pulls as a normal edge does while it points the
-- | right way and is long enough, and when it cannot be, it is allowed to
-- | fail at ten times its weight for every rank it is short. That is done
-- | with one extra variable per such edge (see `soft`), and it is why
-- | cycles through clusters never need an edge turned round.
-- |
-- | Clusters nest, so the ranking is a tree of problems, one per level:
-- | the graph, each cluster, each cluster inside a cluster. A node in two
-- | clusters side by side belongs to the first.
-- |
-- | These rules are Graphviz's, found by experiment against `dot`.
module Stemma.Layout.Rank
  ( Rank(..)
  , Ranking
  , rankOf
  , Pull
  , Variable(..)
  , Placed
  , Problem
  , Solved(..)
  , Unsupported(..)
  , describe
  , pulls
  , rank
  , rankGiven
  , ranking
  , reversed
  , levels
  , clusterPaths
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldl, minimum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Stemma.Attributes (RankKind(..))
import Stemma.Attributes as Attributes
import Stemma.Graph (EdgeId, Graph, NodeName, Subgraph, SubgraphId)
import Stemma.Graph as Graph
import Stemma.Layout.Acyclic as Acyclic
import Stemma.Layout.NetworkSimplex (Assignment, Requirement, solve, value)

-- | The top rank is 0.
newtype Rank = Rank Int

derive newtype instance eqRank :: Eq Rank
derive newtype instance ordRank :: Ord Rank

-- | A rank for every node of the graph it was made for.
newtype Ranking = Ranking (Map NodeName Rank)

-- | The rank of a node. A node of some other graph is on rank 0.
rankOf :: Ranking -> NodeName -> Rank
rankOf (Ranking ranks) node = case Map.lookup node ranks of
  Just found -> found
  Nothing -> Rank 0

-- | What one edge asks of the ranking, as written: its head at least
-- | `minimum` ranks below its tail, pulled together with `weight`.
type Pull = { edge :: EdgeId, tail :: NodeName, head :: NodeName, weight :: Int, minimum :: Int }

-- | What the solver assigns a number to at one level:
-- |
-- |   - a set of nodes ranked together, named by one of them;
-- |   - a cluster inside this level, which moves as a block;
-- |   - the slack of an edge that crosses a cluster's edge (see `soft`).
data Variable = Together NodeName | Block SubgraphId | Slack EdgeId

derive instance eqVariable :: Eq Variable
derive instance ordVariable :: Ord Variable

-- | Where a node is: the variable it moves with, and how many ranks below
-- | that variable's number it sits (0 unless it is inside a cluster).
type Placed = { variable :: Variable, offset :: Int }

-- | One level of the ranking as an optimisation: what must hold between
-- | the variables, where each of the level's nodes is, and which edges
-- | were turned round on the way. `cluster` is `Nothing` for the graph.
type Problem =
  { cluster :: Maybe SubgraphId
  , requirements :: Array (Requirement Variable)
  , place :: Map NodeName Placed
  , reversed :: Array EdgeId
  }

-- | A level's answer: the number each variable got, the tree of
-- | requirements (places in `requirements`) that proves nothing costs
-- | less, the rank of each of the level's nodes counting from the level's
-- | own top, and the answers for the clusters inside it.
newtype Solved = Solved
  { problem :: Problem
  , values :: Assignment Variable
  , tree :: Array Int
  , relative :: Map NodeName Int
  , inside :: Array Solved
  }

-- | The ranking of the whole graph, from the answer for its top level.
ranking :: Solved -> Ranking
ranking (Solved solved) = Ranking (map Rank solved.relative)

-- | Every edge that was turned round, at any level.
reversed :: Solved -> Array EdgeId
reversed (Solved solved) = solved.problem.reversed <> Array.concatMap reversed solved.inside

-- | The top level and every level inside it.
levels :: Solved -> Array Solved
levels whole@(Solved solved) = [ whole ] <> Array.concatMap levels solved.inside

-- | Features of `dot` that change the ranking and are not written here yet.
data Unsupported = SetAcrossCluster | TopIsBottom | NewRank | SolverLimits

derive instance eqUnsupported :: Eq Unsupported

describe :: Unsupported -> String
describe = case _ of
  SetAcrossCluster -> "a rank=same (or min, max, source, sink) subgraph holds a node that is inside a cluster beside it"
  TopIsBottom -> "a node is asked to be on both the top rank and the bottom one (dot fails on this too)"
  NewRank -> "it sets newrank"
  SolverLimits -> "it limits the network simplex (nslimit, nslimit1 or searchsize)"

-- | What each edge asks, for the edges that ask anything.
pulls :: Graph -> Array Pull
pulls graph = Array.mapMaybe pull (Graph.edges graph)
  where
  doubled = if any (_.attributes >>> Attributes.hasLabel) (Graph.edges graph) then 2 else 1
  pull edge =
    if edge.tail == edge.head || not (Attributes.constrains edge.attributes) then Nothing
    else Just
      { edge: edge.id
      , tail: edge.tail
      , head: edge.head
      , weight: Attributes.weight edge.attributes
      , minimum: doubled * Attributes.minimumLength edge.attributes
      }

-- | The ranking, with the edges of each cycle turned round as a
-- | depth-first walk finds them: nodes in order, each node's edges in
-- | order, and any edge that leads back to something the walk is still
-- | inside is reversed.
rank :: Graph -> Either Unsupported Solved
rank = rankWith Nothing

-- | The ranking worked out level by level as another ranking of the same
-- | graph implies: the same edges turned round (those whose head it puts
-- | above their tail), and each cluster laid out inside as it has it. This
-- | is for costing that other ranking against the best one at each level.
rankGiven :: Map NodeName Rank -> Graph -> Either Unsupported Solved
rankGiven given = rankWith (Just given)

rankWith :: Maybe (Map NodeName Rank) -> Graph -> Either Unsupported Solved
rankWith given graph =
  if isSet "newrank" then Left NewRank
  else if isSet "nslimit" || isSet "nslimit1" || isSet "searchsize" then Left SolverLimits
  else solveLevel given (pulls graph) (outline graph)
  where
  isSet key = Attributes.isSet key (Graph.attributes graph)

-- A level before anything is solved: the graph or a cluster, the nodes
-- that are its own (in the graph's order), the clusters directly inside
-- it, and the subgraphs at this level that ask for ranks.
newtype Level = Level
  { cluster :: Maybe SubgraphId
  , nodes :: Array NodeName
  , inside :: Array Level
  , asking :: Array { kind :: RankKind, nodes :: Array NodeName }
  }

outline :: Graph -> Level
outline graph = level Nothing (map _.name (Graph.nodes graph))
  where
  -- The nearest cluster around each subgraph.
  around :: Subgraph -> Maybe SubgraphId
  around = map _.id <<< Graph.enclosingCluster graph

  level :: Maybe SubgraphId -> Array NodeName -> Level
  level here nodes = Level
    { cluster: here
    , nodes
    , inside: (foldl claim { taken: Set.empty, found: [] } (Array.filter (\c -> around c == here) (Graph.clusters graph))).found
    , asking: Array.mapMaybe (\subgraph -> { kind: _, nodes: subgraph.nodes } <$> Attributes.rankKind subgraph.attributes)
        (Array.filter (\subgraph -> not subgraph.cluster && around subgraph == here) (Graph.subgraphs graph))
    }
    where
    mine = Set.fromFoldable nodes
    -- A cluster takes those of its nodes that are this level's and that
    -- no cluster before it took.
    claim state cluster =
      let
        members = Set.fromFoldable cluster.nodes
        taking = Array.filter (\node -> Set.member node members && not (Set.member node state.taken)) nodes
      in
        { taken: foldl (flip Set.insert) state.taken taking
        , found: Array.snoc state.found (level (Just cluster.id) (Array.filter (\node -> Set.member node mine) taking))
        }

-- | The clusters each node is in, outermost first: none for a node that is
-- | in no cluster. A node that several clusters side by side all name is
-- | in the first of them only.
clusterPaths :: Graph -> Map NodeName (Array SubgraphId)
clusterPaths graph = within [] (outline graph) Map.empty
  where
  within path (Level level) soFar = foldl
    ( \found below@(Level inner) -> case inner.cluster of
        Just id -> within (Array.snoc path id) below found
        Nothing -> found
    )
    (foldl (\found node -> Map.insert node path found) soFar level.nodes)
    level.inside

-- Which of a level's loose nodes are ranked together, and the two sets
-- that are pinned.
type Sets =
  { together :: Map NodeName Variable
  , top :: Maybe { set :: Variable, alone :: Boolean }
  , bottom :: Maybe { set :: Variable, alone :: Boolean }
  }

sets :: Array NodeName -> Array { kind :: RankKind, nodes :: Array NodeName } -> Either Unsupported Sets
sets loose asking =
  if any (_.nodes >>> any (\node -> not (Map.member node alone))) asking then Left SetAcrossCluster
  else if isJust top && map _.set top == map _.set bottom then Left TopIsBottom
  else Right { together, top, bottom }
  where
  ofKind kinds = Array.concatMap _.nodes (Array.filter (\s -> Array.elem s.kind kinds) asking)
  tops = ofKind [ Minimum, Source ]
  bottoms = ofKind [ Maximum, Sink ]

  -- Every node starts alone; each asking subgraph joins its nodes, and all
  -- the top ones are joined, and all the bottom ones.
  alone = Map.fromFoldable (map (\node -> Tuple node (Together node)) loose)
  together = foldl join alone (map _.nodes asking <> [ tops, bottoms ])

  join :: Map NodeName Variable -> Array NodeName -> Map NodeName Variable
  join soFar nodes = case Array.uncons (Array.mapMaybe (\node -> Map.lookup node soFar) nodes) of
    Nothing -> soFar
    Just { head: kept, tail: others } ->
      let
        merged = Set.fromFoldable others
      in
        map (\variable -> if Set.member variable merged then kept else variable) soFar

  pinned nodes kinds = Array.head nodes >>= \node -> Map.lookup node together <#> \set ->
    { set, alone: any (\s -> Array.elem s.kind kinds) asking }
  top = pinned tops [ Source ]
  bottom = pinned bottoms [ Sink ]

-- A pull between two of a level's variables, with where its two nodes sit
-- relative to them.
type Between =
  { edge :: EdgeId
  , tail :: Placed
  , head :: Placed
  , weight :: Int
  , minimum :: Int
  }

flipped :: Between -> Between
flipped pull = pull { tail = pull.head, head = pull.tail }

isBlock :: Variable -> Boolean
isBlock = case _ of
  Block _ -> true
  _ -> false

-- An edge with an end inside a cluster of this level, as a preference.
--
-- Say the edge wants its head's variable at least `gap` below its tail's
-- (its `minlen`, adjusted for where the two nodes sit inside their
-- clusters). A new variable, the slack, is put no lower than the tail's
-- and at least `gap` above the head's (or the other way about if `gap`
-- is negative), and pulled towards the tail ten times as hard as towards
-- the head. While the edge is satisfied the slack sits on the tail and
-- the cost is the edge's own, weight times length. When it is not, the
-- slack is dragged above the tail and every rank of that costs ten times
-- the weight.
soft :: Between -> Array (Requirement Variable)
soft pull =
  [ { tail: Slack pull.edge, head: pull.tail.variable, weight: 10 * pull.weight, minimum: if gap > 0 then 0 else negate gap }
  , { tail: Slack pull.edge, head: pull.head.variable, weight: pull.weight, minimum: if gap > 0 then gap else 0 }
  ]
  where
  gap = pull.minimum + pull.tail.offset - pull.head.offset

solveLevel :: Maybe (Map NodeName Rank) -> Array Pull -> Level -> Either Unsupported Solved
solveLevel given asked (Level level) = do
  inside <- traverse (\below -> Tuple below <$> solveLevel given asked below) level.inside
  let
    -- Where each cluster's nodes sit inside it: as this pass ranked it,
    -- or as the given ranking has it.
    blocks = Array.concatMap
      ( \(Tuple (Level below) (Solved solved)) -> case below.cluster of
          Nothing -> []
          Just id -> map (\(Tuple node offset) -> Tuple node { variable: Block id, offset })
            (Map.toUnfoldable (case given of
              Nothing -> solved.relative
              Just theirs -> fromTop (Map.filterKeys (\node -> Array.elem node below.nodes) (map (\(Rank r) -> r) theirs))))
      )
      inside
    inBlocks = Set.fromFoldable (map (\(Tuple node _) -> node) blocks)
    loose = Array.filter (\node -> not (Set.member node inBlocks)) level.nodes
  found <- sets loose level.asking
  let
    place = Map.union (Map.fromFoldable blocks) (map (\variable -> { variable, offset: 0 }) found.together)
    here = Array.mapMaybe
      ( \pull -> do
          tail <- Map.lookup pull.tail place
          head <- Map.lookup pull.head place
          if tail.variable == head.variable then Nothing
          else Just { edge: pull.edge, tail, head, weight: pull.weight, minimum: pull.minimum }
      )
      asked
    crossing = Array.filter (\pull -> isBlock pull.tail.variable || isBlock pull.head.variable) here
    hard = Array.filter (\pull -> not (isBlock pull.tail.variable || isBlock pull.head.variable)) here
    -- An edge into the top set, or out of the bottom one, is turned round.
    againstPin pull = Just pull.head.variable == map _.set found.top || Just pull.tail.variable == map _.set found.bottom
    turned = case given of
      Just theirs -> Set.fromFoldable (Array.mapMaybe (upward theirs) hard)
      Nothing ->
        let
          pinnedRound = Set.fromFoldable (map _.edge (Array.filter againstPin hard))
          afterPins = map (\pull -> if Set.member pull.edge pinnedRound then flipped pull else pull) hard
          -- The level as dot holds it when it looks for cycles: every
          -- variable, and the edges in the order dot makes them, which is
          -- node by node and, for each node, by the node the edge goes
          -- to (and for two edges to the same node, as the file has them).
          positions = Map.fromFoldable (Array.mapWithIndex (\position node -> Tuple node position) level.nodes)
          inFileOrder = Array.sortWith (\pull -> Tuple (Map.lookup pull.tail positions) (Tuple (Map.lookup pull.head positions) pull.edge))
            (Array.filter (\pull -> Map.member pull.tail place && Map.member pull.head place) asked)
          asMade = foldl
            ( \fast pull -> case Map.lookup pull.tail place, Map.lookup pull.head place of
                Just tail, Just head
                  | tail.variable == head.variable -> fast
                  | isBlock tail.variable || isBlock head.variable ->
                      Acyclic.connect (Slack pull.edge) head.variable (Acyclic.connect (Slack pull.edge) tail.variable fast)
                  | otherwise -> Acyclic.connect tail.variable head.variable fast
                _, _ -> fast
            )
            Acyclic.empty
            inFileOrder
          -- Then the pins: every edge out of the bottom set and every edge
          -- into the top set is turned round, in order.
          bottomPinned = case found.bottom of
            Just bottom -> foldl (\fast below -> Acyclic.turn bottom.set below fast) asMade (Acyclic.outOf bottom.set asMade)
            Nothing -> asMade
          pinned = case found.top of
            Just top -> foldl (\fast above -> Acyclic.turn above top.set fast) bottomPinned (Acyclic.intoOf top.set bottomPinned)
            Nothing -> bottomPinned
          starts = Array.nub (map _.variable (Array.mapMaybe (\node -> Map.lookup node place) level.nodes))
          cycles = Acyclic.breakCycles (Acyclic.searchOrder starts pinned) pinned
          inCycle = Set.fromFoldable (map _.edge (Array.filter (\pull -> Set.member (Tuple pull.tail.variable pull.head.variable) cycles) afterPins))
        in
          Set.union (Set.difference pinnedRound inCycle) (Set.difference inCycle pinnedRound)
    oriented = map (\pull -> if Set.member pull.edge turned then flipped pull else pull) hard
    fromEdges = map (\pull -> { tail: pull.tail.variable, head: pull.head.variable, weight: pull.weight, minimum: pull.minimum }) oriented
      <> Array.concatMap soft crossing
    variables = Array.nub (map _.variable (Array.mapMaybe (\node -> Map.lookup node place) level.nodes))
    hasAbove = Set.fromFoldable (map _.head fromEdges)
    hasBelow = Set.fromFoldable (map _.tail fromEdges)
    -- Whatever has nothing above it goes below the top set; whatever has
    -- nothing below it goes above the bottom set. Neither costs anything.
    belowTop = case found.top of
      Nothing -> []
      Just top -> map (\variable -> { tail: top.set, head: variable, weight: 0, minimum: if top.alone then 1 else 0 })
        (Array.filter (\variable -> variable /= top.set && not (Set.member variable hasAbove)) variables)
    aboveBottom = case found.bottom of
      Nothing -> []
      Just bottom -> map (\variable -> { tail: variable, head: bottom.set, weight: 0, minimum: if bottom.alone then 1 else 0 })
        (Array.filter (\variable -> variable /= bottom.set && not (Set.member variable hasBelow)) variables)
    requirements = fromEdges <> belowTop <> aboveBottom
    solved = solve requirements
  pure $ Solved
    { problem:
        { cluster: level.cluster
        , requirements
        , place
        , reversed: Array.filter (\edge -> Set.member edge turned) (map _.edge hard)
        }
    , values: solved.assignment
    , tree: solved.tree
    , relative: fromTop (map (\placed -> value solved.assignment placed.variable + placed.offset) place)
    , inside: map (\(Tuple _ below) -> below) inside
    }
  where
  upward theirs pull = do
    tail <- lowestOf theirs pull.tail.variable
    head <- lowestOf theirs pull.head.variable
    if head < tail then Just pull.edge else Nothing
  -- The given rank of a set of nodes ranked together: that of its name.
  lowestOf theirs = case _ of
    Together node -> Map.lookup node theirs
    _ -> Nothing

-- Ranks counted from the top one among them.
fromTop :: Map NodeName Int -> Map NodeName Int
fromTop ranks = case minimum ranks of
  Just top -> map (_ - top) ranks
  Nothing -> ranks
