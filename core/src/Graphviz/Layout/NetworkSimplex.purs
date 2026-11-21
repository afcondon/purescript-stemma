-- | The optimisation at the heart of `dot`: give every variable an integer
-- | so that each requirement `value head - value tail >= minimum` holds and
-- | the sum of `weight * (value head - value tail)` is as small as it can
-- | be.
-- |
-- | `dot` uses this twice: to choose each node's rank, and later to choose
-- | its x coordinate. The method is the network simplex of Gansner,
-- | Koutsofios, North and Vo, "A Technique for Drawing Directed Graphs"
-- | (IEEE Transactions on Software Engineering, 1993), section 2.3:
-- |
-- |   1. Start from any assignment that satisfies the requirements.
-- |   2. Find a spanning tree of requirements that are tight (hold with
-- |      equality), shifting variables until one exists.
-- |   3. Each tree edge splits the variables in two. Its cut value is the
-- |      weight crossing the split in its own direction, less the weight
-- |      crossing the other way. A negative cut value means the total would
-- |      fall if that edge were allowed to stretch.
-- |   4. Swap such an edge for the tightest requirement crossing the same
-- |      split the other way, and repeat until no cut value is negative.
-- |
-- | Each round works out every cut value afresh, in one pass up the tree,
-- | so a round costs about as much as reading the requirements once. The
-- | faster method of the paper, which repairs only the cut values a swap
-- | changes, is not here yet. The minimum reached is the same whatever
-- | order it pivots in.
-- |
-- | It works over any type of variable that can be ordered, so the caller
-- | passes its own names and nothing has to be numbered.
module Graphviz.Layout.NetworkSimplex
  ( Requirement
  , Assignment
  , value
  , assigned
  , Solution
  , solve
  , cost
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldl, minimum, minimumBy, sum)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

type Requirement v = { tail :: v, head :: v, weight :: Int, minimum :: Int }

-- | An integer for every variable. It is a total function: a variable it
-- | has no entry for is 0, which is also the right answer for a variable
-- | no requirement mentions.
newtype Assignment v = Assignment (Map v Int)

value :: forall v. Ord v => Assignment v -> v -> Int
value (Assignment values) variable = case Map.lookup variable values of
  Just found -> found
  Nothing -> 0

-- | The variables that have an entry, with their values.
assigned :: forall v. Assignment v -> Array (Tuple v Int)
assigned (Assignment values) = Map.toUnfoldable values

-- | What is being minimised.
cost :: forall v. Ord v => Array (Requirement v) -> Assignment v -> Int
cost requirements assignment = sum (map (\r -> r.weight * (value assignment r.head - value assignment r.tail)) requirements)

-- | An assignment, and the spanning tree of tight requirements it was read
-- | off, as places in the array of requirements given. The tree is the
-- | proof that the assignment is optimal: anyone can check that its
-- | requirements are tight, that it spans each group of joined variables,
-- | and that none of its cut values is negative, without trusting this
-- | module.
type Solution v = { assignment :: Assignment v, tree :: Array Int }

-- A requirement with its place in the array the caller gave.
type Placed v = { place :: Int, tail :: v, head :: v, weight :: Int, minimum :: Int }

-- A requirement as seen from one of its two variables.
type Link v = { requirement :: Placed v, other :: v }

-- | An optimal assignment. The requirements must have no cycle. Variables
-- | that no chain of requirements joins are solved apart, and the smallest
-- | value in each such group is 0.
solve :: forall v. Ord v => Array (Requirement v) -> Solution v
solve given = foldl include { assignment: Assignment Map.empty, tree: [] } (groups placed)
  where
  -- A requirement from a variable to itself says nothing.
  placed = Array.filter (\r -> r.tail /= r.head)
    (Array.mapWithIndex (\place r -> { place, tail: r.tail, head: r.head, weight: r.weight, minimum: r.minimum }) given)

  include soFar group =
    { assignment: Assignment (Map.union (normalised solved.values) (unwrap soFar.assignment))
    , tree: soFar.tree <> map _.place solved.tree
    }
    where
    solved = optimise group

  unwrap (Assignment values) = values

  normalised (Assignment values) = case minimum values of
    Just lowest -> map (_ - lowest) values
    Nothing -> values

-- For each variable, the requirements that touch it.
links :: forall v. Ord v => Array (Placed v) -> Map v (Array (Link v))
links = foldl add Map.empty
  where
  add soFar r =
    Map.insertWith (flip append) r.head [ { requirement: r, other: r.tail } ]
      (Map.insertWith (flip append) r.tail [ { requirement: r, other: r.head } ] soFar)

linksOf :: forall v. Ord v => Map v (Array (Link v)) -> v -> Array (Link v)
linksOf neighbours variable = case Map.lookup variable neighbours of
  Just found -> found
  Nothing -> []

-- The requirements, split into groups that share no variable.
groups :: forall v. Ord v => Array (Placed v) -> Array (Array (Placed v))
groups requirements = (foldl start { seen: Set.empty, found: [] } requirements).found
  where
  neighbours = links requirements

  start state r =
    if Set.member r.tail state.seen then state
    else
      let
        reached = spread (Cons r.tail Nil) (Set.singleton r.tail) Map.empty
      in
        { seen: Set.union reached.variables state.seen
        , found: Array.snoc state.found (Array.fromFoldable (Map.values reached.requirements))
        }

  spread pending variables found = case pending of
    Nil -> { variables, requirements: found }
    Cons variable rest ->
      let
        here = linksOf neighbours variable
        fresh = Array.filter (\link -> not (Set.member link.other variables)) here
      in
        spread (foldl (\p link -> Cons link.other p) rest fresh)
          (foldl (\v link -> Set.insert link.other v) variables fresh)
          (foldl (\f link -> Map.insert link.requirement.place link.requirement f) found here)

slack :: forall v. Ord v => Assignment v -> Placed v -> Int
slack assignment r = value assignment r.head - value assignment r.tail - r.minimum

shift :: forall f v. Foldable f => Ord v => Int -> f v -> Assignment v -> Assignment v
shift by variables (Assignment values) =
  Assignment (foldl (\soFar variable -> Map.insertWith add variable by soFar) values variables)

-- Step 1: every variable as early as its requirements allow. Each is
-- placed once all the requirements into it have been placed.
-- Every variable of the group gets an entry, the ones at 0 included, so
-- that moving the whole group later moves all of it.
earliest :: forall v. Ord v => Array (Placed v) -> Assignment v
earliest requirements = place ready (Assignment (Map.fromFoldable (map (\variable -> Tuple variable 0) ready))) waiting
  where
  outgoing = foldl (\m r -> Map.insertWith (flip append) r.tail [ r ] m) Map.empty requirements
  -- How many requirements into each variable are still to be placed.
  waiting = foldl (\m r -> Map.insertWith add r.head 1 (Map.insertWith add r.tail 0 m)) Map.empty requirements
  ready = List.fromFoldable (Map.keys (Map.filter (_ == 0) waiting))

  place pending assignment counts = case pending of
    Nil -> assignment
    Cons variable rest ->
      let
        after = foldl (release assignment variable) { pending: rest, assignment, counts } (case Map.lookup variable outgoing of
          Just found -> found
          Nothing -> [])
      in
        place after.pending after.assignment after.counts

  -- A requirement out of a placed variable pushes its head down, and may
  -- have been the last one the head was waiting for.
  release placedSoFar variable state r =
    let
      Assignment values = state.assignment
      pushed = Assignment (Map.insertWith max r.head (value placedSoFar variable + r.minimum) values)
      counts = Map.update (\left -> Just (left - 1)) r.head state.counts
    in
      { pending: if Map.lookup r.head counts == Just 0 then Cons r.head state.pending else state.pending
      , assignment: pushed
      , counts
      }

-- Steps 2 to 4, for one group.
optimise :: forall v. Ord v => Array (Placed v) -> { values :: Assignment v, tree :: Array (Placed v) }
optimise requirements = case Array.head requirements of
  Nothing -> { values: Assignment Map.empty, tree: [] }
  Just first ->
    let
      feasible = tightTree first.tail (earliest requirements)
      settled = pivot first.tail limit feasible.tree feasible.values
    in
      { values: settled.values, tree: Array.fromFoldable (Map.values settled.tree) }
  where
  neighbours = links requirements
  size = Set.size (foldl (\s r -> Set.insert r.head (Set.insert r.tail s)) Set.empty requirements)
  -- Far more rounds than any graph should need; a guard against going
  -- round in circles, which the caller's check of the tree would catch.
  limit = 20 + 4 * size * Array.length requirements

  -- What each variable adds to a cut it is on the tail side of: the weight
  -- of the requirements out of it, less the weight of those into it.
  net :: Map v Int
  net = foldl (\m r -> Map.insertWith add r.tail r.weight (Map.insertWith add r.head (negate r.weight) m)) Map.empty requirements

  -- Step 2. Grow a tree of tight requirements from one variable; when it
  -- stops short, move the whole tree up to the nearest requirement that
  -- leaves it and carry on from the variable that requirement reaches.
  --
  -- Moving the tree does not touch its variables. `moved` is how far the
  -- tree has travelled in all, and each variable remembers how far it had
  -- travelled when that variable joined, so where a variable now stands is
  -- its first value plus the difference.
  tightTree :: v -> Assignment v -> { tree :: Map Int (Placed v), values :: Assignment v }
  tightTree first (Assignment start) = extend 0 (grow 0 (Cons first Nil) { joined: Map.singleton first 0, tree: Map.empty })
    where
    stands moved joined variable = case Map.lookup variable joined of
      Just since -> value (Assignment start) variable + moved - since
      Nothing -> value (Assignment start) variable

    gap moved joined r = stands moved joined r.head - stands moved joined r.tail - r.minimum

    extend moved grown =
      if Map.size grown.joined == size then finished
      else case minimumBy (comparing (gap moved grown.joined)) (Array.filter (leaves grown.joined) requirements) of
        Nothing -> finished
        Just nearest ->
          let
            fromTree = Map.member nearest.tail grown.joined
            further = if fromTree then moved + gap moved grown.joined nearest else moved - gap moved grown.joined nearest
            reached = if fromTree then nearest.head else nearest.tail
          in
            extend further
              ( grow further (Cons reached Nil)
                  { joined: Map.insert reached further grown.joined
                  , tree: Map.insert nearest.place nearest grown.tree
                  }
              )
      where
      finished =
        { tree: grown.tree
        , values: Assignment (Map.mapMaybeWithKey (\variable _ -> Just (stands moved grown.joined variable)) start)
        }

    grow moved pending grown = case pending of
      Nil -> grown
      Cons variable rest ->
        let
          step state link =
            if Map.member link.other state.grown.joined || gap moved state.grown.joined link.requirement /= 0 then state
            else
              { pending: Cons link.other state.pending
              , grown:
                  { joined: Map.insert link.other moved state.grown.joined
                  , tree: Map.insert link.requirement.place link.requirement state.grown.tree
                  }
              }
          after = foldl step { pending: rest, grown } (linksOf neighbours variable)
        in
          grow moved after.pending after.grown

  leaves joined r = Map.member r.tail joined /= Map.member r.head joined

  -- The tree hung from one variable. `order` lists the variables so that
  -- each comes after everything below it, `number` counts them from the
  -- top so that the variables below any one are a run of numbers, and
  -- `above` is the tree edge from each variable towards the top.
  hung :: v -> Map Int (Placed v) -> { order :: List v, number :: Map v Int, above :: Map v (Placed v) }
  hung top tree = walk (Cons { variable: top, by: Nothing } Nil) { order: Nil, number: Map.empty, above: Map.empty }
    where
    walk pending state = case pending of
      Nil -> state
      Cons { variable, by } rest ->
        let
          numbered =
            { order: Cons variable state.order
            , number: Map.insert variable (Map.size state.number) state.number
            , above: case by of
                Just edge -> Map.insert variable edge state.above
                Nothing -> state.above
            }
          below = Array.filter
            (\link -> Map.member link.requirement.place tree && not (Map.member link.other numbered.number))
            (linksOf neighbours variable)
        in
          walk (foldl (\p link -> Cons { variable: link.other, by: Just link.requirement } p) rest below) numbered

  -- Step 3, for every tree edge in one pass from the bottom up. The cut
  -- value of the edge above a variable is the sum of `net` over that
  -- variable and everything below it, with the sign turned when the edge
  -- points down into them. Of the edges with a negative cut value, the one
  -- given first by the caller is returned, with the variable under it and
  -- how many variables that makes.
  firstNegative :: forall r. { order :: List v, above :: Map v (Placed v) | r } -> Maybe { edge :: Placed v, under :: v, count :: Int }
  firstNegative tree = (foldl visit { carried: Map.empty, found: Nothing } tree.order).found
    where
    visit state variable =
      let
        mine = case Map.lookup variable state.carried of
          Just below -> { total: below.total + own, count: below.count + 1 }
          Nothing -> { total: own, count: 1 }
        own = case Map.lookup variable net of
          Just found -> found
          Nothing -> 0
      in
        case Map.lookup variable tree.above of
          Nothing -> state
          Just edge ->
            let
              parent = if edge.tail == variable then edge.head else edge.tail
              cut = if edge.tail == variable then mine.total else negate mine.total
              better = case state.found of
                Just kept -> edge.place < kept.edge.place
                Nothing -> true
            in
              { carried: Map.insertWith (\old new -> { total: old.total + new.total, count: old.count + new.count }) parent mine state.carried
              , found: if cut < 0 && better then Just { edge, under: variable, count: mine.count } else state.found
              }

  -- Step 4.
  pivot :: v -> Int -> Map Int (Placed v) -> Assignment v -> { values :: Assignment v, tree :: Map Int (Placed v) }
  pivot top rounds tree values =
    if rounds <= 0 then { values, tree }
    else case firstNegative shape of
      Nothing -> { values, tree }
      Just leaving ->
        let
          from = numberOf leaving.under
          below variable = numberOf variable >= from && numberOf variable < from + leaving.count
          -- The leaving edge's tail may be the variable under it or the
          -- one over it.
          tailIsBelow = leaving.edge.tail == leaving.under
          tailSide variable = below variable == tailIsBelow
        in
          case minimumBy (comparing (slack values)) (Array.filter (\r -> not (tailSide r.tail) && tailSide r.head) requirements) of
            Nothing -> { values, tree }
            Just entering ->
              pivot top (rounds - 1) (Map.insert entering.place entering (Map.delete leaving.edge.place tree))
                ( shift (if tailIsBelow then negate (slack values entering) else slack values entering)
                    (Array.filter below (Array.fromFoldable shape.order))
                    values
                )
    where
    shape = hung top tree
    numberOf variable = case Map.lookup variable shape.number of
      Just found -> found
      Nothing -> -1
