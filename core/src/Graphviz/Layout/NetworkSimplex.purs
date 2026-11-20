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
-- | This is the plain version: it recomputes cut values from their
-- | definition on every round, which is slow on a large graph and easy to
-- | see is right. The minimum it reaches is the same whatever order it
-- | pivots in.
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
import Data.Foldable (foldl, minimum, minimumBy, sum)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
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

shift :: forall v. Ord v => Int -> Set v -> Assignment v -> Assignment v
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
      settled = pivot limit feasible.tree feasible.values
    in
      { values: settled.values, tree: Array.fromFoldable (Map.values settled.tree) }
  where
  neighbours = links requirements
  size = Set.size (foldl (\s r -> Set.insert r.head (Set.insert r.tail s)) Set.empty requirements)
  -- Far more rounds than any graph should need; a guard against going
  -- round in circles, which the caller's check of the tree would catch.
  limit = 20 + 4 * size * Array.length requirements

  -- Step 2. Grow a tree of tight requirements from one variable; when it
  -- stops short, move the whole tree up to the nearest requirement that
  -- leaves it and carry on.
  tightTree :: v -> Assignment v -> { tree :: Map Int (Placed v), values :: Assignment v }
  tightTree first values =
    let
      grown = grow values (Cons first Nil) (Set.singleton first) Map.empty
    in
      if Set.size grown.reached == size then { tree: grown.tree, values }
      else
        case minimumBy (comparing (slack values)) (Array.filter (leaves grown.reached) requirements) of
          Nothing -> { tree: grown.tree, values }
          Just nearest ->
            tightTree first
              ( shift (if Set.member nearest.tail grown.reached then slack values nearest else negate (slack values nearest))
                  grown.reached
                  values
              )

  leaves reached r = Set.member r.tail reached /= Set.member r.head reached

  grow :: Assignment v -> List v -> Set v -> Map Int (Placed v) -> { reached :: Set v, tree :: Map Int (Placed v) }
  grow values pending reached tree = case pending of
    Nil -> { reached, tree }
    Cons variable rest ->
      let
        step state link =
          if Set.member link.other state.reached || slack values link.requirement /= 0 then state
          else
            { pending: Cons link.other state.pending
            , reached: Set.insert link.other state.reached
            , tree: Map.insert link.requirement.place link.requirement state.tree
            }
        after = foldl step { pending: rest, reached, tree } (linksOf neighbours variable)
      in
        grow values after.pending after.reached after.tree

  -- The variables on the tail's side of a tree edge.
  tailSide :: Map Int (Placed v) -> Placed v -> Set v
  tailSide tree edge = spread (Cons edge.tail Nil) (Set.singleton edge.tail)
    where
    spread pending reached = case pending of
      Nil -> reached
      Cons variable rest ->
        let
          fresh = Array.filter
            (\link -> link.requirement.place /= edge.place && Map.member link.requirement.place tree && not (Set.member link.other reached))
            (linksOf neighbours variable)
        in
          spread (foldl (\p link -> Cons link.other p) rest fresh) (foldl (\r link -> Set.insert link.other r) reached fresh)

  -- Step 3, for one tree edge.
  cutValue :: Set v -> Int
  cutValue side = foldl crossing 0 requirements
    where
    crossing total r = case Set.member r.tail side, Set.member r.head side of
      true, false -> total + r.weight
      false, true -> total - r.weight
      _, _ -> total

  -- Step 4.
  pivot :: Int -> Map Int (Placed v) -> Assignment v -> { values :: Assignment v, tree :: Map Int (Placed v) }
  pivot rounds tree values =
    if rounds <= 0 then { values, tree }
    else case List.findMap negative (Map.values tree) of
      Nothing -> { values, tree }
      Just leaving ->
        case minimumBy (comparing (slack values)) (Array.filter (\r -> not (Set.member r.tail leaving.side) && Set.member r.head leaving.side) requirements) of
          Nothing -> { values, tree }
          Just entering ->
            pivot (rounds - 1) (Map.insert entering.place entering (Map.delete leaving.edge.place tree))
              (shift (negate (slack values entering)) leaving.side values)
    where
    negative edge =
      let
        side = tailSide tree edge
      in
        if cutValue side < 0 then Just { edge, side } else Nothing
