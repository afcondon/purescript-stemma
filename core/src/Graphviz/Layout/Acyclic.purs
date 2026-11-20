-- | Choosing which edges of a cycle to turn round, the way `dot` chooses.
-- |
-- | Any choice that leaves no cycle gives a valid ranking, but the choice
-- | shows: a reversed edge is drawn pointing up the page. `dot` chooses by
-- | walking the graph depth first and turning round each edge it finds
-- | leading back to a node the walk is still inside. Which edges that is
-- | depends on where the walk starts and what order it takes edges in, so
-- | to choose as `dot` does those have to be the same too:
-- |
-- |   - each node's outgoing edges are taken in the order of the nodes
-- |     they go to (the order the file first mentions those), and several
-- |     edges between the same two nodes count once;
-- |   - the walk's starting points are tried in the order a first sweep
-- |     reaches the nodes: from each node in turn, following its outgoing
-- |     edges in order and then its incoming ones (`searchOrder`);
-- |   - an edge turned round joins the end of its new tail's edges, and
-- |     the gap it leaves is filled by the last edge of its old tail's.
-- |
-- | These are Graphviz's rules, found by experiment against `dot`.
module Graphviz.Layout.Acyclic
  ( Fast
  , empty
  , connect
  , turn
  , outOf
  , intoOf
  , searchOrder
  , breakCycles
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | A directed graph with at most one edge each way between two nodes,
-- | and each node's edges in the order they were added.
newtype Fast v = Fast { out :: Map v (Array v), into :: Map v (Array v) }

empty :: forall v. Fast v
empty = Fast { out: Map.empty, into: Map.empty }

outOf :: forall v. Ord v => v -> Fast v -> Array v
outOf node (Fast fast) = case Map.lookup node fast.out of
  Just found -> found
  Nothing -> []

intoOf :: forall v. Ord v => v -> Fast v -> Array v
intoOf node (Fast fast) = case Map.lookup node fast.into of
  Just found -> found
  Nothing -> []

-- | Add an edge, unless there is one already.
connect :: forall v. Ord v => v -> v -> Fast v -> Fast v
connect tail head whole@(Fast fast) =
  if Array.elem head (outOf tail whole) then whole
  else Fast
    { out: Map.insert tail (Array.snoc (outOf tail whole) head) fast.out
    , into: Map.insert head (Array.snoc (intoOf head whole) tail) fast.into
    }

-- | Turn an edge round: take it away and add its opposite. Where it is
-- | taken from, the last edge in that list moves into its place.
turn :: forall v. Ord v => v -> v -> Fast v -> Fast v
turn tail head whole@(Fast fast) = connect head tail
  ( Fast
      { out: Map.insert tail (without head (outOf tail whole)) fast.out
      , into: Map.insert head (without tail (intoOf head whole)) fast.into
      }
  )

-- A list with one entry gone and the last entry moved into the gap. This
-- is how dot takes an edge out of a list, and it decides what the walk
-- looks at next: with a node's edges going to a, b and c, turning the
-- first round leaves c, b, so c is followed before b.
without :: forall v. Eq v => v -> Array v -> Array v
without gone list = case Array.elemIndex gone list, Array.unsnoc list of
  Just position, Just { init, last } ->
    if position == Array.length init then init
    else case Array.updateAt position last init of
      Just moved -> moved
      Nothing -> init
  _, _ -> list

-- | The order a sweep reaches the nodes in: from each of the given nodes
-- | in turn that has not been reached, take a node, then the nodes its
-- | outgoing edges lead to, in order, then those its incoming edges come
-- | from, each of them the same way before the next.
searchOrder :: forall v. Ord v => Array v -> Fast v -> Array v
searchOrder starts fast = Array.fromFoldable (List.reverse (foldl from { reached: Set.empty, newestFirst: Nil } starts).newestFirst)
  where
  from state start = sweep (Cons start Nil) state

  sweep pending state = case pending of
    Nil -> state
    Cons node rest ->
      if Set.member node state.reached then sweep rest state
      else sweep
        (List.fromFoldable (outOf node fast) <> List.fromFoldable (intoOf node fast) <> rest)
        { reached: Set.insert node state.reached, newestFirst: Cons node state.newestFirst }

type Walk v = { fast :: Fast v, done :: Set v, inside :: Set v, turned :: Set (Tuple v v) }

-- | Walk depth first from each of the given nodes in turn, turning round
-- | every edge found leading back to a node the walk is still inside. The
-- | answer is the edges that were turned, as they were before.
breakCycles :: forall v. Ord v => Array v -> Fast v -> Set (Tuple v v)
breakCycles starts fast = (foldl visit { fast, done: Set.empty, inside: Set.empty, turned: Set.empty } starts).turned
  where
  visit :: Walk v -> v -> Walk v
  visit walk node =
    if Set.member node walk.done then walk
    else
      let
        after = along node 0 walk { done = Set.insert node walk.done, inside = Set.insert node walk.inside }
      in
        after { inside = Set.delete node after.inside }

  -- The node's edges are read afresh each time, by position, because
  -- turning one round takes it out of the list, and edges turned round
  -- further down the walk may have joined the end.
  along :: v -> Int -> Walk v -> Walk v
  along node position walk = case Array.index (outOf node walk.fast) position of
    Nothing -> walk
    Just next ->
      if Set.member next walk.inside then
        along node position walk { fast = turn node next walk.fast, turned = Set.insert (Tuple node next) walk.turned }
      else along node (position + 1) (visit walk next)
