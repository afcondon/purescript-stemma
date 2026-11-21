-- | What a DOT file means: its nodes, edges and subgraphs, each with the
-- | attributes it ends up with.
-- |
-- | The syntax does not say this directly. Attributes are inherited from
-- | defaults that change as the file is read, and the rules for that are
-- | Graphviz's (its `cgraph` library), established here by experiment
-- | against Graphviz and held to it by `bin/check.py`:
-- |
-- |   - A node is created where it is first mentioned, and takes the node
-- |     defaults in force there, then. Defaults set later do not reach it.
-- |   - The defaults in force in a subgraph are its own, then its parent's
-- |     as they are at that moment, and so on outwards.
-- |   - A named subgraph mentioned twice is one subgraph; it keeps its
-- |     defaults and its members.
-- |   - A subgraph's own attributes start as a copy of its parent's, taken
-- |     when it is first opened.
-- |   - A node mentioned inside a subgraph is a member of it and of every
-- |     subgraph around it, wherever it was created.
-- |   - `a -> {b c}` is an edge to each node of the subgraph, as it stands
-- |     once the whole statement has been read: in `s -> subgraph s { a }`
-- |     the first `s` already has `a` in it.
-- |   - Without `strict`, two edge statements make one edge only if they
-- |     join the same nodes and give the same `key`.
-- |   - In a `strict` graph a second edge between the same nodes joins the
-- |     first and adds its attributes to it. With keys it is less tidy: a
-- |     key not seen before makes the statement do nothing if its own
-- |     subgraph already has an edge between those nodes, and makes a
-- |     second edge if it does not. See `connect`.
-- |   - When an undirected edge is merged into one written the other way
-- |     round, a port given with `:` goes to the end it names, not to the
-- |     side of the `--` it was written on.
-- |
-- | A `Graph` can only be made by `fromDot`, so what it promises holds:
-- | every edge joins two of its nodes, and every member of a subgraph is
-- | one of its nodes or edges.
module Stemma.Graph
  ( Graph
  , NodeName(..)
  , EdgeId(..)
  , SubgraphId(..)
  , Node
  , Edge
  , Subgraph
  , fromDot
  , name
  , isStrict
  , isDirected
  , attributes
  , nodes
  , edges
  , subgraphs
  , clusters
  , enclosingCluster
  ) where

import Prelude

import Control.Monad.State (State, execState, gets, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldMap, foldl, for_, traverse_)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.String (toLower)
import Data.String as String
import Stemma.Attributes (Attributes, Port)
import Stemma.Attributes as Attributes
import Stemma.Dot.Syntax (Attribute, Dot, Endpoint(..), GraphKind(..), NodeReference, Statement(..), Target(..), Text(..), text)
import Stemma.Dot.Syntax as Syntax

-- | A node's name. `<b>` and `b` are different names.
newtype NodeName = NodeName Text

derive newtype instance eqNodeName :: Eq NodeName
derive newtype instance ordNodeName :: Ord NodeName

-- | Edges are numbered in the order they were created, from 0.
newtype EdgeId = EdgeId Int

derive newtype instance eqEdgeId :: Eq EdgeId
derive newtype instance ordEdgeId :: Ord EdgeId

-- | Subgraphs are numbered in the order they were first opened, from 0.
newtype SubgraphId = SubgraphId Int

derive newtype instance eqSubgraphId :: Eq SubgraphId
derive newtype instance ordSubgraphId :: Ord SubgraphId

type Node = { name :: NodeName, attributes :: Attributes }

-- | `tailPort` and `headPort` say where on each node the edge attaches, if
-- | the file says (`a:out -> b:in`, or the `tailport` and `headport`
-- | attributes).
type Edge =
  { id :: EdgeId
  , tail :: NodeName
  , head :: NodeName
  , tailPort :: Maybe Port
  , headPort :: Maybe Port
  , attributes :: Attributes
  }

-- | `nodes` and `edges` are in the order they joined, and include those of
-- | the subgraphs inside this one. `name` is `Nothing` for a subgraph
-- | written without one, and `parent` is `Nothing` for one directly inside
-- | the graph.
-- |
-- | `cluster` says whether this subgraph is a cluster: a group that is
-- | drawn, with its nodes kept together inside a box. A subgraph is one if
-- | its name starts with `cluster` (in any case) or it sets `cluster=true`,
-- | unless the graph's `clusterrank` turns clusters off.
type Subgraph =
  { id :: SubgraphId
  , name :: Maybe Text
  , parent :: Maybe SubgraphId
  , cluster :: Boolean
  , attributes :: Attributes
  , nodes :: Array NodeName
  , edges :: Array EdgeId
  }

newtype Graph = Graph
  { name :: Maybe Text
  , strict :: Boolean
  , directed :: Boolean
  , attributes :: Attributes
  , nodes :: Array Node
  , edges :: Array Edge
  , subgraphs :: Array Subgraph
  }

name :: Graph -> Maybe Text
name (Graph graph) = graph.name

isStrict :: Graph -> Boolean
isStrict (Graph graph) = graph.strict

isDirected :: Graph -> Boolean
isDirected (Graph graph) = graph.directed

attributes :: Graph -> Attributes
attributes (Graph graph) = graph.attributes

-- | In the order they were created.
nodes :: Graph -> Array Node
nodes (Graph graph) = graph.nodes

-- | In the order they were created.
edges :: Graph -> Array Edge
edges (Graph graph) = graph.edges

-- | In the order they were first opened.
subgraphs :: Graph -> Array Subgraph
subgraphs (Graph graph) = graph.subgraphs

-- | The subgraphs that are clusters, in the order they were first opened.
clusters :: Graph -> Array Subgraph
clusters = Array.filter _.cluster <<< subgraphs

-- | The nearest cluster around a subgraph, if it is inside one. Subgraphs
-- | that are not clusters do not count: a cluster inside a plain subgraph
-- | inside another cluster is nested in that other cluster.
enclosingCluster :: Graph -> Subgraph -> Maybe Subgraph
enclosingCluster graph = outward
  where
  byId = Map.fromFoldable (map (\s -> Tuple s.id s) (subgraphs graph))
  outward subgraph = subgraph.parent >>= \id -> Map.lookup id byId >>= \parent ->
    if parent.cluster then Just parent else outward parent

-- Where a statement is: directly in the graph, or inside a subgraph.
data Place = TopLevel | Inside SubgraphId

derive instance eqPlace :: Eq Place
derive instance ordPlace :: Ord Place

-- Things in the order they arrived, with a quick test for membership.
type Members a = { present :: Set a, newestFirst :: List a }

noMembers :: forall a. Members a
noMembers = { present: Set.empty, newestFirst: Nil }

joined :: forall a. Ord a => a -> Members a -> Members a
joined member members =
  if Set.member member members.present then members
  else { present: Set.insert member members.present, newestFirst: Cons member members.newestFirst }

inOrder :: forall a. Members a -> Array a
inOrder members = Array.fromFoldable (List.reverse members.newestFirst)

-- The graph or a subgraph while the file is being read.
type Scope =
  { name :: Maybe Text
  , parent :: Place
  , attributes :: Attributes
  , nodeDefaults :: Attributes
  , edgeDefaults :: Attributes
  , nodes :: Members NodeName
  , edges :: Members EdgeId
  }

type EdgeRead = { tail :: NodeName, head :: NodeName, key :: Maybe String, attributes :: Attributes }

type Reading =
  { nodes :: Map NodeName { position :: Int, attributes :: Attributes }
  , edges :: Map EdgeId EdgeRead
  -- The keys edges have been made with.
  , keys :: Set String
  -- The edges between two nodes, oldest first. The nodes are in a fixed
  -- order if the graph is undirected.
  , between :: Map (Tuple NodeName NodeName) (Array EdgeId)
  , graph :: Scope
  , subgraphs :: Map SubgraphId Scope
  , named :: Map (Tuple Place Text) SubgraphId
  }

type Read = State Reading

fromDot :: Dot -> Graph
fromDot dot = Graph
  { name: dot.name
  , strict: dot.strict
  , directed
  , attributes: read.graph.attributes
  , nodes: map (\(Tuple node found) -> { name: node, attributes: found.attributes })
      (Array.sortWith (\(Tuple _ found) -> found.position) (Map.toUnfoldable read.nodes))
  , edges: map
      ( \(Tuple id edge) ->
          { id
          , tail: edge.tail
          , head: edge.head
          , tailPort: Attributes.tailPort edge.attributes
          , headPort: Attributes.headPort edge.attributes
          , attributes: edge.attributes
          }
      )
      (Map.toUnfoldable read.edges)
  , subgraphs: map
      ( \(Tuple id scope) ->
          { id
          , name: scope.name
          , parent: case scope.parent of
              TopLevel -> Nothing
              Inside outer -> Just outer
          , cluster: clustersDrawn && (any namedCluster scope.name || Attributes.marksCluster scope.attributes)
          , attributes: scope.attributes
          , nodes: inOrder scope.nodes
          , edges: inOrder scope.edges
          }
      )
      (Map.toUnfoldable read.subgraphs)
  }
  where
  directed = dot.kind == Directed
  clustersDrawn = Attributes.clustersAreDrawn read.graph.attributes
  namedCluster written = toLower (String.take 7 (text written)) == "cluster"

  read = execState (statements TopLevel dot.statements)
    { nodes: Map.empty
    , edges: Map.empty
    , keys: Set.empty
    , between: Map.empty
    , graph: emptyScope Nothing TopLevel Attributes.empty
    , subgraphs: Map.empty
    , named: Map.empty
    }

  statements :: Place -> Array Statement -> Read Unit
  statements here = traverse_ (statement here)

  statement :: Place -> Statement -> Read Unit
  statement here = case _ of
    NodeStatement reference given -> do
      mention here (NodeName reference.name)
      modify_ \r -> r { nodes = Map.update (\found -> Just found { attributes = with given found.attributes }) (NodeName reference.name) r.nodes }
    EdgeStatement endpoints given -> do
      ends <- traverse (endpoint here) endpoints
      resolved <- traverse members ends
      for_ (Array.zip resolved (Array.drop 1 resolved)) \(Tuple tails heads) ->
        for_ tails \tail -> for_ heads \head -> connect here tail head given
    DefaultsStatement Nodes given -> inScope here \s -> s { nodeDefaults = with given s.nodeDefaults }
    DefaultsStatement Edges given -> inScope here \s -> s { edgeDefaults = with given s.edgeDefaults }
    DefaultsStatement Graphs given -> inScope here \s -> s { attributes = with given s.attributes }
    GraphAttribute given -> inScope here \s -> s { attributes = with [ given ] s.attributes }
    SubgraphStatement subgraph -> void (enter here subgraph)

  -- Open the subgraph, read what is inside it, and say which one it is.
  enter :: Place -> Syntax.Subgraph -> Read SubgraphId
  enter here (Syntax.Subgraph subgraph) = do
    inner <- open here subgraph.name
    statements (Inside inner) subgraph.statements
    pure inner

  open :: Place -> Maybe Text -> Read SubgraphId
  open here given = do
    existing <- gets \r -> given >>= \n -> Map.lookup (Tuple here n) r.named
    case existing of
      Just inner -> pure inner
      Nothing -> do
        inner <- gets \r -> SubgraphId (Map.size r.subgraphs)
        inherited <- gets \r -> foldMap _.attributes (scopeAt here r)
        modify_ \r -> r
          { subgraphs = Map.insert inner (emptyScope given here inherited) r.subgraphs
          , named = case given of
              Just n -> Map.insert (Tuple here n) inner r.named
              Nothing -> r.named
          }
        pure inner

  -- Read one end of an edge statement: a node, or a subgraph.
  endpoint :: Place -> Endpoint -> Read (Either NodeReference SubgraphId)
  endpoint here = case _ of
    NodeEndpoint reference -> mention here (NodeName reference.name) $> Left reference
    SubgraphEndpoint subgraph -> Right <$> enter here subgraph

  -- The nodes an end stands for, asked once every end has been read.
  members :: Either NodeReference SubgraphId -> Read (Array { node :: NodeName, port :: Maybe String })
  members = case _ of
    Left reference -> pure [ { node: NodeName reference.name, port: reference.port } ]
    Right inner -> gets \r ->
      map (\node -> { node, port: Nothing }) (foldMap (_.nodes >>> inOrder) (Map.lookup inner r.subgraphs))

  -- A node named somewhere: created if it is new, with the defaults in
  -- force there, and made a member of that subgraph and those around it.
  mention :: Place -> NodeName -> Read Unit
  mention here node = do
    modify_ \r ->
      if Map.member node r.nodes then r
      else r { nodes = Map.insert node { position: Map.size r.nodes, attributes: inForce _.nodeDefaults here r } r.nodes }
    outwards here \s -> s { nodes = joined node s.nodes }

  -- One pair of nodes of an edge statement, by the steps Graphviz takes:
  --
  --   1. If the statement's key has been used by an edge already, or it has
  --      no key and the graph is strict, look for an edge to join: one
  --      between these nodes with that key (any edge, if there is no key),
  --      first among the edges of this subgraph, then in the whole graph.
  --      If several would do, the newest is joined.
  --   2. Otherwise, or if there is none: in a strict graph, if this
  --      subgraph already has an edge between these nodes written the same
  --      way round, do nothing at all.
  --   3. Otherwise make a new edge.
  --
  -- Step 2 looks only in the subgraph the statement is in, and only for an
  -- edge written the same way round, even in an undirected graph. Those are
  -- the two ways a strict graph can come to hold two edges between the same
  -- nodes: `strict graph { p -- g; g -- p [key=k] }` has two. This is what
  -- Graphviz does, not what its documentation says strict means.
  --
  -- "The newest" in step 1 is a choice, and the one place this module does
  -- not claim to match Graphviz. It only matters in a strict graph that
  -- already holds two edges between the same nodes. There Graphviz's own
  -- choice is not stable: the same file read twice can come out two ways
  -- (see known-differences/).
  connect :: Place -> { node :: NodeName, port :: Maybe String } -> { node :: NodeName, port :: Maybe String } -> Array Attribute -> Read Unit
  connect here tail head given = do
    r <- gets identity
    let
      between = foldMap identity (Map.lookup ends r.between)
      candidates = Array.mapMaybe (\id -> Tuple id <$> Map.lookup id r.edges) between
      here' = Array.filter (\(Tuple id _) -> isMember r id) candidates
      keyUsed = case key of
        Just k -> Set.member k r.keys
        Nothing -> false
      suits (Tuple _ edge) = not keyUsed || edge.key == key
      toJoin =
        if keyUsed || (key == Nothing && dot.strict) then
          case Array.last (Array.filter suits here') of
            Just found -> Just found
            Nothing -> Array.last (Array.filter suits candidates)
        else Nothing
    case toJoin of
      Just (Tuple id edge) -> do
        let stated = if edge.tail /= edge.head && edge.head == tail.node then statedReversed else statedAsWritten
        modify_ _ { edges = Map.insert id edge { attributes = stated <> edge.attributes } r.edges }
        member id
      Nothing -> unless (dot.strict && any (\(Tuple _ edge) -> edge.tail == tail.node && edge.head == head.node) here') do
        let id = EdgeId (Map.size r.edges)
        modify_ _
          { edges = Map.insert id
              { tail: tail.node, head: head.node, key, attributes: statedAsWritten <> inForce _.edgeDefaults here r }
              r.edges
          , keys = case key of
              Just k -> Set.insert k r.keys
              Nothing -> r.keys
          , between = Map.insert ends (Array.snoc between id) r.between
          }
        member id
    where
    member id = outwards here \s -> s { edges = joined id s.edges }
    isMember r id = case here of
      TopLevel -> true
      Inside inner -> any (\s -> Set.member id s.edges.present) (Map.lookup inner r.subgraphs)
    key = (_.value >>> text) <$> Array.find (_.key >>> (_ == "key")) given
    ends = if not directed && head.node < tail.node then Tuple head.node tail.node else Tuple tail.node head.node
    statedAsWritten = stated' tail.port head.port
    statedReversed = stated' head.port tail.port
    -- Ports written with `:` first, then the statement's own attributes,
    -- which win if they name `tailport` or `headport` themselves.
    stated' tailPort headPort = with (Array.filter (_.key >>> (_ /= "key")) given)
      (port "tailport" tailPort (port "headport" headPort Attributes.empty))
    port which = case _ of
      Just value -> Attributes.set which (Plain value)
      Nothing -> identity

  scopeAt :: Place -> Reading -> Maybe Scope
  scopeAt here r = case here of
    TopLevel -> Just r.graph
    Inside inner -> Map.lookup inner r.subgraphs

  inScope :: Place -> (Scope -> Scope) -> Read Unit
  inScope here change = modify_ \r -> case here of
    TopLevel -> r { graph = change r.graph }
    Inside inner -> r { subgraphs = Map.update (change >>> Just) inner r.subgraphs }

  -- Apply a change to a subgraph and every subgraph around it, but not to
  -- the graph itself, which has everything as a member already.
  outwards :: Place -> (Scope -> Scope) -> Read Unit
  outwards here change = case here of
    TopLevel -> pure unit
    Inside inner -> do
      inScope here change
      parent <- gets \r -> _.parent <$> Map.lookup inner r.subgraphs
      for_ parent \outer -> outwards outer change

  -- The defaults in force somewhere: its own, over those of the subgraphs
  -- around it, and of the graph, as they are now.
  inForce :: (Scope -> Attributes) -> Place -> Reading -> Attributes
  inForce which here r = case here of
    TopLevel -> which r.graph
    Inside inner -> foldMap (\s -> which s <> inForce which s.parent r) (Map.lookup inner r.subgraphs)

emptyScope :: Maybe Text -> Place -> Attributes -> Scope
emptyScope given parent inherited =
  { name: given, parent, attributes: inherited, nodeDefaults: Attributes.empty, edgeDefaults: Attributes.empty, nodes: noMembers, edges: noMembers }

-- The attributes with some set, later ones winning.
with :: Array Attribute -> Attributes -> Attributes
with given onto = foldl (\soFar a -> Attributes.set a.key a.value soFar) onto given
