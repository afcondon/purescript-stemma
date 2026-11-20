-- | The DOT language as written: what a file says, before anything is made
-- | of it. `Graphviz.Graph` turns this into nodes and edges.
module Graphviz.Dot.Syntax
  ( Dot
  , Text(..)
  , text
  , GraphKind(..)
  , Statement(..)
  , Target(..)
  , Attribute
  , Endpoint(..)
  , NodeReference
  , Subgraph(..)
  ) where

import Prelude

import Data.Maybe (Maybe)

-- | A piece of text as DOT writes it: plain (a name, a numeral or a quoted
-- | string) or an HTML string, written between `<` and `>`. Graphviz keeps
-- | the two apart even when the characters are the same: `<b>` and `b` are
-- | different nodes, and a label that is an HTML string is drawn as markup.
data Text = Plain String | Html String

derive instance eqText :: Eq Text
derive instance ordText :: Ord Text

-- | The characters, whichever kind it is.
text :: Text -> String
text = case _ of
  Plain characters -> characters
  Html characters -> characters

data GraphKind = Directed | Undirected

derive instance eqGraphKind :: Eq GraphKind

type Dot =
  { strict :: Boolean
  , kind :: GraphKind
  , name :: Maybe Text
  , statements :: Array Statement
  }

type Attribute = { key :: String, value :: Text }

-- | What an attribute statement (`node [...]`) sets the defaults of.
data Target = Graphs | Nodes | Edges

-- | A node as an edge or node statement names it: `name`, `name:port` or
-- | `name:port:compass`. `port` is everything after the first colon.
type NodeReference = { name :: Text, port :: Maybe String }

data Endpoint
  = NodeEndpoint NodeReference
  | SubgraphEndpoint Subgraph

newtype Subgraph = Subgraph { name :: Maybe Text, statements :: Array Statement }

data Statement
  = NodeStatement NodeReference (Array Attribute)
  -- | A chain: `a -> b -> c [..]` has three endpoints.
  | EdgeStatement (Array Endpoint) (Array Attribute)
  | DefaultsStatement Target (Array Attribute)
  -- | `key = value` on its own: an attribute of the enclosing graph.
  | GraphAttribute Attribute
  | SubgraphStatement Subgraph
