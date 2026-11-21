-- | The attributes of a graph, node, edge or subgraph: names to values, as
-- | text, and the typed readings of the ones the layout uses.
-- |
-- | DOT attributes are text and stay text here (plain or HTML, as written), because a file may set any
-- | attribute at all and most are only passed through. What the layout
-- | needs is read through the functions below, each of which says how
-- | Graphviz reads that attribute, so no other module parses a value.
-- |
-- | An attribute set to the empty string still overrides a default, but
-- | reads as absent. That is Graphviz's rule, and `lookup` is where it
-- | lives.
module Stemma.Attributes
  ( Attributes
  , empty
  , set
  , lookup
  , lookupText
  , toArray
  , RankKind(..)
  , rankKind
  , weight
  , minimumLength
  , constrains
  , hasLabel
  , isSet
  , Port
  , Compass(..)
  , tailPort
  , headPort
  , marksCluster
  , clustersAreDrawn
  ) where

import Prelude

import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), toLower, trim)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple)
import Stemma.Dot.Syntax (Text, text)

newtype Attributes = Attributes (Map String Text)

-- | Joining two sets of attributes: where both have a name, the left wins.
instance semigroupAttributes :: Semigroup Attributes where
  append (Attributes over) (Attributes under) = Attributes (Map.union over under)

instance monoidAttributes :: Monoid Attributes where
  mempty = empty

empty :: Attributes
empty = Attributes Map.empty

set :: String -> Text -> Attributes -> Attributes
set key value (Attributes all) = Attributes (Map.insert key value all)

-- | The value of an attribute, if it has one that is not empty, and
-- | whether it was written as plain text or as an HTML string.
lookupText :: String -> Attributes -> Maybe Text
lookupText key (Attributes all) = Map.lookup key all >>= \found ->
  if text found == "" then Nothing else Just found

-- | The characters of an attribute's value, if it has one that is not
-- | empty.
lookup :: String -> Attributes -> Maybe String
lookup key = map text <<< lookupText key

-- | Every attribute as set, the empty ones included, in order of name.
toArray :: Attributes -> Array (Tuple String Text)
toArray (Attributes all) = Map.toUnfoldable all

isSet :: String -> Attributes -> Boolean
isSet key = isJust <<< lookup key

-- | How a subgraph asks for its nodes to be ranked.
data RankKind = Same | Minimum | Maximum | Source | Sink

derive instance eqRankKind :: Eq RankKind

-- | The `rank` attribute of a subgraph, if it is one of the five values
-- | Graphviz knows, in lower case. Anything else is ignored, as Graphviz
-- | ignores it: `rank=SAME` does nothing.
rankKind :: Attributes -> Maybe RankKind
rankKind attributes = lookup "rank" attributes >>= case _ of
  "same" -> Just Same
  "min" -> Just Minimum
  "max" -> Just Maximum
  "source" -> Just Source
  "sink" -> Just Sink
  _ -> Nothing

-- | How strongly an edge pulls its two nodes together: 1 unless said.
weight :: Attributes -> Int
weight = integer { otherwise: 1, least: 0 } "weight"

-- | How many ranks at least an edge must span: 1 unless said.
minimumLength :: Attributes -> Int
minimumLength = integer { otherwise: 1, least: 0 } "minlen"

-- | Whether an edge has any say in the ranking: yes unless `constraint` is
-- | set to something false.
constrains :: Attributes -> Boolean
constrains attributes = case lookup "constraint" attributes of
  Nothing -> true
  Just value -> truth value

hasLabel :: Attributes -> Boolean
hasLabel = isSet "label"

-- | A side or corner of a node, for an edge to be aimed at: the compass
-- | points, the centre (`c`), or whichever side suits (`_`).
data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest | Centre | WhicheverSuits

derive instance eqCompass :: Eq Compass

-- | Where on a node an edge is attached: `node:name`, `node:name:compass`
-- | or `node:compass`.
-- |
-- | `name` is a place the node itself defines (a field of a record, a cell
-- | of an HTML-like label), and `compass` a side of whatever that is.
-- | Written with one part, as in `a:n`, DOT does not say which is meant:
-- | Graphviz takes it as a name if the node has a port called that, and
-- | otherwise as a compass point. So for one part that is a compass word
-- | both fields are filled, and whoever knows the node's ports decides.
type Port = { written :: String, name :: Maybe String, compass :: Maybe Compass }

-- | Where an edge leaves its tail, if it says.
tailPort :: Attributes -> Maybe Port
tailPort = map port <<< lookup "tailport"

-- | Where an edge enters its head, if it says.
headPort :: Attributes -> Maybe Port
headPort = map port <<< lookup "headport"

port :: String -> Port
port written = case String.indexOf (Pattern ":") written of
  Just colon ->
    { written, name: Just (String.take colon written), compass: compass (String.drop (colon + 1) written) }
  Nothing -> { written, name: Just written, compass: compass written }

compass :: String -> Maybe Compass
compass = case _ of
  "n" -> Just North
  "ne" -> Just NorthEast
  "e" -> Just East
  "se" -> Just SouthEast
  "s" -> Just South
  "sw" -> Just SouthWest
  "w" -> Just West
  "nw" -> Just NorthWest
  "c" -> Just Centre
  "_" -> Just WhicheverSuits
  _ -> Nothing

-- | Whether a subgraph's own attributes make it a cluster (`cluster=true`).
-- | A name starting with `cluster` does too, which is not an attribute.
marksCluster :: Attributes -> Boolean
marksCluster attributes = case lookup "cluster" attributes of
  Just value -> truth value
  Nothing -> false

-- | Whether the graph draws clusters at all: not if `clusterrank` is
-- | `none` or `global`, which make every cluster an ordinary subgraph.
clustersAreDrawn :: Attributes -> Boolean
clustersAreDrawn attributes = case toLower <$> lookup "clusterrank" attributes of
  Just "none" -> false
  Just "global" -> false
  _ -> true

-- Graphviz's reading of an integer attribute: the digits it starts with,
-- after blanks and a sign; the default if there are none; and never below
-- the least value allowed.
integer :: { otherwise :: Int, least :: Int } -> String -> Attributes -> Int
integer range key attributes = case lookup key attributes >>= leadingInteger of
  Just value -> max range.least value
  Nothing -> range.otherwise

leadingInteger :: String -> Maybe Int
leadingInteger text = sign <$> Int.fromString (CodeUnits.takeWhile isDigit unsigned)
  where
  trimmed = trim text
  negative = String.take 1 trimmed == "-"
  unsigned = if negative || String.take 1 trimmed == "+" then String.drop 1 trimmed else trimmed
  sign value = if negative then negate value else value
  isDigit c = c >= '0' && c <= '9'

-- Graphviz's reading of a boolean: false and no are false, true and yes are
-- true, and anything else is true if it starts with a number other than 0.
truth :: String -> Boolean
truth value = case toLower value of
  "false" -> false
  "no" -> false
  "true" -> true
  "yes" -> true
  _ -> case leadingInteger value of
    Just number -> number /= 0
    Nothing -> false
