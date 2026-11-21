-- | Tokens to the syntax tree, by the grammar in the Graphviz documentation:
-- |
-- | ```
-- | graph      : [strict] (graph | digraph) [ID] '{' stmt_list '}'
-- | stmt_list  : [stmt [';'] stmt_list]
-- | stmt       : node_stmt | edge_stmt | attr_stmt | ID '=' ID | subgraph
-- | attr_stmt  : (graph | node | edge) attr_list
-- | attr_list  : '[' [a_list] ']' [attr_list]
-- | a_list     : ID '=' ID [(';' | ',')] [a_list]
-- | edge_stmt  : (node_id | subgraph) edgeRHS [attr_list]
-- |            | subgraph [attr_list]
-- | edgeRHS    : edgeop (node_id | subgraph) [edgeRHS]
-- | node_stmt  : node_id [attr_list]
-- | node_id    : ID [':' ID [':' ID]]
-- | subgraph   : [subgraph [ID]] '{' stmt_list '}'
-- | ```
-- |
-- | Keywords are matched whatever their case, and only when unquoted. The
-- | edge operator must suit the graph: `->` in a digraph, `--` in a graph.
-- | Quoted strings joined by `+` are one ID.
module Stemma.Dot.Parser
  ( parse
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Stemma.Dot.Lexer (Located, Token(Name, Quoted, OpenBrace, CloseBrace, OpenBracket, CloseBracket, Equals, Semicolon, Comma, Colon, Plus, Arrow, Line), tokens)
import Stemma.Dot.Lexer as Lexer
import Stemma.Dot.Syntax (Attribute, Dot, Endpoint(..), GraphKind(..), NodeReference, Statement(..), Subgraph(..), Target(..), Text(..), text)

-- | Parse the first graph in a DOT file.
parse :: String -> Either String Dot
parse text = do
  found <- tokens text
  evalStateT (graph found) 0

-- The position in the token array.
type Parser = StateT Int (Either String)

data Keyword = Strict | Graph | Digraph | Node | Edge | SubgraphWord

derive instance eqKeyword :: Eq Keyword

keyword :: Token -> Maybe Keyword
keyword = case _ of
  Name word -> case toLower word of
    "strict" -> Just Strict
    "graph" -> Just Graph
    "digraph" -> Just Digraph
    "node" -> Just Node
    "edge" -> Just Edge
    "subgraph" -> Just SubgraphWord
    _ -> Nothing
  _ -> Nothing

graph :: Array Located -> Parser Dot
graph found = do
  strict <- acceptKeyword Strict
  kind <- peek >>= map keyword >>> case _ of
    Just (Just Graph) -> advance $> Undirected
    Just (Just Digraph) -> advance $> Directed
    _ -> failure "expected `graph` or `digraph`"
  name <- optionalId
  expect OpenBrace "`{`"
  statements <- statementList kind
  expect CloseBrace "`}`"
  pure { strict, kind, name, statements }
  where
  peek :: Parser (Maybe Token)
  peek = get <#> \i -> _.token <$> Array.index found i

  peekAfter :: Parser (Maybe Token)
  peekAfter = get <#> \i -> _.token <$> Array.index found (i + 1)

  advance :: Parser Unit
  advance = get >>= \i -> put (i + 1)

  failure :: forall a. String -> Parser a
  failure message = do
    i <- get
    let
      line = case Array.index found i of
        Just located -> "line " <> show located.line
        Nothing -> "the end of the file"
    lift (Left (line <> ": " <> message))

  accept :: Token -> Parser Boolean
  accept token = peek >>= \next ->
    if next == Just token then advance $> true else pure false

  expect :: Token -> String -> Parser Unit
  expect token described = accept token >>= \found' ->
    unless found' (failure ("expected " <> described))

  acceptKeyword :: Keyword -> Parser Boolean
  acceptKeyword wanted = peek >>= \next ->
    if (next >>= keyword) == Just wanted then advance $> true else pure false

  -- An ID: a name, a numeral, an HTML string, or quoted strings joined by +.
  optionalId :: Parser (Maybe Text)
  optionalId = peek >>= case _ of
    Just token@(Name word) | keyword token == Nothing -> advance $> Just (Plain word)
    Just (Lexer.Html inner) -> advance $> Just (Html inner)
    Just (Quoted first) -> advance *> (Just <<< Plain <$> tailRecM joined first)
    _ -> pure Nothing
    where
    joined soFar = peek >>= case _ of
      Just Plus -> peekAfter >>= case _ of
        Just (Quoted more) -> advance *> advance $> Loop (soFar <> more)
        _ -> pure (Done soFar)
      _ -> pure (Done soFar)

  identifier :: String -> Parser Text
  identifier described = optionalId >>= case _ of
    Just value -> pure value
    Nothing -> failure ("expected " <> described)

  statementList :: GraphKind -> Parser (Array Statement)
  statementList kind = inOrder <$> tailRecM step Nil
    where
    step newestFirst = peek >>= case _ of
      Just CloseBrace -> pure (Done newestFirst)
      Nothing -> pure (Done newestFirst)
      Just Semicolon -> advance $> Loop newestFirst
      -- Graphviz accepts a stray comma between statements.
      Just Comma -> advance $> Loop newestFirst
      Just _ -> statement kind <#> \found' -> Loop (Cons found' newestFirst)

  statement :: GraphKind -> Parser Statement
  statement kind = peek >>= \next -> case next >>= keyword of
    Just Graph -> advance *> (DefaultsStatement Graphs <$> attributeLists)
    Just Node -> advance *> (DefaultsStatement Nodes <$> attributeLists)
    Just Edge -> advance *> (DefaultsStatement Edges <$> attributeLists)
    Just SubgraphWord -> subgraph kind >>= edgeOrSubgraph
    Just _ -> failure "a keyword cannot start a statement here"
    Nothing -> case next of
      Just OpenBrace -> subgraph kind >>= edgeOrSubgraph
      _ -> do
        name <- identifier "a statement"
        peek >>= case _ of
          Just Equals -> do
            advance
            value <- identifier "a value after `=`"
            pure (GraphAttribute { key: text name, value })
          _ -> do
            reference <- port name
            isEdge <- startsEdge
            if isEdge then edgeStatement kind (NodeEndpoint reference)
            else NodeStatement reference <$> attributeLists
    where
    -- Graphviz's grammar lets an attribute list follow a subgraph that
    -- has no edges; it has nothing to apply to and is dropped.
    edgeOrSubgraph found' = startsEdge >>= \isEdge ->
      if isEdge then edgeStatement kind (SubgraphEndpoint found')
      else attributeLists $> SubgraphStatement found'

  startsEdge :: Parser Boolean
  startsEdge = peek <#> \next -> next == Just Arrow || next == Just Line

  port :: Text -> Parser NodeReference
  port name = accept Colon >>= case _ of
    false -> pure { name, port: Nothing }
    true -> do
      first <- identifier "a port after `:`"
      accept Colon >>= case _ of
        false -> pure { name, port: Just (text first) }
        true -> do
          compass <- identifier "a compass point after `:`"
          pure { name, port: Just (text first <> ":" <> text compass) }

  edgeStatement :: GraphKind -> Endpoint -> Parser Statement
  edgeStatement kind first = do
    endpoints <- inOrder <$> tailRecM more (Cons first Nil)
    EdgeStatement endpoints <$> attributeLists
    where
    wanted = if kind == Directed then Arrow else Line
    more newestFirst = peek >>= case _ of
      Just token | token == Arrow || token == Line ->
        if token /= wanted then failure
          ( if kind == Directed then "`--` in a digraph; its edges are written `->`"
            else "`->` in an undirected graph; its edges are written `--`"
          )
        else do
          advance
          next <- endpoint kind
          pure (Loop (Cons next newestFirst))
      _ -> pure (Done newestFirst)

  endpoint :: GraphKind -> Parser Endpoint
  endpoint kind = peek >>= \next ->
    if next == Just OpenBrace || (next >>= keyword) == Just SubgraphWord then SubgraphEndpoint <$> subgraph kind
    else NodeEndpoint <$> (identifier "a node or a subgraph after the edge operator" >>= port)

  subgraph :: GraphKind -> Parser Subgraph
  subgraph kind = do
    named <- acceptKeyword SubgraphWord
    name <- if named then optionalId else pure Nothing
    expect OpenBrace "`{`"
    statements <- statementList kind
    expect CloseBrace "`}`"
    pure (Subgraph { name, statements })

  -- Zero or more bracketed lists, read as one.
  attributeLists :: Parser (Array Attribute)
  attributeLists = inOrder <$> tailRecM step Nil
    where
    step newestFirst = accept OpenBracket >>= case _ of
      false -> pure (Done newestFirst)
      true -> do
        inside <- tailRecM attributes newestFirst
        expect CloseBracket "`]`"
        pure (Loop inside)

    attributes newestFirst = peek >>= case _ of
      Just CloseBracket -> pure (Done newestFirst)
      Just Semicolon -> advance $> Loop newestFirst
      Just Comma -> advance $> Loop newestFirst
      _ -> do
        key <- identifier "an attribute name"
        expect Equals "`=` after an attribute name"
        value <- identifier "an attribute value"
        pure (Loop (Cons { key: text key, value } newestFirst))

  -- Lists are built newest first, which costs nothing per item, and turned
  -- round once at the end.
  inOrder :: forall a. List a -> Array a
  inOrder = Array.fromFoldable <<< List.reverse
