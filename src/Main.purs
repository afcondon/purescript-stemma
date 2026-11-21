-- | Run one stage of the core over each DOT file named on the command line
-- | and write what it made of it, as JSON, into the directory given:
-- |
-- |     node run.mjs read found/graphviz corpus/graphviz/unix.gv ...
-- |     node run.mjs rank found/graphviz corpus/graphviz/unix.gv ...
-- |     node run.mjs draw found/svg corpus/graphviz/unix.gv ...
-- |     node run.mjs geometry found/geometry corpus/graphviz/unix.gv ...
-- |
-- | `bin/check.py` compares those files with what Graphviz reports.
module Main where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringify, toArray, toNumber, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), lastIndexOf)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Stemma.Dot.Parser (parse)
import Stemma.Attributes (Attributes)
import Stemma.Attributes as Attributes
import Stemma.Dot.Syntax (Text(..))
import Stemma.Graph (EdgeId(..), Graph, NodeName(..), SubgraphId(..), fromDot)
import Stemma.Graph as Graph
import Stemma.Layout (DrawnCluster, DrawnEdge, DrawnNode, Stage(..), defaults, drawAt, layout)
import Stemma.Layout.NetworkSimplex (value)
import Stemma.Layout.Order (Vertex(..), backwards, backwardsBetweenClusters, crossings, layered, order, split)
import Stemma.Svg (render)
import Stemma.Layout.Rank (Rank(..), Solved(..), Variable(..), describe, levels, rank, rankGiven, rankOf, ranking, reversed)
import Node.Encoding (Encoding(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.FS.Sync (exists, readFile, readTextFile, writeTextFile)
import Node.Process (argv)

main :: Effect Unit
main = do
  arguments <- Array.drop 2 <$> argv
  case Array.take 2 arguments, Array.drop 2 arguments of
    [ "read", directory ], files -> for_ files \file -> do
      text <- readText file
      write directory file case parse text of
        Left problem -> record [ Tuple "error" (fromString problem) ]
        Right dot -> graphJson (fromDot dot)
    [ "rank", directory ], files -> for_ files \file -> do
      text <- readText file
      given <- oracleRanks (directory <> "/" <> baseName file <> ".oracle.json")
      write directory file case parse text of
        Left problem -> record [ Tuple "error" (fromString problem) ]
        Right dot -> rankJson given (fromDot dot)
    [ "order", directory ], files -> for_ files \file -> do
      text <- readText file
      given <- oracleOrder (directory <> "/" <> baseName file <> ".oracle.json")
      write directory file case parse text of
        Left problem -> record [ Tuple "error" (fromString problem) ]
        Right dot -> orderJson given (fromDot dot)
    [ "draw", directory ], files -> for_ files \file -> do
      text <- readText file
      writeTextFile UTF8 (directory <> "/" <> baseName file <> ".svg") case layout text of
        Left problem -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"600\" height=\"40\"><text x=\"8\" y=\"24\">" <> problem <> "</text></svg>"
        Right drawing -> render drawing
    [ "stages", directory ], files -> for_ files \file -> do
      text <- readText file
      for_ [ Tuple "1-ranked" Ranked, Tuple "2-ordered" Ordered, Tuple "3-positioned" Positioned, Tuple "4-routed" Routed ] \(Tuple name stage) ->
        writeTextFile UTF8 (directory <> "/" <> baseName file <> "." <> name <> ".svg") case parse text >>= (fromDot >>> drawAt stage defaults) of
          Left problem -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"600\" height=\"40\"><text x=\"8\" y=\"24\">" <> problem <> "</text></svg>"
          Right drawing -> render drawing
    [ "geometry", directory ], files -> for_ files \file -> do
      text <- readText file
      write directory file case layout text of
        Left problem -> record [ Tuple "error" (fromString problem) ]
        Right drawing -> record
          [ Tuple "width" (fromNumber drawing.width)
          , Tuple "height" (fromNumber drawing.height)
          , Tuple "scale" (fromNumber drawing.scale)
          , Tuple "nodes" (fromArray (map (\(found :: DrawnNode) -> record [ Tuple "name" (nameJson found.name), Tuple "box" (boxJson found.box) ]) drawing.nodes))
          , Tuple "edges" (fromArray (map
              ( \(found :: DrawnEdge) -> record
                  [ Tuple "tail" (nameJson found.tail)
                  , Tuple "head" (nameJson found.head)
                  , Tuple "path" (fromArray (map pointJson found.path))
                  , Tuple "labelAt" (case found.labelAt of
                      Just at -> pointJson at
                      Nothing -> jsonNull)
                  ]
              )
              drawing.edges))
          , Tuple "clusters" (fromArray (map (\(found :: DrawnCluster) -> boxJson found.box) drawing.clusters))
          ]
    _, _ -> log "usage: (read | rank | order | draw | stages | geometry) <output directory> <file.gv>..."
  where
  nameJson (NodeName written) = textJson written
  pointJson at = fromArray [ fromNumber at.x, fromNumber at.y ]
  boxJson box = fromArray [ fromNumber box.centre.x, fromNumber box.centre.y, fromNumber box.width, fromNumber box.height ]
  write directory file found =
    writeTextFile UTF8 (directory <> "/" <> baseName file <> ".json") (stringify found)

-- The rank dot gave each node of this file, if bin/check.py left them
-- beside the output: an array of ranks, in the order of the graph's nodes.
oracleRanks :: String -> Effect (Maybe (Array Rank))
oracleRanks path = do
  present <- exists path
  if not present then pure Nothing
  else do
    text <- readTextFile UTF8 path
    pure case jsonParser text of
      Right json -> toArray json >>= traverse (map (Int.round >>> Rank) <<< toNumber)
      Left _ -> Nothing

-- What the ranking pass makes of a graph, once turning cycles round its
-- own way and once following dot (the same edges turned round, and each
-- cluster laid out inside as dot has it): every level's problem and the
-- answer found for it.
--
-- Nodes are written as their place in the graph's order, since two nodes
-- can share their characters (`b` and `<b>`). A level's variables are
-- numbered; `of` says which variable each node moves with at that level
-- and `offset` how far below it the node sits, both null for a node that
-- is not in the level.
rankJson :: Maybe (Array Rank) -> Graph -> Json
rankJson oracle graph = case rank graph of
  Left reason -> record [ Tuple "unsupported" (fromString (describe reason)) ]
  Right own -> record
    ( [ Tuple "own" (attempt own) ]
        <> case oracle of
          Just ranks | Array.length ranks == Array.length names ->
            case rankGiven (Map.fromFoldable (Array.zip names ranks)) graph of
              Right given -> [ Tuple "given" (attempt given) ]
              Left _ -> []
          _ -> []
    )
  where
  int = Int.toNumber >>> fromNumber
  names = map _.name (Graph.nodes graph)

  attempt solved = record
    [ Tuple "levels" (fromArray (map level (levels solved)))
    , Tuple "ranks" (fromArray (map (\node -> case rankOf (ranking solved) node of
        Rank found -> int found) names))
    , Tuple "reversed" (fromArray (map (\(EdgeId id) -> int id) (reversed solved)))
    ]

  level (Solved solved) =
    let
      stated = solved.problem
      variables = Array.nub
        (map _.variable (Array.fromFoldable (Map.values stated.place)) <> Array.concatMap (\r -> [ r.tail, r.head ]) stated.requirements)
      numbers = Map.fromFoldable (Array.mapWithIndex (\position variable -> Tuple variable position) variables)
      number variable = case Map.lookup variable numbers of
        Just found -> int found
        Nothing -> jsonNull
      requirement r = record
        [ Tuple "tail" (number r.tail)
        , Tuple "head" (number r.head)
        , Tuple "weight" (int r.weight)
        , Tuple "minimum" (int r.minimum)
        ]
      placed pick node = case Map.lookup node stated.place of
        Just found -> pick found
        Nothing -> jsonNull
    in
      record
        [ Tuple "cluster" (case stated.cluster of
            Just (SubgraphId id) -> int id
            Nothing -> jsonNull)
        , Tuple "variables" (int (Array.length variables))
        , Tuple "slack" (fromArray (map (case _ of
            Slack _ -> fromBoolean true
            _ -> fromBoolean false) variables))
        , Tuple "of" (fromArray (map (placed (_.variable >>> number)) names))
        , Tuple "offset" (fromArray (map (placed (_.offset >>> int)) names))
        , Tuple "requirements" (fromArray (map requirement stated.requirements))
        , Tuple "values" (fromArray (map (value solved.values >>> int) variables))
        , Tuple "tree" (fromArray (map int solved.tree))
        ]

-- The rank and the place in its rank that dot gave each node of this
-- file, if bin/check.py left them beside the output: two arrays in the
-- order of the graph's nodes.
oracleOrder :: String -> Effect (Maybe { ranks :: Array Int, places :: Array Int })
oracleOrder path = do
  present <- exists path
  if not present then pure Nothing
  else do
    text <- readTextFile UTF8 path
    pure case jsonParser text of
      Right json -> do
        object <- toObject json
        ranks <- Object.lookup "ranks" object >>= toArray >>= traverse (map Int.round <<< toNumber)
        places <- Object.lookup "orders" object >>= toArray >>= traverse (map Int.round <<< toNumber)
        pure { ranks, places }
      Left _ -> Nothing

-- What the ordering pass makes of a graph ranked as dot ranked it: how
-- many crossings its order leaves, how many times it splits a cluster,
-- and how many same-rank edges it has the wrong way round (both of which
-- should be none).
--
-- dot's own order is measured the same way as far as it can be. It says
-- where the nodes are and not where the bends are, so its clusters and
-- same-rank edges are judged among the nodes alone, and its crossings are
-- counted only where there are no bends.
orderJson :: Maybe { ranks :: Array Int, places :: Array Int } -> Graph -> Json
orderJson oracle graph = case oracle of
  Just given | Array.length given.ranks == Array.length names ->
    let
      ranks = Map.fromFoldable (Array.zip names given.ranks)
      places = Map.fromFoldable (Array.zip names given.places)
      rankOf node = case Map.lookup node ranks of
        Just found -> Rank found
        Nothing -> Rank 0
      cut = layered graph rankOf
      ordered = order cut
      bends = Array.length (Array.filter isBend (Array.concat cut.ranks))
      placeOf = case _ of
        Node node -> Map.lookup node places
        Bend _ _ _ -> Nothing
      dots = cut { ranks = map (Array.sortWith placeOf <<< Array.filter (not <<< isBend)) cut.ranks }
    in
      record
        [ Tuple "crossings" (int (crossings ordered))
        , Tuple "split" (int (split ordered))
        , Tuple "backwards" (int (backwards ordered))
        , Tuple "backwardsBetweenClusters" (int (backwardsBetweenClusters ordered))
        , Tuple "bends" (int bends)
        , Tuple "dots" (if bends == 0 then int (crossings dots) else jsonNull)
        , Tuple "dotsSplit" (int (split dots))
        , Tuple "dotsBackwards" (int (backwards dots))
        , Tuple "dotsBackwardsBetweenClusters" (int (backwardsBetweenClusters dots))
        ]
  _ -> record [ Tuple "error" (fromString "no ranks from dot to order by") ]
  where
  int = Int.toNumber >>> fromNumber
  names = map _.name (Graph.nodes graph)
  isBend = case _ of
    Bend _ _ _ -> true
    Node _ -> false

-- DOT files are bytes, and some of Graphviz's own examples are Latin-1.
-- Decoding is the caller's business, not the core's: read as UTF-8, and if
-- the bytes are not valid UTF-8 (decoding and encoding again changes their
-- number), read as Latin-1.
readText :: String -> Effect String
readText file = do
  bytes <- readFile file
  asUtf8 <- Buffer.toString UTF8 bytes
  again <- Buffer.fromString asUtf8 UTF8 :: Effect Buffer
  sameSize <- (==) <$> Buffer.size bytes <*> Buffer.size again
  if sameSize then pure asUtf8 else Buffer.toString Latin1 bytes

baseName :: String -> String
baseName path = fromMaybe path do
  slash <- lastIndexOf (Pattern "/") path
  pure (String.drop (slash + 1) path)

record :: Array (Tuple String Json) -> Json
record = fromObject <<< Object.fromFoldable

attributesJson :: Attributes -> Json
attributesJson attributes =
  record (map (\(Tuple key found) -> Tuple key (textJson found)) (Attributes.toArray attributes))

-- Plain text is a string; an HTML string is an object saying so.
textJson :: Text -> Json
textJson = case _ of
  Plain characters -> fromString characters
  Html characters -> record [ Tuple "html" (fromString characters) ]

optional :: Maybe Text -> Json
optional = case _ of
  Just found -> textJson found
  Nothing -> jsonNull

graphJson :: Graph -> Json
graphJson graph = record
  [ Tuple "name" (optional (Graph.name graph))
  , Tuple "strict" (fromBoolean (Graph.isStrict graph))
  , Tuple "directed" (fromBoolean (Graph.isDirected graph))
  , Tuple "attributes" (attributesJson (Graph.attributes graph))
  , Tuple "nodes" (fromArray (map (\n -> record [ Tuple "name" (nodeName n.name), Tuple "attributes" (attributesJson n.attributes) ]) (Graph.nodes graph)))
  , Tuple "edges" (fromArray (map edge (Graph.edges graph)))
  , Tuple "subgraphs" (fromArray (map subgraph (Graph.subgraphs graph)))
  ]
  where
  int = Int.toNumber >>> fromNumber
  nodeName (NodeName written) = textJson written
  edge e = record
    [ Tuple "tail" (nodeName e.tail), Tuple "head" (nodeName e.head), Tuple "attributes" (attributesJson e.attributes) ]
  subgraph s = record
    [ Tuple "name" (optional s.name)
    , Tuple "parent" (case s.parent of
        Just (SubgraphId outer) -> int outer
        Nothing -> jsonNull)
    , Tuple "cluster" (fromBoolean s.cluster)
    , Tuple "attributes" (attributesJson s.attributes)
    , Tuple "nodes" (fromArray (map nodeName s.nodes))
    , Tuple "edges" (fromArray (map (\(EdgeId id) -> int id) s.edges))
    ]
