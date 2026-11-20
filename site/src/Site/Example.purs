-- | A bundled example: a DOT file from the corpus, embedded at build time
-- | with the path of Graphviz's own drawing of it.
module Site.Example
  ( Example
  , ExampleName(..)
  , Group(..)
  , groups
  , groupTitle
  ) where

import Prelude

-- | The file's name without its directory or `.gv`, as the picker shows it.
newtype ExampleName = ExampleName String

derive newtype instance eqExampleName :: Eq ExampleName

-- | Where in the corpus an example comes from, which is also how the
-- | picker groups them.
data Group = GraphvizGallery | Clusters | RankSets

derive instance eqGroup :: Eq Group

-- | `source` is the file's path in the repository, and `graphvizPicture`
-- | the path of Graphviz's SVG relative to the page.
type Example =
  { name :: ExampleName
  , group :: Group
  , source :: String
  , graphvizPicture :: String
  , dot :: String
  }

-- | The groups in the order the picker shows them.
groups :: Array Group
groups = [ GraphvizGallery, Clusters, RankSets ]

groupTitle :: Group -> String
groupTitle = case _ of
  GraphvizGallery -> "Graphviz’s examples"
  Clusters -> "Clusters"
  RankSets -> "Rank sets"
