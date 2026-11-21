-- | A bundled example: one of this project's own DOT files, embedded at
-- | build time with the path of Graphviz's own drawing of it.
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

-- | What an example shows, which is how the picker groups them: whole
-- | pictures written for this page, and small files from the test corpus
-- | that each show one rule.
data Group = FamilyTrees | Clusters | RankSets

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
groups = [ FamilyTrees, Clusters, RankSets ]

groupTitle :: Group -> String
groupTitle = case _ of
  FamilyTrees -> "Family trees"
  Clusters -> "Clusters"
  RankSets -> "Rank sets"
