-- | How the page sets a label: one font, and Graphviz's line breaks.
-- |
-- | Measuring and drawing must agree, or the boxes the layout makes will
-- | not fit the text drawn in them, so both read the font and the line
-- | breaking from here.
module Site.Label
  ( fontFamily
  , fontSize
  , lineHeight
  , font
  , lines
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)

-- | System fonts only: a web font that loaded after the first layout would
-- | change the widths under a drawing already made.
fontFamily :: String
fontFamily = "\"Helvetica Neue\", Helvetica, Arial, sans-serif"

-- | In points, which are the drawing's units; Graphviz's default size.
fontSize :: Number
fontSize = 14.0

lineHeight :: Number
lineHeight = 1.2 * fontSize

-- | The CSS font shorthand that the canvas measures with.
font :: String
font = show fontSize <> "px " <> fontFamily

-- | A label's lines. In a DOT label `\n`, `\l` and `\r` end a line (centred,
-- | left- and right-justified); the page centres them all. A label that
-- | ends with a break does not gain an empty last line, as in Graphviz.
lines :: String -> Array String
lines written =
  let
    broken = Array.concatMap (split (Pattern "\\r"))
      (Array.concatMap (split (Pattern "\\l")) (split (Pattern "\\n") written))
  in
    case Array.unsnoc broken of
      Just { init, last: "" } | not (Array.null init) -> init
      _ -> broken
