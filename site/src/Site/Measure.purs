-- | Text measured by the browser, for `Stemma.Layout.layoutWith`.
module Site.Measure
  ( measure
  , options
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int (toNumber)
import Stemma.Layout (Options, defaults)
import Site.Label as Label

-- | The width of a line of text in a CSS font, from a canvas's
-- | `measureText`. It is given a pure type because for one font, on one
-- | page, the answer does not change: the page uses system fonts only, so
-- | no font arrives late to change it.
foreign import textWidthImpl :: Fn2 String String Number

-- | The widest line, and as many line heights as there are lines.
measure :: String -> { width :: Number, height :: Number }
measure written =
  let
    lines = Label.lines written
  in
    { width: foldl (\widest line -> max widest (runFn2 textWidthImpl Label.font line)) 0.0 lines
    , height: Label.lineHeight * toNumber (max 1 (Array.length lines))
    }

-- | Graphviz's spacing, with the browser's measurements.
options :: Options
options = defaults { measure = measure }
