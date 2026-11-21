-- | The little of Graphviz's HTML-like labels that is about lines of text:
-- | `<br/>` ends a line, `<b>` makes text bold, and `<font point-size="9">`
-- | sets its size. That is enough for a title over a list. Tables are not
-- | read here; `Stemma.Layout` sizes those roughly and by itself.
-- |
-- | The layout uses this to size a node and a renderer to draw its label,
-- | so that the two agree on what the lines are.
module Stemma.Label
  ( Line
  , markupLines
  , hasTable
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, ignoreCase)

-- | One line of a label. `size` is in points when the markup gives one.
-- | A line takes the style of its first piece of text.
type Line = { text :: String, bold :: Boolean, size :: Maybe Number }

-- | Whether the markup holds a table, which `markupLines` does not read.
hasTable :: String -> Boolean
hasTable markup = String.contains (String.Pattern "<table") (String.toLower markup)

-- What has been read so far: finished lines, the line being read, how
-- many `<b>` are open, and the sizes of the `<font>`s that are open.
type Reading =
  { done :: Array Line
  , current :: Maybe Line
  , bold :: Int
  , sizes :: Array (Maybe Number)
  }

-- | The lines of an HTML-like label, in order.
markupLines :: String -> Array Line
markupLines markup = case regex "<[^>]*>" (global <> ignoreCase), regex "point-size\\s*=\\s*\"?([0-9.]+)" ignoreCase of
  Right tag, Right pointSize ->
    let
      texts = Regex.split tag markup
      tags = case Regex.match tag markup of
        Just found -> Array.catMaybes (NonEmptyArray.toArray found)
        Nothing -> []
      -- Text and tags alternate, text first.
      read state place = written (Array.index texts place) (case Array.index tags (place - 1) of
        Just found | place > 0 -> marked pointSize found state
        _ -> state)
      finished = foldl read { done: [], current: Nothing, bold: 0, sizes: [] } (Array.range 0 (Array.length texts - 1))
    in
      finished.done <> Array.fromFoldable finished.current
  _, _ -> [ { text: markup, bold: false, size: Nothing } ]
  where
  written :: Maybe String -> Reading -> Reading
  written text state = case text of
    Just found | String.trim found /= "" -> case state.current of
      Just line -> state { current = Just (line { text = line.text <> entities found }) }
      Nothing -> state { current = Just { text: entities found, bold: state.bold > 0, size: join (Array.last state.sizes) } }
    _ -> state

  marked :: Regex -> String -> Reading -> Reading
  marked pointSize found state =
    let
      name = String.toLower (String.trim (String.replaceAll (String.Pattern "/") (String.Replacement " ") (String.drop 1 (String.takeWhile (_ /= String.codePointFromChar '>') found))))
      closing = String.take 2 found == "</"
      word = String.takeWhile (_ /= String.codePointFromChar ' ') name
    in
      case word of
        "br" -> state
          { done = Array.snoc state.done (case state.current of
              Just line -> line
              Nothing -> { text: "", bold: state.bold > 0, size: join (Array.last state.sizes) })
          , current = Nothing
          }
        "b" -> state { bold = if closing then max 0 (state.bold - 1) else state.bold + 1 }
        "font" ->
          if closing then state { sizes = Array.dropEnd 1 state.sizes }
          else state
            { sizes = Array.snoc state.sizes case Regex.match pointSize found of
                Just groups -> join (NonEmptyArray.index groups 1) >>= Number.fromString
                Nothing -> join (Array.last state.sizes)
            }
        _ -> state

  entities = String.replaceAll (String.Pattern "&amp;") (String.Replacement "&")
    >>> String.replaceAll (String.Pattern "&lt;") (String.Replacement "<")
    >>> String.replaceAll (String.Pattern "&gt;") (String.Replacement ">")
    >>> String.replaceAll (String.Pattern "&quot;") (String.Replacement "\"")
    >>> String.replaceAll (String.Pattern "&nbsp;") (String.Replacement " ")
