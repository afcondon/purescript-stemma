-- | DOT text to tokens.
-- |
-- | The lexer is a list of rules, tried in order at each position. A rule
-- | looks at the text from there and, if it applies, says how far it
-- | reaches and what it found: something to skip, a token, or a mistake.
-- | `rules` below is the whole lexical grammar, and its order is the
-- | precedence: `->` before a numeral that starts with `-`, a numeral
-- | before a name.
-- |
-- | The rules themselves are Graphviz's, established against it:
-- |
-- |   - a name is letters, digits and underscores not starting with a digit,
-- |     where any character above ASCII counts as a letter;
-- |   - a numeral is `-?(.d+ | d+(.d*)?)`;
-- |   - in a quoted string only `\"` and a backslash before a line end are
-- |     escapes; every other backslash stays, for whoever reads the label;
-- |   - `<...>` is an HTML string, closed by its matching `>`;
-- |   - `/* */`, `//` and lines starting with `#` are comments.
module Graphviz.Dot.Lexer
  ( Token(..)
  , Located
  , tokens
  ) where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits as CodeUnits

data Token
  = Name String
  | Quoted String
  | Html String
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | Equals
  | Semicolon
  | Comma
  | Colon
  | Plus
  | Arrow
  | Line

derive instance eqToken :: Eq Token

type Located = { token :: Token, line :: Int }

-- The text, as characters that can be reached by position.
newtype Source = Source (Array Char)

-- A position in the text: how many characters come before it.
type Position = Int

data Found
  = Skipped
  | Token Token
  | Mistake String

-- What a rule found, and the position after it.
type Step = { found :: Found, next :: Position }

type Rule = Source -> Position -> Maybe Step

-- | The lexical grammar, in order of precedence.
rules :: Array Rule
rules =
  [ skipping (spanOf isBlank)
  , skipping hashLine
  , skipping (from "//" restOfLine)
  , blockComment
  , quoted
  , html
  , symbol "->" Arrow
  , symbol "--" Line
  , naming numeral
  , naming (spanOf isNameStart `then'` spanWhile isNamePart)
  , symbol "{" OpenBrace
  , symbol "}" CloseBrace
  , symbol "[" OpenBracket
  , symbol "]" CloseBracket
  , symbol "=" Equals
  , symbol ";" Semicolon
  , symbol "," Comma
  , symbol ":" Colon
  , symbol "+" Plus
  ]

tokens :: String -> Either String (Array Located)
tokens text = go 0 1 Nil
  where
  source = Source (toCharArray text)

  go :: Position -> Int -> List Located -> Either String (Array Located)
  go position line found =
    if atEnd source position then Right (Array.fromFoldable (List.reverse found))
    else case Array.findMap (\rule -> rule source position) rules of
      Nothing -> Left ("line " <> show line <> ": unexpected character " <> show (slice source position (position + 1)))
      Just step -> case step.found of
        Mistake problem -> Left ("line " <> show line <> ": " <> problem)
        Skipped -> go step.next (line + lineEnds source position step.next) found
        Token token -> go step.next (line + lineEnds source position step.next) (Cons { token, line } found)

-- Reading the source ---------------------------------------------------------

charAt :: Source -> Position -> Maybe Char
charAt (Source chars) = Array.index chars

atEnd :: Source -> Position -> Boolean
atEnd (Source chars) position = position >= Array.length chars

slice :: Source -> Position -> Position -> String
slice (Source chars) start end = fromCharArray (Array.slice start end chars)

-- Whether the text at a position starts with the given characters.
startsWith :: String -> Source -> Position -> Boolean
startsWith wanted source position = slice source position (position + CodeUnits.length wanted) == wanted

lineEnds :: Source -> Position -> Position -> Int
lineEnds (Source chars) start end = Array.length (Array.filter (_ == '\n') (Array.slice start end chars))

-- Spans: how far something reaches -------------------------------------------

-- A span says where a piece of text starting at a position ends, if such
-- a piece starts there at all.
type Span = Source -> Position -> Maybe Position

-- Zero or more characters of a kind. Always matches, perhaps nothing.
spanWhile :: (Char -> Boolean) -> Span
spanWhile wanted source = Just <<< reach
  where
  reach position = case charAt source position of
    Just c | wanted c -> reach (position + 1)
    _ -> position

-- One or more characters of a kind.
spanOf :: (Char -> Boolean) -> Span
spanOf wanted source position = case spanWhile wanted source position of
  Just end | end > position -> Just end
  _ -> Nothing

-- One span and then another.
then' :: Span -> Span -> Span
then' first second source position = first source position >>= second source

-- The given characters, and then a span.
from :: String -> Span -> Span
from wanted after source position =
  if startsWith wanted source position then after source (position + CodeUnits.length wanted) else Nothing

optionally :: Span -> Span
optionally span source position = case span source position of
  Nothing -> Just position
  found -> found

restOfLine :: Span
restOfLine = spanWhile (_ /= '\n')

-- A line whose first character is `#`.
hashLine :: Span
hashLine source position =
  if startsWith "#" source position && (position == 0 || charAt source (position - 1) == Just '\n') then restOfLine source position
  else Nothing

-- `-?(.d+ | d+(.d*)?)`
numeral :: Span
numeral = optionally (from "-" (\_ -> Just)) `then'` digitsFirst
  where
  digits = spanOf isDigit
  digitsFirst source position = case digits source position of
    Just end -> optionally (from "." (spanWhile isDigit)) source end
    Nothing -> from "." digits source position

-- Rules ----------------------------------------------------------------------

skipping :: Span -> Rule
skipping span source position = { found: Skipped, next: _ } <$> span source position

-- A name or a numeral: the token is the text itself.
naming :: Span -> Rule
naming span source position = span source position <#> \end -> { found: Token (Name (slice source position end)), next: end }

symbol :: String -> Token -> Rule
symbol written token source position =
  if startsWith written source position then Just { found: Token token, next: position + CodeUnits.length written }
  else Nothing

blockComment :: Rule
blockComment source position =
  if startsWith "/*" source position then Just (close (position + 2)) else Nothing
  where
  close at
    | atEnd source at = { found: Mistake "a /* comment is never closed", next: at }
    | startsWith "*/" source at = { found: Skipped, next: at + 2 }
    | otherwise = close (at + 1)

-- `<` to its matching `>`. The token is what lies between them.
html :: Rule
html source position =
  if startsWith "<" source position then Just (close (position + 1) 1) else Nothing
  where
  close at depth = case charAt source at of
    Nothing -> { found: Mistake "an HTML string is never closed", next: at }
    Just '>' ->
      if depth == 1 then { found: Token (Html (slice source (position + 1) at)), next: at + 1 }
      else close (at + 1) (depth - 1)
    Just '<' -> close (at + 1) (depth + 1)
    Just _ -> close (at + 1) depth

-- `"` to the next `"` that no backslash precedes. `\"` gives a quote, a
-- backslash before a line end gives nothing, and `\\` stays as it is (and
-- so cannot escape a quote that follows it).
quoted :: Rule
quoted source position =
  if startsWith "\"" source position then Just (close (position + 1) Nil) else Nothing
  where
  close at kept = case charAt source at of
    Nothing -> { found: Mistake "a quoted string is never closed", next: at }
    Just '"' -> { found: Token (Quoted (fromCharArray (Array.fromFoldable (List.reverse kept)))), next: at + 1 }
    Just '\\' -> case charAt source (at + 1) of
      Just '"' -> close (at + 2) (Cons '"' kept)
      Just '\\' -> close (at + 2) (Cons '\\' (Cons '\\' kept))
      Just '\n' -> close (at + 2) kept
      Just '\r' | charAt source (at + 2) == Just '\n' -> close (at + 3) kept
      _ -> close (at + 1) (Cons '\\' kept)
    Just c -> close (at + 1) (Cons c kept)

-- Kinds of character ---------------------------------------------------------

-- Blanks, including the byte-order mark some files start with.
isBlank :: Char -> Boolean
isBlank c = c == ' ' || c == '\t' || c == '\r' || c == '\n' || toCharCode c == 12 || toCharCode c == 65279

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isNameStart :: Char -> Boolean
isNameStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || toCharCode c >= 128

isNamePart :: Char -> Boolean
isNamePart c = isNameStart c || isDigit c
