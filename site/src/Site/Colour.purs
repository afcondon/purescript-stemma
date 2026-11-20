-- | Graphviz colour values, read as far as SVG can show them.
-- |
-- | A colour SVG does not understand is not ignored by the browser: an
-- | unknown `fill` paints black. So a value is passed on only when it is
-- | known to be valid CSS, and otherwise the caller's default is used.
module Site.Colour
  ( colour
  ) where

import Prelude

import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split, toLower, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Halogen.Svg.Attributes (Color(..))

-- | `#rrggbb` and `#rrggbbaa` pass through; three numbers from 0 to 1 are
-- | Graphviz's hue, saturation and value; a name passes if CSS knows it.
-- | A list (`red:blue`, for parallel or gradient strokes) gives its first
-- | colour, without any `;fraction` after it.
colour :: String -> Maybe Color
colour written =
  let
    first = case Array.head (split (Pattern ":") written) of
      Just found -> found
      Nothing -> written
    plain = trim (case Array.head (split (Pattern ";") first) of
      Just found -> found
      Nothing -> first)
  in
    case CodeUnits.charAt 0 plain of
      Just '#' | hexadecimal (CodeUnits.drop 1 plain) -> Just (Named plain)
      _ -> case hueSaturationValue plain of
        Just found -> Just found
        Nothing ->
          if Set.member (toLower plain) cssNames then Just (Named (toLower plain)) else Nothing

-- | Three, six or eight hexadecimal digits, which is what CSS accepts
-- | after a `#` (four is valid CSS too, but not something Graphviz writes).
hexadecimal :: String -> Boolean
hexadecimal digits =
  Array.elem (CodeUnits.length digits) [ 3, 6, 8 ]
    && Array.all (\character -> CodeUnits.contains (Pattern (CodeUnits.singleton character)) "0123456789abcdefABCDEF") (CodeUnits.toCharArray digits)

hueSaturationValue :: String -> Maybe Color
hueSaturationValue written =
  let
    parts = Array.filter (_ /= "") (split (Pattern " ") (joinWithSpaces written))
  in
    case traverse fromString parts of
      Just [ hue, saturation, value ] | Array.all (\part -> part >= 0.0 && part <= 1.0) [ hue, saturation, value ] ->
        Just (fromHsv hue saturation value)
      _ -> Nothing
  where
  joinWithSpaces = CodeUnits.fromCharArray <<< map (\character -> if character == ',' then ' ' else character) <<< CodeUnits.toCharArray

fromHsv :: Number -> Number -> Number -> Color
fromHsv hue saturation value =
  let
    sector = hue * 6.0
    whole = floor sector
    fraction = sector - toNumber whole
    lowest = value * (1.0 - saturation)
    falling = value * (1.0 - saturation * fraction)
    rising = value * (1.0 - saturation * (1.0 - fraction))
    channel part = floor (part * 255.0 + 0.5)
    rgb red green blue = RGB (channel red) (channel green) (channel blue)
  in
    case whole `mod` 6 of
      0 -> rgb value rising lowest
      1 -> rgb falling value lowest
      2 -> rgb lowest value rising
      3 -> rgb lowest falling value
      4 -> rgb rising lowest value
      _ -> rgb value lowest falling

-- | The colour names of CSS Color Level 4. Graphviz's X11 names are mostly
-- | among them; the numbered variants (`dodgerblue1`) are not.
cssNames :: Set String
cssNames = Set.fromFoldable
  [ "aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige", "bisque", "black"
  , "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse"
  , "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan"
  , "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta"
  , "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen"
  , "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "deeppink"
  , "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen"
  , "fuchsia", "gainsboro", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow"
  , "grey", "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender"
  , "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan"
  , "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink", "lightsalmon"
  , "lightseagreen", "lightskyblue", "lightslategray", "lightslategrey", "lightsteelblue"
  , "lightyellow", "lime", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine"
  , "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue"
  , "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream"
  , "mistyrose", "moccasin", "navajowhite", "navy", "oldlace", "olive", "olivedrab", "orange"
  , "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred"
  , "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "rebeccapurple"
  , "red", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen"
  , "seashell", "sienna", "silver", "skyblue", "slateblue", "slategray", "slategrey", "snow"
  , "springgreen", "steelblue", "tan", "teal", "thistle", "tomato", "turquoise", "violet"
  , "wheat", "white", "whitesmoke", "yellow", "yellowgreen"
  ]
