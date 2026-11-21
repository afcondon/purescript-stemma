-- | A `Drawing` as SVG text.
-- |
-- | This is the plainest rendering that shows a layout: an ellipse and a
-- | label for each node, a line for each edge with an arrowhead if the
-- | graph is directed, a rectangle for each cluster. It reads the few
-- | attributes that change what is seen at that level (`shape=box`, colours,
-- | `style=dashed` or `invis`) and ignores the rest. A caller that wants
-- | more draws the `Drawing` itself.
module Graphviz.Svg
  ( render
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Graphviz.Attributes (Attributes)
import Graphviz.Attributes as Attributes
import Graphviz.Layout (Box, Drawing, Point)

render :: Drawing -> String
render drawing = joinWith "\n"
  ( [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> number (drawing.width * drawing.scale) <> "pt\" height=\"" <> number (drawing.height * drawing.scale) <> "pt"
        <> "\" viewBox=\"0 0 " <> number drawing.width <> " " <> number drawing.height <> "\" font-family=\"Helvetica, Arial, sans-serif\" font-size=\"14\">"
    , "<defs><marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\"><path d=\"M 0 0 L 10 5 L 0 10 z\"/></marker></defs>"
    ]
      <> map cluster drawing.clusters
      <> map edge (Array.filter (_.attributes >>> visible) drawing.edges)
      <> Array.concatMap node (Array.filter (_.attributes >>> visible) drawing.nodes)
      <> [ "</svg>" ]
  )
  where
  cluster found = joinWith ""
    [ rectangle found.box
        ( "fill=\"" <> (if Attributes.lookup "style" found.attributes == Just "filled" then colour "color" "#eee" found.attributes else "none")
            <> "\" stroke=\"" <> colour "color" "#888" found.attributes <> "\""
        )
    , case found.label of
        Just written -> "<text x=\"" <> number found.box.centre.x <> "\" y=\"" <> number (found.box.centre.y - found.box.height / 2.0 + 14.0)
          <> "\" text-anchor=\"middle\" fill=\"#555\">" <> escape written <> "</text>"
        Nothing -> ""
    ]

  edge found =
    "<path fill=\"none\" stroke=\"" <> colour "color" "#333" found.attributes <> "\""
      <> (if dashed found.attributes then " stroke-dasharray=\"5,4\"" else "")
      <> (if drawing.directed then " marker-end=\"url(#arrow)\"" else "")
      <> " d=\"" <> curve found.path <> "\"/>"
      <> case Attributes.lookup "label" found.attributes, found.labelAt of
        Just written, Just at -> "<text x=\"" <> number at.x <> "\" y=\"" <> number (at.y + 4.0) <> "\" text-anchor=\"middle\" font-size=\"12\">" <> escape written <> "</text>"
        _, _ -> ""

  node found =
    [ if Attributes.lookup "shape" found.attributes == Just "box" || Attributes.lookup "shape" found.attributes == Just "record" then
        rectangle found.box (paint found.attributes)
      else
        "<ellipse cx=\"" <> number found.box.centre.x <> "\" cy=\"" <> number found.box.centre.y
          <> "\" rx=\"" <> number (found.box.width / 2.0) <> "\" ry=\"" <> number (found.box.height / 2.0) <> "\" " <> paint found.attributes <> "/>"
    , "<text x=\"" <> number found.box.centre.x <> "\" y=\"" <> number (found.box.centre.y + 5.0) <> "\" text-anchor=\"middle\">" <> escape found.label <> "</text>"
    ]

  paint attributes = "fill=\"" <> colour "fillcolor" "white" attributes <> "\" stroke=\"" <> colour "color" "#333" attributes <> "\""

  -- The path is the start and then three points for each piece.
  curve path = case Array.uncons path of
    Just first -> "M " <> point first.head <> " C " <> joinWith " " (map point first.tail)
    Nothing -> ""

-- A point moved a distance towards another, or not at all if they are
-- closer than that.
towards :: Point -> Point -> Number -> Point
towards from to distance =
  if length <= distance then from
  else { x: from.x + (to.x - from.x) * distance / length, y: from.y + (to.y - from.y) * distance / length }
  where
  across = to.x - from.x
  down = to.y - from.y
  length = sqrt (across * across + down * down)

rectangle :: Box -> String -> String
rectangle box rest =
  "<rect x=\"" <> number (box.centre.x - box.width / 2.0) <> "\" y=\"" <> number (box.centre.y - box.height / 2.0)
    <> "\" width=\"" <> number box.width <> "\" height=\"" <> number box.height <> "\" " <> rest <> "/>"

point :: Point -> String
point found = number found.x <> "," <> number found.y

number :: Number -> String
number = toStringWith (fixed 1)

visible :: Attributes -> Boolean
visible attributes = Attributes.lookup "style" attributes /= Just "invis"

dashed :: Attributes -> Boolean
dashed attributes = Attributes.lookup "style" attributes == Just "dashed"

colour :: String -> String -> Attributes -> String
colour key otherwise attributes = case Attributes.lookup key attributes of
  Just given -> escape given
  Nothing -> otherwise

escape :: String -> String
escape = replaceAll (Pattern "<") (Replacement "&lt;") <<< replaceAll (Pattern ">") (Replacement "&gt;")
  <<< replaceAll (Pattern "\"") (Replacement "&quot;")
  <<< replaceAll (Pattern "&") (Replacement "&amp;")
