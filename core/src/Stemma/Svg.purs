-- | A `Drawing` as SVG text.
-- |
-- | This is the plainest rendering that shows a layout: an ellipse and a
-- | label for each node, a line for each edge with an arrowhead if the
-- | graph is directed, a rectangle for each cluster. It reads the few
-- | attributes that change what is seen at that level (`shape=box`, colours,
-- | `style=dashed` or `invis`) and ignores the rest. A caller that wants
-- | more draws the `Drawing` itself.
module Stemma.Svg
  ( render
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Stemma.Dot.Syntax (Text(..))
import Stemma.Label as Label
import Stemma.Attributes (Attributes)
import Stemma.Attributes as Attributes
import Stemma.Layout (Box, Drawing, Outline(..), Point, outlineOf)

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
        Just _, Just at -> joinWith "" (stacked at (linesOf found.attributes (case Attributes.lookup "label" found.attributes of
          Just written -> written
          Nothing -> "")))
        _, _ -> ""

  node found =
    [ let
        box = found.box
        ellipse grow =
          "<ellipse cx=\"" <> number box.centre.x <> "\" cy=\"" <> number box.centre.y
            <> "\" rx=\"" <> number (box.width / 2.0 - grow) <> "\" ry=\"" <> number (box.height / 2.0 - grow) <> "\" " <> paint found.attributes <> "/>"
      in
        case outlineOf found.attributes of
          Rectangle -> rectangle box (paint found.attributes)
          NoOutline -> ""
          Diamond ->
            "<polygon points=\"" <> number box.centre.x <> "," <> number (box.centre.y - box.height / 2.0) <> " "
              <> number (box.centre.x + box.width / 2.0) <> "," <> number box.centre.y <> " "
              <> number box.centre.x <> "," <> number (box.centre.y + box.height / 2.0) <> " "
              <> number (box.centre.x - box.width / 2.0) <> "," <> number box.centre.y <> "\" " <> paint found.attributes <> "/>"
          DoubleEllipse -> ellipse 0.0 <> ellipse 4.0
          Ellipse -> ellipse 0.0
    , joinWith "" (stacked found.box.centre (linesOf found.attributes found.label))
    ]

  -- A label as lines, each with its size in points and whether it is
  -- bold: an HTML-like label by its markup, any other by its `\n`s.
  linesOf attributes written =
    let
      usual = case Attributes.lookup "fontsize" attributes >>= Number.fromString of
        Just points -> points
        Nothing -> 14.0
    in
      case Attributes.lookupText "label" attributes of
        Just (Html markup) | not (Label.hasTable markup) ->
          map (\line -> { text: line.text, bold: line.bold, size: case line.size of
            Just given -> given
            Nothing -> usual }) (Label.markupLines markup)
        _ -> map (\text -> { text, bold: false, size: usual })
          (Array.concatMap (split (Pattern "\\l")) (split (Pattern "\\n") written))

  -- The lines one under another, centred as a block on a point.
  stacked centre lines =
    let
      heightOf line = 1.2 * line.size
      total = Array.foldl (\soFar line -> soFar + heightOf line) 0.0 lines
      place state line =
        { top: state.top + heightOf line
        , drawn: Array.snoc state.drawn
            ( "<text x=\"" <> number centre.x <> "\" y=\"" <> number (state.top + heightOf line * 0.75) <> "\" text-anchor=\"middle\" font-size=\""
                <> number line.size <> "\"" <> (if line.bold then " font-weight=\"bold\"" else "") <> ">" <> escape line.text <> "</text>"
            )
        }
    in
      (Array.foldl place { top: centre.y - total / 2.0, drawn: [] } lines).drawn

  paint attributes = "fill=\"" <> colour "fillcolor" "white" attributes <> "\" stroke=\"" <> colour "color" "#333" attributes <> "\""

  -- The path is the start and then three points for each piece.
  curve path = case Array.uncons path of
    Just first -> "M " <> point first.head <> " C " <> joinWith " " (map point first.tail)
    Nothing -> ""

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
