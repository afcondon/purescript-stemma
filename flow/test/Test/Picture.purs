-- | A layout as plain SVG, to look at: boxes, outlines with their names,
-- | routes, and each node's words.
module Test.Picture (svg) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.String (joinWith)
import Hylograph.Flow.Render.Engine (Layout)
import Hylograph.Flow.Render.Route (svgPath)

svg :: forall wire. Layout wire -> String
svg laid = joinWith "\n" $
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> toString laid.width <> "\" height=\"" <> toString laid.height <> "\" font-family=\"Helvetica, Arial, sans-serif\">" ]
    <> map outline laid.outlines
    <> map edge laid.edges
    <> map node laid.nodes
    <> [ "</svg>" ]
  where
  rectangle box rest = "<rect x=\"" <> toString box.x <> "\" y=\"" <> toString box.y <> "\" width=\"" <> toString box.width <> "\" height=\"" <> toString box.height <> "\" " <> rest <> "/>"
  written x y size rest words = "<text x=\"" <> toString x <> "\" y=\"" <> toString y <> "\" font-size=\"" <> toString size <> "\" " <> rest <> ">" <> words <> "</text>"
  outline found = rectangle found.box "fill=\"#f4f4f4\" fill-opacity=\"0.6\" stroke=\"#999\""
    <> written (found.box.x + 6.0) (found.box.y + 13.0) 11.0 "fill=\"#555\"" found.name
  edge found = "<path d=\"" <> svgPath found.route <> "\" fill=\"none\" stroke=\"#333\"/>"
  node found = rectangle found.box "fill=\"white\" stroke=\"#333\""
    <> case found.words of
      Just set -> joinWith "" (Array.mapWithIndex (\line words -> written (found.box.x + 8.0) (found.box.y + 16.0 + 14.0 * toNumber line) 11.0 "" words) (set.title <> set.detail))
      Nothing -> written (found.box.x + 8.0) (found.box.y + found.box.height / 2.0 + 4.0) 11.0 "" found.node.label
