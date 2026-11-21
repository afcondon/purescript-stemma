-- | The family tree, with a scrubber through the passes of the layout.
-- |
-- | Graphviz hands back a finished picture. Here each pass is a separate
-- | value (`Graphviz.Layout.drawAt`), so the page can show the drawing as
-- | it stands after each one, and move the boxes from one to the next.
module Site.Passes
  ( component
  , FamilyTree
  , Band
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Graphviz.Dot.Parser (parse)
import Graphviz.Graph (fromDot)
import Graphviz.Layout (Drawing, DrawnNode, Stage(..), drawAt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..), Namespace(..), ElemName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Site.Measure as Measure
import Site.Picture (TurnedEdges(..), arrowhead, cluster, edge, nameOf, node)

-- | A band in the legend, with the colour its line-ups are drawn in.
type Band = { name :: String, colour :: String }

-- | The family tree's DOT text and what the page says about where it came
-- | from. Generated at build time from `site/examples/fairport/`.
type FamilyTree =
  { dot :: String
  , source :: String
  , fetched :: String
  , bands :: Array Band
  }

-- | One drawing for each pass, all made when the page loads: they take a
-- | fraction of a second for a graph this size, and stepping between them
-- | must not wait on the layout.
type Drawings =
  { ranked :: Drawing
  , ordered :: Drawing
  , positioned :: Drawing
  , routed :: Drawing
  }

drawingAt :: Stage -> Drawings -> Drawing
drawingAt = case _ of
  Ranked -> _.ranked
  Ordered -> _.ordered
  Positioned -> _.positioned
  Routed -> _.routed

-- | Whether the scrubber is stepping by itself. `Playing` carries the
-- | number of the run, so that a step that was waiting when the reader
-- | pressed a button does nothing.
data Playback = Playing Int | Stopped

derive instance eqPlayback :: Eq Playback

type State =
  { tree :: FamilyTree
  , drawings :: Either String Drawings
  , shown :: Stage
  , playback :: Playback
  , runs :: Int
  }

data Action
  = Show Stage
  | Play
  | Stop
  | Advance Int

stages :: Array Stage
stages = [ Ranked, Ordered, Positioned, Routed ]

following :: Stage -> Maybe Stage
following = case _ of
  Ranked -> Just Ordered
  Ordered -> Just Positioned
  Positioned -> Just Routed
  Routed -> Nothing

stageName :: Stage -> String
stageName = case _ of
  Ranked -> "Ranked"
  Ordered -> "Ordered"
  Positioned -> "Positioned"
  Routed -> "Routed"

-- | What each pass did, for a reader who has never heard of Graphviz.
caption :: Stage -> String
caption = case _ of
  Ranked -> "Each box is put on the row for its year. Within a row the boxes simply stand in the order the file happens to list them, so the lines cross wherever they like."
  Ordered -> "Within each row the boxes are shuffled, again and again, towards the boxes they are joined to above and below, until as few lines cross as the search can find."
  Positioned -> "Every box now gets its exact place across the page: pulled sideways to sit close to the boxes it is joined to, so that lines run as straight as they can without boxes overlapping."
  Routed -> "Last, each line becomes a smooth curve that threads its way between the boxes it passes and stops at their edges. This is the finished picture."

-- | How long each step stays on screen while playing: long enough for the
-- | boxes to arrive and the caption to be read.
stepTime :: Milliseconds
stepTime = Milliseconds 2600.0

component :: forall query output m. MonadAff m => H.Component query FamilyTree output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: FamilyTree -> State
initialState tree =
  { tree
  , drawings: do
      graph <- fromDot <$> parse tree.dot
      ranked <- drawAt Ranked Measure.options graph
      ordered <- drawAt Ordered Measure.options graph
      positioned <- drawAt Positioned Measure.options graph
      routed <- drawAt Routed Measure.options graph
      pure { ranked, ordered, positioned, routed }
  -- The finished picture first: it is what a reader expects to see, and
  -- the buttons take it apart.
  , shown: Routed
  , playback: Stopped
  , runs: 0
  }

handleAction :: forall slots output m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Show stage -> H.modify_ _ { shown = stage, playback = Stopped }
  Stop -> H.modify_ _ { playback = Stopped }
  Play -> do
    state <- H.modify \state -> state
      { runs = state.runs + 1
      -- From the finished picture, playing starts again from the top.
      , shown = if state.shown == Routed then Ranked else state.shown
      }
    H.modify_ _ { playback = Playing state.runs }
    wait state.runs
  Advance run -> do
    state <- H.get
    when (state.playback == Playing run) case following state.shown of
      Just next -> do
        H.modify_ _ { shown = next }
        if following next == Nothing then H.modify_ _ { playback = Stopped } else wait run
      Nothing -> H.modify_ _ { playback = Stopped }
  where
  wait run = void $ H.fork do
    H.liftAff (delay stepTime)
    handleAction (Advance run)

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ HP.class_ (ClassName "passes") ]
    [ HH.div [ HP.class_ (ClassName "passes__controls") ]
        [ HH.button
            [ HP.class_ (ClassName "play")
            , HE.onClick \_ -> if state.playback == Stopped then Play else Stop
            , ARIA.label (if state.playback == Stopped then "Play through the passes" else "Stop")
            ]
            [ HH.text (if state.playback == Stopped then "▶ Play" else "■ Stop") ]
        , HH.ol [ HP.class_ (ClassName "steps") ] (Array.mapWithIndex step stages)
        ]
    , HH.p [ HP.class_ (ClassName "passes__caption"), ARIA.live "polite" ]
        [ HH.strong_ [ HH.text (stageName state.shown <> ". ") ]
        , HH.text (caption state.shown)
        ]
    , case state.drawings of
        Left message ->
          HH.div [ HP.class_ (ClassName "failure") ]
            [ HH.p [ HP.class_ (ClassName "failure__title") ] [ HH.text "The family tree could not be laid out." ]
            , HH.pre [ HP.class_ (ClassName "failure__message") ] [ HH.text message ]
            ]
        Right drawings -> HH.div [ HP.class_ (ClassName "passes__frame") ] [ stagePicture state.shown drawings ]
    , legend state.tree
    ]
  where
  step position stage =
    HH.li_
      [ HH.button
          [ HP.classes (map ClassName ([ "step" ] <> if stage == state.shown then [ "step--shown" ] else []))
          , HE.onClick \_ -> Show stage
          , HP.attr (AttrName "aria-current") (if stage == state.shown then "step" else "false")
          ]
          [ HH.span [ HP.class_ (ClassName "step__number") ] [ HH.text (show (position + 1)) ]
          , HH.text (stageName stage)
          ]
      ]

legend :: forall w i. FamilyTree -> HH.HTML w i
legend tree =
  HH.div [ HP.class_ (ClassName "passes__legend") ]
    [ HH.ul [ HP.class_ (ClassName "bands") ]
        ( map
            ( \band -> HH.li [ HP.class_ (ClassName "band") ]
                [ HH.span [ HP.class_ (ClassName "band__swatch"), HP.attr (AttrName "style") ("border-color: " <> band.colour) ] []
                , HH.text band.name
                ]
            )
            tree.bands
        )
    , HH.p [ HP.class_ (ClassName "passes__note") ]
        [ HH.text "Each box is a band in a year its line-up changed, with who was in it that year. Bold lines carry a band on; thin ones are people leaving for another band. Bands are told apart by colour alone: this layout cannot yet keep a band in a box of its own while also lining the years up across the page. "
        , HH.text ("Membership from MusicBrainz, fetched " <> tree.fetched <> "; a few undated memberships filled in by hand (" <> tree.source <> ").")
        ]
    ]

svgNamespace :: Namespace
svgNamespace = Namespace "http://www.w3.org/2000/svg"

-- | All four drawings share one frame, as big as the biggest, and each is
-- | centred in it across the page, so that the frame does not jump as the
-- | reader steps and a box moves only as far as the pass moved it.
stagePicture :: forall w i. Stage -> Drawings -> HH.HTML w i
stagePicture shown drawings =
  SE.svg
    [ SA.viewBox 0.0 0.0 width height
    , SA.width (width * 96.0 / 72.0)
    , SA.height (height * 96.0 / 72.0)
    , SA.class_ (ClassName "drawing")
    ]
    [ SE.defs [] [ arrowhead ]
    -- Keyed by stage, so that the lines of a new stage are new elements
    -- and fade in, rather than snapping from one shape to another.
    , HH.keyedNS svgNamespace (ElemName "g") []
        [ Tuple (stageName shown)
            ( SE.g [ SA.class_ (ClassName "passes__lines"), SA.transform [ SA.Translate offset 0.0 ] ]
                ( map cluster drawing.clusters
                    <> map (edge Unmarked drawing.directed) drawing.edges
                )
            )
        ]
    -- Keyed by node name, so that each box is the same element at every
    -- stage and the stylesheet's transition can glide it to its new place.
    , HH.keyedNS svgNamespace (ElemName "g") []
        (map placed drawing.nodes)
    ]
  where
  all = map (\stage -> drawingAt stage drawings) stages
  width = fromMaybe 0.0 (maximum (map _.width all))
  height = fromMaybe 0.0 (maximum (map _.height all))
  drawing = drawingAt shown drawings
  offset = (width - drawing.width) / 2.0
  placed :: DrawnNode -> Tuple String (HH.HTML w i)
  placed found =
    Tuple (nameOf found.name)
      ( SE.g
          [ SA.class_ (ClassName "glide")
          -- A CSS transform and not the SVG attribute, since only the
          -- former is animated by a CSS transition.
          , HP.attr (AttrName "style")
              ("transform: translate(" <> show (found.box.centre.x + offset) <> "px, " <> show found.box.centre.y <> "px)")
          ]
          [ node found { box = found.box { centre = { x: 0.0, y: 0.0 } } } ]
      )
