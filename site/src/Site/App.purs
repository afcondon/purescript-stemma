-- | The page: DOT text on the left, the library's drawing of it on the
-- | right, and Graphviz's own drawing of a bundled example beside it.
module Site.App
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Graphviz.Layout (Drawing, layoutWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Site.Corpus (examples, graphvizVersion)
import Site.Example (Example, ExampleName(..), Group, groupTitle, groups)
import Site.Measure as Measure
import Site.Picture (TurnedEdges(..), picture)

-- | `generation` counts edits, so that a delayed re-layout can tell
-- | whether another edit has come in since it was asked for.
type State =
  { written :: String
  , chosen :: Maybe Example
  , outcome :: Either String Drawing
  , generation :: Int
  , turnedEdges :: TurnedEdges
  }

data Action
  = Choose Example
  | Edit String
  | Settle Int
  | Revert
  | ToggleTurnedEdges

-- | What the comparison panel can show. Graphviz's drawing exists only for
-- | the text of a bundled example, since it was made at build time.
data Comparison
  = Unedited Example
  | Edited Example
  | NothingChosen

comparison :: State -> Comparison
comparison state = case state.chosen of
  Just example | example.dot == state.written -> Unedited example
  Just example -> Edited example
  Nothing -> NothingChosen

-- | How long typing must pause before the text is laid out again.
settleTime :: Milliseconds
settleTime = Milliseconds 250.0

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = case Array.head examples of
  Just example ->
    { written: example.dot, chosen: Just example, outcome: layoutWith Measure.options example.dot, generation: 0, turnedEdges: Marked }
  Nothing ->
    { written: "", chosen: Nothing, outcome: layoutWith Measure.options "", generation: 0, turnedEdges: Marked }

handleAction :: forall slots output m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Choose example -> load example
  Revert -> do
    state <- H.get
    case state.chosen of
      Just example -> load example
      Nothing -> pure unit
  Edit written -> do
    state <- H.modify \state -> state { written = written, generation = state.generation + 1 }
    let generation = state.generation
    void $ H.fork do
      H.liftAff (delay settleTime)
      handleAction (Settle generation)
  Settle generation -> do
    state <- H.get
    when (state.generation == generation) do
      H.modify_ _ { outcome = layoutWith Measure.options state.written }
  ToggleTurnedEdges ->
    H.modify_ \state -> state
      { turnedEdges = case state.turnedEdges of
          Marked -> Unmarked
          Unmarked -> Marked
      }
  where
  -- Loading an example lays it out at once, and moves the generation on
  -- so that a re-layout still waiting from earlier typing does nothing.
  load example =
    H.modify_ \state -> state
      { written = example.dot
      , chosen = Just example
      , outcome = layoutWith Measure.options example.dot
      , generation = state.generation + 1
      }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "page") ]
    [ HH.header [ HP.class_ (H.ClassName "masthead") ]
        [ HH.h1 [ HP.class_ (H.ClassName "title") ] [ HH.text "Graphviz in PureScript" ]
        , HH.p [ HP.class_ (H.ClassName "subtitle") ]
            [ HH.text "Graphviz’s DOT language and its dot layout, written in PureScript and checked stage by stage against Graphviz. Edit the text; the drawing follows." ]
        ]
    -- Outside the masthead so that it can stay pinned to the top of the
    -- window while the title scrolls away.
    , status
    , picker state
    , HH.main [ HP.class_ (H.ClassName "workspace") ]
        [ editor state
        , ourPanel state
        , graphvizPanel (comparison state)
        ]
    , HH.footer [ HP.class_ (H.ClassName "colophon") ]
        [ HH.text "The bundled examples are from the library’s test corpus; those under “Graphviz’s examples” ship with Graphviz and carry its licence (EPL 1.0)." ]
    ]

-- | Always visible: what the reader is looking at, from the README.
status :: forall w i. HH.HTML w i
status =
  HH.div [ HP.class_ (H.ClassName "status") ]
    [ HH.p [ HP.class_ (H.ClassName "status__main") ]
        [ HH.strong_ [ HH.text "Real: " ]
        , HH.text "which rank each node is on, and the order within each rank, come from the real passes. "
        , HH.strong_ [ HH.text "Interim: " ]
        , HH.text "coordinates and edge routing. Each rank is spaced out and centred, and edges are straight pieces through their bends, so node positions and edge shapes will differ from Graphviz’s for now."
        ]
    , HH.p [ HP.class_ (H.ClassName "status__figures") ]
        [ HH.text "On the library’s corpus, per its README: 864 of 864 rankings proved optimal at every level. Ordering, of 884 graphs: the same number of crossings as dot on 709, fewer on 92, more on 83." ]
    ]

picker :: forall m. State -> H.ComponentHTML Action () m
picker state =
  HH.nav [ HP.class_ (H.ClassName "picker") ]
    (map pickerGroup groups)
  where
  pickerGroup :: Group -> H.ComponentHTML Action () m
  pickerGroup group =
    HH.div [ HP.class_ (H.ClassName "picker__group") ]
      [ HH.h2 [ HP.class_ (H.ClassName "picker__title") ] [ HH.text (groupTitle group) ]
      , HH.ul [ HP.class_ (H.ClassName "picker__list") ]
          (map choice (Array.filter (\example -> example.group == group) examples))
      ]
  choice example =
    HH.li_
      [ HH.button
          [ HP.classes (map H.ClassName ([ "picker__choice" ] <> if isChosen example then [ "picker__choice--chosen" ] else []))
          , HP.title example.source
          , HE.onClick \_ -> Choose example
          ]
          [ HH.text (nameText example.name) ]
      ]
  isChosen example = case state.chosen of
    Just chosen -> chosen.source == example.source
    Nothing -> false

nameText :: ExampleName -> String
nameText (ExampleName written) = written

editor :: forall m. State -> H.ComponentHTML Action () m
editor state =
  HH.section [ HP.class_ (H.ClassName "panel panel--editor") ]
    [ HH.div [ HP.class_ (H.ClassName "panel__head") ]
        [ HH.h2 [ HP.class_ (H.ClassName "panel__title") ] [ HH.text "DOT" ]
        , HH.span [ HP.class_ (H.ClassName "panel__note") ] (sourceNote (comparison state))
        ]
    , HH.textarea
        [ HP.class_ (H.ClassName "editor")
        , HP.value state.written
        , HP.spellcheck false
        , HE.onValueInput Edit
        ]
    ]
  where
  sourceNote = case _ of
    Unedited example -> [ HH.text example.source ]
    Edited example ->
      [ HH.text (example.source <> ", edited ")
      , HH.button [ HP.class_ (H.ClassName "link-button"), HE.onClick \_ -> Revert ] [ HH.text "Revert" ]
      ]
    NothingChosen -> []

ourPanel :: forall m. State -> H.ComponentHTML Action () m
ourPanel state =
  HH.section [ HP.class_ (H.ClassName "panel panel--ours") ]
    ( [ HH.div [ HP.class_ (H.ClassName "panel__head") ]
          [ HH.h2 [ HP.class_ (H.ClassName "panel__title") ] [ HH.text "This library" ]
          , HH.span [ HP.class_ (H.ClassName "panel__note") ] [ HH.text summary ]
          ]
      ]
        <> case state.outcome of
          Left message ->
            [ HH.div [ HP.class_ (H.ClassName "failure") ]
                [ HH.p [ HP.class_ (H.ClassName "failure__title") ] [ HH.text "The library could not lay this out." ]
                , HH.pre [ HP.class_ (H.ClassName "failure__message") ] [ HH.text message ]
                ]
            ]
          Right drawing ->
            [ HH.div [ HP.class_ (H.ClassName "frame") ] [ picture state.turnedEdges drawing ]
            , HH.div [ HP.class_ (H.ClassName "legend") ]
                [ HH.label [ HP.class_ (H.ClassName "legend__toggle") ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked (state.turnedEdges == Marked)
                        , HE.onChange \_ -> ToggleTurnedEdges
                        ]
                    , HH.span [ HP.class_ (H.ClassName "legend__swatch") ] []
                    , HH.text "Mark edges turned round to break a cycle (they point up the page)"
                    ]
                , HH.p [ HP.class_ (H.ClassName "legend__note") ]
                    [ HH.text "Edge labels are put at the middle of each path by this page; the layout does not place them yet. Hover over a node or edge for its name." ]
                ]
            ]
    )
  where
  summary = case state.outcome of
    Left _ -> "no drawing"
    Right drawing ->
      Array.intercalate " · "
        ( [ count (Array.length drawing.nodes) "node" "nodes"
          , count (Array.length drawing.edges) "edge" "edges"
          , count (Array.length drawing.clusters) "cluster" "clusters"
          ]
            <> let turned = Array.length (Array.filter _.turned drawing.edges) in
              if turned > 0 then [ count turned "edge turned" "edges turned" ] else []
        )
  count number one many = show number <> " " <> if number == 1 then one else many

graphvizPanel :: forall m. Comparison -> H.ComponentHTML Action () m
graphvizPanel shown =
  HH.section [ HP.class_ (H.ClassName "panel panel--graphviz") ]
    ( [ HH.div [ HP.class_ (H.ClassName "panel__head") ]
          [ HH.h2 [ HP.class_ (H.ClassName "panel__title") ] [ HH.text ("Graphviz " <> graphvizVersion) ]
          , HH.span [ HP.class_ (H.ClassName "panel__note") ] [ HH.text "dot -Tsvg, drawn at build time" ]
          ]
      ]
        <> case shown of
          Unedited example ->
            [ HH.div [ HP.class_ (H.ClassName "frame frame--graphviz") ]
                [ HH.img
                    [ HP.src example.graphvizPicture
                    , HP.alt ("Graphviz " <> graphvizVersion <> "’s drawing of " <> example.source)
                    ]
                ]
            ]
          Edited _ -> [ absent "The text has been edited. Graphviz’s drawing exists only for the bundled examples, since the browser has no Graphviz; revert the text or choose an example to see it again." ]
          NothingChosen -> [ absent "Graphviz’s drawing exists only for the bundled examples, since the browser has no Graphviz." ]
    )
  where
  absent message = HH.p [ HP.class_ (H.ClassName "absent") ] [ HH.text message ]
