-- | The page: the family tree with its scrubber through the passes, then
-- | an editor with DOT text on the left, the library's drawing of it on the
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
import Site.Corpus (examples, familyTree, graphvizVersion)
import Site.Example (Example, ExampleName(..), Group, groupTitle, groups)
import Site.Measure as Measure
import Site.Passes as Passes
import Site.Picture (TurnedEdges(..), picture)
import Type.Proxy (Proxy(..))

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

type Slots = (passes :: forall query. H.Slot query Void Unit)

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

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
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

repository :: String
repository = "https://github.com/afcondon/purescript-graphviz"

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.class_ (H.ClassName "page") ]
    [ HH.header [ HP.class_ (H.ClassName "masthead") ]
        [ HH.h1 [ HP.class_ (H.ClassName "title") ] [ HH.text "Graphviz in PureScript" ]
        , HH.p [ HP.class_ (H.ClassName "subtitle") ]
            [ HH.text "Graphviz’s DOT language and its dot layout, rewritten in PureScript from the published algorithms, so a graph can be laid out in the browser with no C and no WebAssembly. Each pass of the layout is a separate value, so you can watch a drawing being made." ]
        , HH.p [ HP.class_ (H.ClassName "masthead__links") ]
            [ HH.a [ HP.href repository ] [ HH.text "Source on GitHub" ]
            , HH.text " · MIT licence"
            ]
        ]
    , HH.section [ HP.class_ (H.ClassName "feature") ]
        [ HH.div [ HP.class_ (H.ClassName "section-head") ]
            [ HH.h2 [ HP.class_ (H.ClassName "section-title") ] [ HH.text "Fairport Convention, 1967 to 1980" ]
            , HH.p [ HP.class_ (H.ClassName "section-lede") ]
                [ HH.text "A rock family tree, drawn by this library in your browser. Step through the four passes of the layout, or press play, and watch the boxes move as each pass does its work." ]
            ]
        , HH.slot_ (Proxy :: Proxy "passes") unit Passes.component familyTree
        ]
    , HH.section [ HP.class_ (H.ClassName "playground") ]
        [ HH.div [ HP.class_ (H.ClassName "section-head") ]
            [ HH.h2 [ HP.class_ (H.ClassName "section-title") ] [ HH.text "Try it" ]
            , HH.p [ HP.class_ (H.ClassName "section-lede") ]
                [ HH.text "Choose an example or write your own DOT. The drawing follows as you type." ]
            ]
        , status
        , picker state
        , HH.div [ HP.class_ (H.ClassName "workspace") ]
            [ editor state
            , ourPanel state
            , graphvizPanel (comparison state)
            ]
        ]
    , HH.footer [ HP.class_ (H.ClassName "colophon") ]
        [ HH.p_
            [ HH.text "Band membership data from "
            , HH.a [ HP.href "https://musicbrainz.org/" ] [ HH.text "MusicBrainz" ]
            , HH.text ", whose core data is in the public domain (CC0); thank you to its editors. The idea of a rock family tree is Pete Frame’s, whose hand-drawn trees of the 1970s onwards drew the comings and goings of bands; this one is drawn by a program and makes no attempt to look like his."
            ]
        , HH.p_
            [ HH.text "The other examples are this project’s own. The drawings marked Graphviz are made by the installed Graphviz at build time, from those same files. "
            , HH.a [ HP.href repository ] [ HH.text "github.com/afcondon/purescript-graphviz" ]
            ]
        ]
    ]

-- | What the reader is looking at, from the README.
status :: forall w i. HH.HTML w i
status =
  HH.div [ HP.class_ (H.ClassName "status") ]
    [ HH.p [ HP.class_ (H.ClassName "status__main") ]
        [ HH.text "Ranks, the order within each rank, coordinates and edge routing all come from the real passes. Not yet: cluster boxes are not kept clear of edges, node shapes are reduced to five outlines, and record and HTML labels are sized but not laid out inside." ]
    , HH.p [ HP.class_ (H.ClassName "status__figures") ]
        [ HH.text "On the library’s test corpus of 884 graphs, per its README: every ranking proved optimal; as few crossings as dot or fewer on 801; every drawing keeps the geometry rules (no overlaps, edges end on outlines)." ]
    ]

picker :: forall m. State -> H.ComponentHTML Action Slots m
picker state =
  HH.nav [ HP.class_ (H.ClassName "picker") ]
    (map pickerGroup groups)
  where
  pickerGroup :: Group -> H.ComponentHTML Action Slots m
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

editor :: forall m. State -> H.ComponentHTML Action Slots m
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

ourPanel :: forall m. State -> H.ComponentHTML Action Slots m
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
                    [ HH.text "Hover over a node or edge for its name." ]
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

graphvizPanel :: forall m. Comparison -> H.ComponentHTML Action Slots m
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
