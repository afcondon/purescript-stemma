-- | The Stemma engine, held to the renderer's own laws on made flows under
-- | every drawing, with and without words, and with groups open (side by
-- | side, and nested): the general laws and the one it promises, layers in
-- | line. Where no group is open it is held to `FlowsForward` too, which
-- | it does not promise of every layout (`squeezed` with groups open is a
-- | layout that breaks it; see `Stemma.Flow`).
module Test.Main (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), GroupId(..), ItemId(..), PortName(..), StepId(..), WireId(..), itemLabel, project, stepLabel)
import Hylograph.Flow.Grouping (Grouping(..))
import Data.Maybe (isNothing)
import Hylograph.Flow.Render.Engine (Picture, Promise(..), plainPicture, run)
import Hylograph.Flow.Render.Laws (check, promiseFaults)
import Stemma.Flow (defaultMetrics, stemma)
import Test.Picture (svg)

type Made = Flow Unit Unit Number

flowOf :: Array String -> Array { step :: String, reads :: Array String, writes :: Array String } -> Made
flowOf lone steps = Flow
  { items: Array.nub (lone <> Array.concatMap (\s -> s.reads <> s.writes) steps) <#> \i -> { id: ItemId i, name: Just i, count: 1, payload: unit }
  , steps: steps <#> \s -> { id: StepId s.step, name: Just s.step, inputs: map PortName s.reads, outputs: map PortName s.writes, payload: unit }
  , wires: steps # Array.concatMap \s ->
      (s.reads <#> \r -> { id: Reads (ItemId r) (StepId s.step) (PortName r), payload: 1.0 })
        <> (s.writes <#> \w -> { id: Writes (StepId s.step) (PortName w) (ItemId w), payload: 1.0 })
  }

subjects :: Array { name :: String, flow :: Made }
subjects =
  [ { name: "chain", flow: flowOf [] [ { step: "f", reads: [ "a" ], writes: [ "b" ] }, { step: "g", reads: [ "b" ], writes: [ "c" ] } ] }
  , { name: "long edge"
    , flow: flowOf []
        [ { step: "f", reads: [ "a" ], writes: [ "b" ] }
        , { step: "g", reads: [ "b" ], writes: [ "c" ] }
        , { step: "h", reads: [ "a", "c" ], writes: [ "d" ] }
        ]
    }
  , { name: "scan"
    , flow: flowOf []
        [ { step: "add", reads: [ "amount", "total" ], writes: [ "total" ] }
        , { step: "report", reads: [ "total" ], writes: [ "page" ] }
        ]
    }
  , { name: "tangled", flow: tangled }
  , { name: "ouroboros", flow: flowOf [] [ { step: "again", reads: [ "it" ], writes: [ "it" ] } ] }
  , { name: "unconnected", flow: flowOf [ "a", "b", "c", "d", "e", "f", "g" ] [] }
  , { name: "nothing", flow: flowOf [] [] }
  , { name: "wide", flow: wide }
  , { name: "squeezed", flow: squeezed }
  ]

tangled :: Made
tangled = flowOf [ "alone", "apart" ]
  [ { step: "f", reads: [ "a", "d" ], writes: [ "b" ] }
  , { step: "g", reads: [ "b" ], writes: [ "c" ] }
  , { step: "h", reads: [ "c" ], writes: [ "d" ] }
  , { step: "k", reads: [ "c", "e" ], writes: [ "e" ] }
  , { step: "m", reads: [ "x" ], writes: [ "y" ] }
  ]

-- | Four steps reading one item and feeding one step, so a rank has many
-- | nodes and edges cross clusters.
wide :: Made
wide = flowOf []
  [ { step: "p1", reads: [ "source" ], writes: [ "o1" ] }
  , { step: "p2", reads: [ "source" ], writes: [ "o2" ] }
  , { step: "p3", reads: [ "source", "o1" ], writes: [ "o3" ] }
  , { step: "p4", reads: [ "source" ], writes: [ "o4" ] }
  , { step: "join", reads: [ "o1", "o2", "o3", "o4" ], writes: [ "result" ] }
  , { step: "after", reads: [ "result", "source" ], writes: [ "final" ] }
  ]

-- | No cycle, and with `f`, `b` and `g` in one group there is no rank for
-- | `m` between them: it reads what `f` wrote and `g` reads what it wrote.
squeezed :: Made
squeezed = flowOf []
  [ { step: "f", reads: [ "a" ], writes: [ "b", "side" ] }
  , { step: "m", reads: [ "side" ], writes: [ "back" ] }
  , { step: "g", reads: [ "b", "back" ], writes: [ "c" ] }
  ]

drawings :: Array { name :: String, drawing :: Drawing }
drawings =
  [ { name: "Both", drawing: Both }
  , { name: "Holders", drawing: Holders }
  , { name: "Steps", drawing: Steps }
  , { name: "Chosen a to c", drawing: Chosen early }
  ]
  where
  early = case _ of
    ItemKey item -> itemLabel item <= "c"
    StepKey step -> stepLabel step <= "c"
    WireKey _ -> false

worded :: Picture -> Picture
worded picture = picture { words = Just \node -> { title: node.label <> " and a name long enough to wrap", detail: Just "some detail" } }

open :: Array Grouping -> Picture
open grouping = plainPicture { groups = Just { grouping, closed: Set.empty } }

-- | Every picture a flow is drawn under: plain; with words; and with these
-- | groups open, which name whatever of them the flow has.
pictures :: Array { name :: String, picture :: Picture }
pictures =
  [ { name: "plain", picture: plainPicture }
  , { name: "worded", picture: worded plainPicture }
  , { name: "two groups", picture: open sideBySide }
  , { name: "nested groups", picture: open nested }
  , { name: "nested groups, worded", picture: worded (open nested) }
  ]
  where
  sideBySide =
    [ Group (GroupId "left") "the left group" [ Member (StepId "f"), Holds (ItemId "a"), Member (StepId "p1"), Member (StepId "p2"), Holds (ItemId "o1") ]
    , Group (GroupId "right") "the right group" [ Member (StepId "h"), Holds (ItemId "d"), Member (StepId "p4"), Holds (ItemId "o4"), Member (StepId "add") ]
    ]
  nested =
    [ Group (GroupId "outer") "outer"
        [ Group (GroupId "inner one") "inner one" [ Member (StepId "f"), Holds (ItemId "b"), Member (StepId "g"), Member (StepId "p1"), Holds (ItemId "o1"), Member (StepId "again") ]
        , Group (GroupId "inner two") "inner two"
            [ Member (StepId "p3")
            , Group (GroupId "innermost") "innermost" [ Holds (ItemId "c"), Holds (ItemId "o3"), Holds (ItemId "total") ]
            ]
        , Member (StepId "p2")
        , Holds (ItemId "source")
        ]
    , Group (GroupId "beside") "beside" [ Member (StepId "join"), Holds (ItemId "result"), Member (StepId "k"), Holds (ItemId "e") ]
    ]

main :: Effect Unit
main = do
  results <- for_' cases \found -> do
    let
      projection = project found.drawing found.flow
      engine = stemma defaultMetrics
      laid = run engine found.picture projection
      faults = check engine found.picture projection laid
        <> (if isNothing found.picture.groups then promiseFaults FlowsForward found.picture projection laid else [])
    for_ faults \fault -> log ("    fault (" <> show fault.law <> "): " <> fault.words)
    log ((if Array.null faults then "  ok: " else "  FAILED: ") <> found.name)
    when (Array.elem found.name shown) do
      log ("<!-- picture: " <> found.name <> " -->")
      log (svg laid)
      log "<!-- end -->"
    pure (Array.null faults)
  let failed = Array.length (Array.filter not results)
  log (show (Array.length results - failed) <> " of " <> show (Array.length results) <> " layouts keep every law")
  when (failed > 0) (throw (show failed <> " layouts break a law"))
  where
  -- written out as SVG among the results, to be looked at
  shown = [ "wide, Both, nested groups, worded", "tangled, Both, two groups" ]

  cases = do
    subject <- subjects
    drawing <- drawings
    picture <- pictures
    pure { name: subject.name <> ", " <> drawing.name <> ", " <> picture.name, flow: subject.flow, drawing: drawing.drawing, picture: picture.picture }

  for_' :: forall a b. Array a -> (a -> Effect b) -> Effect (Array b)
  for_' items act = Array.foldM (\done item -> Array.snoc done <$> act item) [] items
