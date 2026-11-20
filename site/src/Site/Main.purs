module Site.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Site.App (component)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
