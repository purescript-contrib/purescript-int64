module Test.Main where

import Prelude

import Data.Long.FFISpec (ffiSpec)
import Data.Long.InternalSpec (internalSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  ffiSpec
  internalSpec
