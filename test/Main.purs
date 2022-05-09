module Test.Main where

import Prelude

import Data.Int64 as Int64
import Data.Long.FFISpec (ffiSpec)
import Data.Long.InternalSpec (internalSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  ffiSpec
  internalSpec
  describe "Examples" do
    it "Example1" do
      let
        hundred = Int64.fromInt 100
        billion = Int64.fromInt 1000000000
        hundredbillion = hundred * billion
      shouldEqual
        (show hundredbillion)
        ("100000000000l")
      shouldEqual (Int64.fromInt 0) zero
      shouldEqual
        (billion / hundred)
        (Int64.fromInt 10000000)
      shouldEqual (show $ Int64.lowBits hundredbillion) "1215752192"
      shouldEqual (show $ Int64.highBits hundredbillion) "23"
