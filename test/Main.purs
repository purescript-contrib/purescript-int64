module Test.Main where

import Prelude

import Data.Int64 as Int64
import Data.Int64 (Int64)
import Data.UInt64 as UInt64
import Data.UInt64 (UInt64)
import Data.Long.FFISpec (ffiSpec)
import Data.Long.InternalSpec (internalSpec)
import Data.Maybe (Maybe(..))
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
      shouldEqual
        (Just hundredbillion)
        (Int64.fromString "100000000000")
      shouldEqual (Int64.fromInt 0) zero
      shouldEqual
        (billion / hundred)
        (Int64.fromInt 10000000)
      shouldEqual (show $ Int64.lowBits hundredbillion) "1215752192"
      shouldEqual (show $ Int64.highBits hundredbillion) "23"
      shouldEqual true $ case (one :: Int64) of
        x | x == Int64.fromInt 1  -> true
        _ -> false
      shouldEqual (show (one :: UInt64)) "1ul"
