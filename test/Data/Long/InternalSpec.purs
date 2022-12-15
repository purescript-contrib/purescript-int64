module Data.Long.InternalSpec
  ( internalSpec
  ) where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Int (Parity(..), Radix, binary, decimal, hexadecimal, octal, radix)
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.Int64.Gen (chooseInt64)
import Data.Int64.Internal (class SInfo, Long', SignProxy(..), Signed, Unsigned)
import Data.Int64.Internal as Internal
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Number as Number
import Data.Ord (abs)
import Data.Traversable (traverse_)
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Data.UInt64.Gen (chooseUInt64)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, class Testable, arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Laws.Data (checkBoundedGen, checkCommutativeRingGen, checkEqGen, checkEuclideanRingGen, checkOrdGen, checkRingGen, checkSemiringGen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

internalSpec :: Spec Unit
internalSpec = do
  longSpec
  fromStringSpec

longSpec :: Spec Unit
longSpec = describe "Long" do
  it "should follow laws" $ liftEffect do
    checkEqGen $ chooseInt64 bottom top
    checkOrdGen $ chooseInt64 bottom top
    checkBoundedGen $ chooseInt64 bottom top
    checkSemiringGen $ chooseInt64 bottom top
    checkRingGen $ chooseInt64 bottom top
    checkCommutativeRingGen $ chooseInt64 bottom top
    -- `mod` is only lawful if the divisor is in 32-bit Int range.
    checkEuclideanRingGen $ chooseInt64 (Int64.fromInt bottom) (Int64.fromInt top)

    checkEqGen $ chooseUInt64 bottom top
    checkOrdGen $ chooseUInt64 bottom top
    checkBoundedGen $ chooseUInt64 bottom top
    checkSemiringGen $ chooseUInt64 bottom top
    checkRingGen $ chooseUInt64 bottom top
    checkCommutativeRingGen $ chooseUInt64 bottom top
    -- `mod` is only lawful if the divisor is in 32-bit Int range.
    checkEuclideanRingGen $ chooseUInt64 zero (UInt64.unsafeFromInt top)

  it "should be built from high and low bits" do
    quickCheck' \high low ->
      let
        l = Int64.fromLowHighBits low high :: Int64
      in
        Int64.highBits l == high && Int64.lowBits l == low

  it "should convert ints" $ do
    quickCheck' \i -> Internal.toInt (Internal.signedLongFromInt i) == Just i
    quickCheck' \i' ->
      let
        i = abs i'
        l = Internal.unsignedLongFromInt (abs i)
      in
        (l >>= Internal.toInt) == Just i

  it "should fail to convert negative ints to unsigned longs" do
    Internal.unsignedLongFromInt (-1) `shouldSatisfy` isNothing

  it "should convert to strings" $ do
    quickCheck' \(Radix' r) (Int64' l) ->
      readSigned r (Int64.toStringAs r l) == Just l

    quickCheck' \(Radix' r) (UInt64' l) ->
      readUnsigned r (UInt64.toStringAs r l) == Just l

  it "should convert numbers" $ do
    traverse_ (checkNumber signedProxy)
      [ 0.0
      , 9223372036854775807.0
      , (-9223372036854775808.0)
      , 2e10
      ]

    traverse_ (checkNumber unsignedProxy)
      [ 0.0
      , 9223372036854775807.0
      , (18446744073709551615.0)
      , 2e10
      ]

  it "should reject conversion from non whole numbers" $ do
    traverse_ (\n -> (Int64.fromNumber n :: Maybe Int64) `shouldSatisfy` isNothing)
      [ 5.5
      , 100.1
      , 200.25
      , 0.000001
      -- , 999999999.00000001 -- Number is not precise enough to store the decimal part
      , Number.nan
      , Number.infinity
      ]

  it "should reject conversion of numbers outside the long range" $ do
    traverse_ (\n -> (Int64.fromNumber n :: Maybe Int64) `shouldSatisfy` isNothing)
      [ -10000000000000000000.0 -- Must be big enough to store precision
      , 10000000000000000000.0
      ]

    traverse_ (\n -> (UInt64.fromNumber n :: Maybe UInt64) `shouldSatisfy` isNothing)
      [ -1.0
      , 20000000000000000000.0
      ]

  it "should determine odd/even" do
    quickCheck' \(Int64' l) -> (Int64.parity l == Even) == (l `mod` (Int64.fromInt 2) == zero)
    quickCheck' \(UInt64' l) -> (UInt64.parity l == Even) == (l `mod` (UInt64.unsafeFromInt 2) == zero)

  it "Int64 should always have positive mods" do
    -- `mod` is only lawful if the divisor is in 32-bit Int range.
    quickCheck' \(Int64' l1) l2 -> (l1 `mod` Int64.fromInt l2) > zero <?> show l1 <> " `mod` " <> show l2

  it "UInt64 should always have positive mods" do
    -- `mod` is only lawful if the divisor is in 32-bit Int range.
    quickCheck' \(UInt64' l1) l2 -> (l1 `mod` UInt64.unsafeFromInt (abs l2)) > zero <?> show l1 <> " `mod` " <> show l2

  it "should div, quot, mod, rem by 0 be 0" do
    traverse_ (\f -> f (Internal.signedLongFromInt 2) zero `shouldEqual` zero)
      [ div, Internal.quot, mod, Internal.rem ]

checkNumber :: forall s. SInfo s => Bounded (Long' s) => SignProxy s -> Number -> Aff Unit
checkNumber _ n =
  (Internal.toNumber <$> (Internal.fromNumber n :: Maybe (Long' s))) `shouldEqual` Just n

fromStringSpec :: Spec Unit
fromStringSpec = describe "fromString" do
  it "should leave valid strings unchanged" do
    readSigned decimal "12345" `shouldEqual` Just (i2lS 12345)
    readSigned decimal "+12345" `shouldEqual` Just (i2lS 12345)
    readSigned decimal "-12345" `shouldEqual` Just (i2lS (-12345))
    readSigned decimal "-12345" `shouldEqual` Just (i2lS (-12345))

  it "should read signed zeros" do
    readSigned decimal "0" `shouldEqual` Just zero
    readSigned decimal "-0" `shouldEqual` Just zero
    readSigned decimal "000" `shouldEqual` Just zero
    readSigned decimal "-00" `shouldEqual` Just zero

  it "should read unsigned zeros" do
    readUnsigned decimal "0" `shouldEqual` Just zero
    readUnsigned decimal "-0" `shouldEqual` Just zero
    readUnsigned decimal "000" `shouldEqual` Just zero
    readUnsigned decimal "-00" `shouldEqual` Just zero

  it "should return Nothing on empty string" do
    readSigned decimal "" `shouldEqual` Nothing

  it "should disallow negative for unsigned" do
    readUnsigned decimal "-123" `shouldEqual` Nothing

  it "should disallow invalid characters depending on radix" do
    readSigned binary "1010" `shouldSatisfy` isJust
    readSigned binary "-1010" `shouldSatisfy` isJust
    readSigned binary "1012" `shouldSatisfy` isNothing

    readSigned octal "1234" `shouldSatisfy` isJust
    readSigned octal "1834" `shouldSatisfy` isNothing

    readSigned hexadecimal "1bcd" `shouldSatisfy` isJust
    readSigned hexadecimal "1BCd" `shouldSatisfy` isJust
    readSigned hexadecimal "1bcz" `shouldSatisfy` isNothing

  it "should read at the limits" do
    readSigned decimal "9223372036854775807" `shouldEqual` Just top
    readSigned decimal "+009223372036854775807" `shouldEqual` Just top
    readSigned decimal "-09223372036854775808" `shouldEqual` Just bottom

    readUnsigned decimal "18446744073709551615" `shouldEqual` Just top

    readSigned hexadecimal "7fffffffffffffff" `shouldEqual` Just top
    readSigned hexadecimal "-8000000000000000" `shouldEqual` Just bottom

  it "should fail for overflows" do
    readSigned decimal "9223372036854775808" `shouldSatisfy` isNothing
    readSigned decimal "-9223372036854775809" `shouldSatisfy` isNothing

    readUnsigned decimal "18446744073709551616" `shouldSatisfy` isNothing

    readSigned hexadecimal "8000000000000000" `shouldSatisfy` isNothing
    readSigned hexadecimal "-8000000000000001" `shouldSatisfy` isNothing

readSigned :: Radix -> String -> Maybe Int64
readSigned = Int64.fromStringAs

readUnsigned :: Radix -> String -> Maybe UInt64
readUnsigned = UInt64.fromStringAs

i2lS :: Int -> Int64
i2lS = Int64.fromInt

signedProxy :: SignProxy Signed
signedProxy = SignProxy

unsignedProxy :: SignProxy Unsigned
unsignedProxy = SignProxy

newtype Radix' = Radix' Radix

instance arbitraryRadix' :: Arbitrary Radix' where
  arbitrary = chooseInt 2 36 >>= \i ->
    case radix i of
      Just r -> pure (Radix' r)
      Nothing -> arbitrary

newtype Int64' = Int64' Int64

instance Arbitrary Int64' where
  arbitrary = Int64' <$> chooseInt64 bottom top

newtype UInt64' = UInt64' UInt64

instance Arbitrary UInt64' where
  arbitrary = UInt64' <$> chooseUInt64 bottom top

quickCheck' :: forall a. Testable a => a -> Aff Unit
quickCheck' = liftEffect <<< quickCheck
