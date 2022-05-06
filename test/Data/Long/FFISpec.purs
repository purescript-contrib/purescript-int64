module Data.Long.FFISpec
       ( ffiSpec
       ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Int (decimal)
import Data.Int64.Internal.FFI (Long)
import Data.Int64.Internal.FFI as FFI
import Data.Int64.Internal as Internal
import Data.Long.TestUtils (i2lS, i2lU, isBigEndianV, isLittleEndianV, isSignedV, isUnsignedV, unsafeS2lS, unsafeS2lU)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Foreign (unsafeToForeign)
import Test.Assert (assert)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual, shouldSatisfy)

ffiSpec :: Spec Unit
ffiSpec = describe "FFI" do
  it "should have correct values for constants" do
    FFI.zero `shouldEqual` (i2lS 0)
    FFI.one `shouldEqual` (i2lS 1)
    FFI.negOne `shouldEqual` (i2lS (-1))
    FFI.uzero `shouldEqual` (i2lU 0)
    FFI.uone `shouldEqual` (i2lU 1)
    FFI.maxValue `shouldEqual` (unsafeS2lS "9223372036854775807")
    FFI.minValue `shouldEqual` (unsafeS2lS "-9223372036854775808")
    FFI.maxUnsignedValue `shouldEqual` (unsafeS2lU "18446744073709551615")

  it "should create longs" do
    liftEffect $ assert $ FFI.isLong (unsafeToForeign FFI.zero)
    (runFn3 FFI.fromBits sampleS.low sampleS.high isSignedV) `shouldEqual` sampleS.value
    (runFn3 FFI.fromBits sampleU.low sampleU.high isUnsignedV) `shouldEqual` sampleU.value
    (runFn3 FFI.fromBytes sampleS.beBytes isSignedV isBigEndianV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromBytesLE sampleS.leBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromBytesBE sampleS.beBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromInt 2 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (runFn2 FFI.fromNumber 2.0 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (runFn2 FFI.fromNumber 2.0 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (liftEffect $ runEffectFn3 FFI.fromString "2" isSignedV decimal)
      >>= (_ `shouldEqual` (runFn2 FFI.fromInt 2 isSignedV))
    expectError $ liftEffect (runEffectFn3 FFI.fromString "2-2" isSignedV decimal)

  it "should access fields" do
    FFI.unsigned (i2lU 2) `shouldEqual` isUnsignedV

  it "should access methods" do
    (FFI.add (i2lS 2) (i2lS 3)) `shouldEqual` (i2lS 5)
    (FFI.and (i2lS 2) (i2lS 1)) `shouldEqual` (i2lS 0)

    (FFI.compare (i2lS 1) (i2lS 2)) `shouldEqual` (-1)
    (FFI.compare (i2lS 2) (i2lS 2)) `shouldEqual` 0
    (FFI.compare (i2lS 2) (i2lS 1)) `shouldEqual` 1

    (FFI.divide (i2lS 8) (i2lS 3)) `shouldEqual` (i2lS 2)
    (Internal.numberBitsToInt $ FFI.getHighBits sampleS.value) `shouldEqual` sampleS.high
    (Internal.numberBitsToInt $ FFI.getHighBitsUnsigned sampleU.value) `shouldEqual` sampleU.high
    (Internal.numberBitsToInt $ FFI.getLowBits sampleS.value) `shouldEqual` sampleS.low
    (Internal.numberBitsToInt $ FFI.getLowBitsUnsigned sampleU.value) `shouldEqual` sampleU.low
    (i2lS 5) `shouldSatisfy` (_ `FFI.greaterThan` (i2lS 2))
    (i2lS 5) `shouldSatisfy` (_ `FFI.greaterThanOrEqual` (i2lS 5))
    (i2lS 6) `shouldSatisfy` FFI.isEven
    (i2lS (-6)) `shouldSatisfy` FFI.isNegative
    (i2lS 5) `shouldSatisfy` FFI.isOdd
    (i2lS 5) `shouldSatisfy` FFI.isPositive
    FFI.zero `shouldSatisfy` FFI.isZero
    (i2lS 2) `shouldSatisfy` (_ `FFI.lessThan` (i2lS 5))
    (i2lS 5) `shouldSatisfy` (_ `FFI.lessThanOrEqual` (i2lS 5))

    -- modulo, note the sign of the answers
    (FFI.modulo (i2lS 5) (i2lS 3)) `shouldEqual` (i2lS 2)
    (FFI.modulo (i2lS (-5)) (i2lS 3)) `shouldEqual` (i2lS (-2))
    (FFI.modulo (i2lS 5) (i2lS (-3))) `shouldEqual` (i2lS 2)
    (FFI.modulo (i2lS (-5)) (i2lS (-3))) `shouldEqual` (i2lS (-2))

    (FFI.multiply (i2lS 5) (i2lS 3)) `shouldEqual` (i2lS 15)
    (FFI.negate (i2lS 5)) `shouldEqual` (i2lS (-5))
    (FFI.not (i2lS (-12345))) `shouldEqual` (i2lS 12344)
    (i2lS 12344) `shouldSatisfy` (FFI.notEquals (i2lS (-12345)) )
    (FFI.or (i2lS 11) (i2lS 5)) `shouldEqual` (i2lS 15)
    (FFI.shiftLeft (i2lS 11) (i2lS 2)) `shouldEqual` (i2lS 44)
    (FFI.shiftRight (i2lS (-11)) (i2lS 2)) `shouldEqual` (i2lS (-3))
    (FFI.shiftRightUnsigned (unsafeS2lU "18446744073709551605") (i2lU 2)) `shouldEqual` (unsafeS2lU "4611686018427387901")
    -- TODO rotateLeft, i2lS 2 (FFI.subtract (i2lS 2) (i2lS 3)) `shouldEqual` (i2lS -1)
    (FFI.toBytes sampleS.value isLittleEndianV) `shouldEqual` sampleS.leBytes
    (FFI.toBytes sampleS.value isBigEndianV) `shouldEqual` sampleS.beBytes
    (FFI.toInt (i2lS 2)) `shouldEqual` 2
    -- out of range gets clipped
    (FFI.toInt (unsafeS2lS "100000000000")) `shouldEqual` (Internal.numberBitsToInt 100000000000.0)

    -- can lose precision when converting to number
    (FFI.toNumber (unsafeS2lS "9007199254740993")) `shouldEqual` 9007199254740992.0

    (FFI.toSigned (unsafeS2lU "18446744073709551605")) `shouldEqual` (i2lS (-11))
    (FFI.toUnsigned (i2lS (-11))) `shouldEqual` (unsafeS2lU "18446744073709551605")
    (FFI.xor (i2lS 11) (i2lS 5)) `shouldEqual` (i2lS 14)

-- Sample

sampleS ::
  { value :: Long
  , high :: Int
  , low :: Int
  , beBytes :: Array Int
  , leBytes :: Array Int
  }
sampleS =
  { value: unsafeS2lS "-107374182489"
  , high: -26
  , low: -89
  , beBytes
  , leBytes: Array.reverse beBytes
  }
  where
    beBytes = [255, 255, 255, 230, 255, 255, 255, 167]

sampleU ::
  { value :: Long
  , high :: Int
  , low :: Int
  }
sampleU =
  { value: unsafeS2lU "18446743983515238366"
  , high: -22
  , low: -34
  }
