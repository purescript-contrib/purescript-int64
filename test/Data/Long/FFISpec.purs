module Data.Long.FFISpec
       ( ffiSpec
       ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Int (decimal)
import Data.Int64.Internal (Long)
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
    Internal.zero_ `shouldEqual` (i2lS 0)
    Internal.one_ `shouldEqual` (i2lS 1)
    Internal.negOne_ `shouldEqual` (i2lS (-1))
    Internal.uzero_ `shouldEqual` (i2lU 0)
    Internal.uone_ `shouldEqual` (i2lU 1)
    Internal.maxValue_ `shouldEqual` (unsafeS2lS "9223372036854775807")
    Internal.minValue_ `shouldEqual` (unsafeS2lS "-9223372036854775808")
    Internal.maxUnsignedValue_ `shouldEqual` (unsafeS2lU "18446744073709551615")

  it "should create longs" do
    liftEffect $ assert $ Internal.isLong_ (unsafeToForeign Internal.zero_)
    (runFn3 Internal.fromBits_ sampleS.low sampleS.high isSignedV) `shouldEqual` sampleS.value
    (runFn3 Internal.fromBits_ sampleU.low sampleU.high isUnsignedV) `shouldEqual` sampleU.value
    (runFn3 Internal.fromBytes_ sampleS.beBytes isSignedV isBigEndianV) `shouldEqual` sampleS.value
    (runFn2 Internal.fromBytesLE_ sampleS.leBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 Internal.fromBytesBE_ sampleS.beBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 Internal.fromInt_ 2 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (runFn2 Internal.fromNumber_ 2.0 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (runFn2 Internal.fromNumber_ 2.0 isSignedV) `shouldEqual` (unsafeS2lS "2")
    (liftEffect $ runEffectFn3 Internal.fromString_ "2" isSignedV decimal)
      >>= (_ `shouldEqual` (runFn2 Internal.fromInt_ 2 isSignedV))
    expectError $ liftEffect (runEffectFn3 Internal.fromString_ "2-2" isSignedV decimal)

  it "should access fields" do
    Internal.unsigned_ (i2lU 2) `shouldEqual` isUnsignedV

  it "should access methods" do
    (Internal.add_ (i2lS 2) (i2lS 3)) `shouldEqual` (i2lS 5)
    (Internal.and_ (i2lS 2) (i2lS 1)) `shouldEqual` (i2lS 0)

    (Internal.compare_ (i2lS 1) (i2lS 2)) `shouldEqual` (-1)
    (Internal.compare_ (i2lS 2) (i2lS 2)) `shouldEqual` 0
    (Internal.compare_ (i2lS 2) (i2lS 1)) `shouldEqual` 1

    (Internal.divide_ (i2lS 8) (i2lS 3)) `shouldEqual` (i2lS 2)
    (Internal.numberBitsToInt $ Internal.getHighBits_ sampleS.value) `shouldEqual` sampleS.high
    (Internal.numberBitsToInt $ Internal.getHighBitsUnsigned_ sampleU.value) `shouldEqual` sampleU.high
    (Internal.numberBitsToInt $ Internal.getLowBits_ sampleS.value) `shouldEqual` sampleS.low
    (Internal.numberBitsToInt $ Internal.getLowBitsUnsigned_ sampleU.value) `shouldEqual` sampleU.low
    (i2lS 5) `shouldSatisfy` (_ `Internal.greaterThan_` (i2lS 2))
    (i2lS 5) `shouldSatisfy` (_ `Internal.greaterThanOrEqual_` (i2lS 5))
    (i2lS 6) `shouldSatisfy` Internal.isEven_
    (i2lS (-6)) `shouldSatisfy` Internal.isNegative_
    (i2lS 5) `shouldSatisfy` Internal.isOdd_
    (i2lS 5) `shouldSatisfy` Internal.isPositive_
    Internal.zero_ `shouldSatisfy` Internal.isZero_
    (i2lS 2) `shouldSatisfy` (_ `Internal.lessThan_` (i2lS 5))
    (i2lS 5) `shouldSatisfy` (_ `Internal.lessThanOrEqual_` (i2lS 5))

    -- modulo, note the sign of the answers
    (Internal.modulo_ (i2lS 5) (i2lS 3)) `shouldEqual` (i2lS 2)
    (Internal.modulo_ (i2lS (-5)) (i2lS 3)) `shouldEqual` (i2lS (-2))
    (Internal.modulo_ (i2lS 5) (i2lS (-3))) `shouldEqual` (i2lS 2)
    (Internal.modulo_ (i2lS (-5)) (i2lS (-3))) `shouldEqual` (i2lS (-2))

    (Internal.multiply_ (i2lS 5) (i2lS 3)) `shouldEqual` (i2lS 15)
    (Internal.negate_ (i2lS 5)) `shouldEqual` (i2lS (-5))
    (Internal.not_ (i2lS (-12345))) `shouldEqual` (i2lS 12344)
    (i2lS 12344) `shouldSatisfy` (Internal.notEquals_ (i2lS (-12345)) )
    (Internal.or_ (i2lS 11) (i2lS 5)) `shouldEqual` (i2lS 15)
    (Internal.shiftLeft_ (i2lS 11) (i2lS 2)) `shouldEqual` (i2lS 44)
    (Internal.shiftRight_ (i2lS (-11)) (i2lS 2)) `shouldEqual` (i2lS (-3))
    (Internal.shiftRightUnsigned_ (unsafeS2lU "18446744073709551605") (i2lU 2)) `shouldEqual` (unsafeS2lU "4611686018427387901")
    -- TODO rotateLeft, i2lS 2 (FFI.subtract (i2lS 2) (i2lS 3)) `shouldEqual` (i2lS -1)
    (Internal.toBytes_ sampleS.value isLittleEndianV) `shouldEqual` sampleS.leBytes
    (Internal.toBytes_ sampleS.value isBigEndianV) `shouldEqual` sampleS.beBytes
    (Internal.toInt_ (i2lS 2)) `shouldEqual` 2
    -- out of range gets clipped
    (Internal.toInt_ (unsafeS2lS "100000000000")) `shouldEqual` (Internal.numberBitsToInt 100000000000.0)

    -- can lose precision when converting to number
    (Internal.toNumber_ (unsafeS2lS "9007199254740993")) `shouldEqual` 9007199254740992.0

    (Internal.toSigned_ (unsafeS2lU "18446744073709551605")) `shouldEqual` (i2lS (-11))
    (Internal.toUnsigned_ (i2lS (-11))) `shouldEqual` (unsafeS2lU "18446744073709551605")
    (Internal.xor_ (i2lS 11) (i2lS 5)) `shouldEqual` (i2lS 14)

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
