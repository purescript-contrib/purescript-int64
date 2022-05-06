module Data.Long.TestUtils
       ( i2lS
       , i2lU
       , unsafeS2lS
       , unsafeS2lU
       , isSignedV
       , isUnsignedV
       , isBigEndianV
       , isLittleEndianV
       ) where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Int (decimal)
import Data.Int64.Internal.FFI as FFI
import Effect.Uncurried (runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)

i2lS :: Int -> FFI.Long
i2lS i = runFn2 FFI.fromInt_ i isSignedV

i2lU :: Int -> FFI.Long
i2lU i = runFn2 FFI.fromInt_ i isUnsignedV

unsafeS2lS :: String -> FFI.Long
unsafeS2lS s = unsafePerformEffect $ runEffectFn3 FFI.fromString_ s isSignedV decimal

unsafeS2lU :: String -> FFI.Long
unsafeS2lU s = unsafePerformEffect $ runEffectFn3 FFI.fromString_ s isUnsignedV decimal

-- Constants

isSignedV :: FFI.IsUnsigned
isSignedV = FFI.IsUnsigned false

isUnsignedV :: FFI.IsUnsigned
isUnsignedV = FFI.IsUnsigned true

isBigEndianV :: FFI.IsLittleEndian
isBigEndianV = FFI.IsLittleEndian false

isLittleEndianV :: FFI.IsLittleEndian
isLittleEndianV = FFI.IsLittleEndian true
