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
import Data.Int64.Internal as Internal
import Effect.Uncurried (runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)

i2lS :: Int -> Internal.Long
i2lS i = runFn2 Internal.fromInt_ i isSignedV

i2lU :: Int -> Internal.Long
i2lU i = runFn2 Internal.fromInt_ i isUnsignedV

unsafeS2lS :: String -> Internal.Long
unsafeS2lS s = unsafePerformEffect $ runEffectFn3 Internal.fromString_ s isSignedV decimal

unsafeS2lU :: String -> Internal.Long
unsafeS2lU s = unsafePerformEffect $ runEffectFn3 Internal.fromString_ s isUnsignedV decimal

-- Constants

isSignedV :: Internal.IsUnsigned
isSignedV = Internal.IsUnsigned false

isUnsignedV :: Internal.IsUnsigned
isUnsignedV = Internal.IsUnsigned true

isBigEndianV :: Internal.IsLittleEndian
isBigEndianV = Internal.IsLittleEndian false

isLittleEndianV :: Internal.IsLittleEndian
isLittleEndianV = Internal.IsLittleEndian true
