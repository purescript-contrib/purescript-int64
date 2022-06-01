-- | Signed two’s-complement 64-bit integers and operations.
-- |
-- | All of the usual arithmetic operations are supplied by typeclass
-- | instances.
-- |
-- | The `Show` instance will suffix a lowercase ‘l’ for “long”.
-- | (See `toString`.)
-- |
-- | #### Usage
-- |
-- | ```purescript
-- | import Prelude
-- | import Data.Int64 as Int64
-- |
-- | let
-- |   hundred = Int64.fromInt 100
-- |   billion = Int64.fromInt 1000000000
-- | ```
-- | ---
-- | ```purescript
-- | > hundred * billion
-- | 100000000000l
-- | ```
-- | ---
-- | ```purescript
-- | > billion / hundred
-- | 10000000l
-- | ```
-- | ---
-- | ```purescript
-- | > hundred + one
-- | 101l
-- | ```
-- | ---
-- | ```purescript
-- | > hundred * zero
-- | 0l
-- | ```
-- | ---
-- | ```purescript
-- | > Int64.lowBits (hundred * billion)
-- | 1215752192
-- | ```
-- | ---
-- | ```purescript
-- | > Int64.highBits (hundred * billion)
-- | 23
-- | ```
module Data.Int64
  ( module R
  , fromLowHighBits
  , fromInt
  , fromNumber
  , fromString
  , fromStringAs
  , lowBits
  , highBits
  , toInt
  , toNumber
  , toString
  , toStringAs
  , parity
  , even
  , odd
  , quot
  , rem
  , toUnsigned
  , and
  , (.&.)
  , or
  , (.|.)
  , xor
  , (.^.)
  , shl
  , shr
  , zshr
  , complement
  ) where

import Data.Int (Parity, Radix)
import Data.Int64.Internal (Int64) as R
import Data.Int64.Internal (Int64, UInt64)
import Data.Int64.Internal as Internal
import Data.Maybe (Maybe)

-- | Creates an `Int64` from an `Int` value.
fromInt :: Int -> Int64
fromInt = Internal.signedLongFromInt

-- | Creates an `Int64` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int64` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Int64
fromNumber = Internal.fromNumber

-- | Creates an `Int64` from low and high bits represented as `Int`.
fromLowHighBits :: Int -> Int -> Int64
fromLowHighBits = Internal.fromLowHighBits

-- | Reads an `Int64` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int64` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Int64
fromString = Internal.fromString

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe Int64
fromStringAs = Internal.fromStringAs

-- | Get the low (least significant) bits of an `Int64` as an `Int`.
lowBits :: Int64 -> Int
lowBits = Internal.lowBits

-- | Get the high (most significant) bits of an `Int64` as an `Int`.
highBits :: Int64 -> Int
highBits = Internal.highBits

-- | Creates an `Int` if the `Int64` value is within the range of `Int64`.
toInt :: Int64 -> Maybe Int
toInt = Internal.toInt

-- | Like `show`, but omits the `l` suffix.
toString :: Int64 -> String
toString = Internal.toString

-- | Like `toStringAs`, but the integer can be specified in a different base.
toStringAs :: Radix -> Int64 -> String
toStringAs = Internal.toStringAs

-- | Creates a `Number` value from a `Int64`. Values not within
-- | `Number.MIN_SAFE_INTEGER` and `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: Int64 -> Number
toNumber = Internal.toNumber

-- | Returns whether a `Int64` is `Even` or `Odd`.
parity :: Int64 -> Parity
parity = Internal.parity

-- | Returns whether a `Int64` is an even number.
even :: Int64 -> Boolean
even = Internal.even

-- | Returns whether a `Int64` is an odd number.
odd :: Int64 -> Boolean
odd = Internal.odd

-- | The `quot` function provides _truncating_ long division (see the
-- | documentation for the `EuclideanRing` class). It is identical to `div` in
-- | the `EuclideanRing Int` instance if the dividend is positive, but will be
-- | slightly different if the dividend is negative.
quot :: Int64 -> Int64 -> Int64
quot = Internal.quot

-- | The `rem` function provides the remainder after _truncating_ long
-- | division (see the documentation for the `EuclideanRing` class). It is
-- | identical to `mod` in the `EuclideanRing Int` instance if the dividend is
-- | positive, but will be slightly different if the dividend is negative.
rem :: Int64 -> Int64 -> Int64
rem = Internal.rem

-- | Converts to a `UInt64` by reading the bits as a 64 bit unsigned integer.
toUnsigned :: Int64 -> UInt64
toUnsigned = Internal.signedToUnsigned

-- | Bitwise AND.
and :: Int64 -> Int64 -> Int64
and = Internal.and

infixl 10 and as .&.

-- | Bitwise OR.
or :: Int64 -> Int64 -> Int64
or = Internal.or

infixl 10 or as .|.

-- | Bitwise XOR.
xor :: Int64 -> Int64 -> Int64
xor = Internal.xor

infixl 10 xor as .^.

-- | Bitwise shift left.
shl :: Int64 -> Int64 -> Int64
shl = Internal.shl

-- | Bitwise shift right.
shr :: Int64 -> Int64 -> Int64
shr = Internal.shr

-- | Bitwise zero-fill shift right.
zshr :: Int64 -> Int64 -> Int64
zshr = Internal.zshr

-- | Bitwise NOT.
complement :: Int64 -> Int64
complement = Internal.complement
