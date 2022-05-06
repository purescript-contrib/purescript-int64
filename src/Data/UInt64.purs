module Data.UInt64
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
  , toSigned
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
import Data.Int64.Internal (UInt64) as R
import Data.Int64.Internal (Int64, UInt64)
import Data.Int64.Internal as Internal
import Data.Maybe (Maybe)

-- | Creates a `Long` from an `Int` value
fromInt :: Int -> Maybe UInt64
fromInt = Internal.unsignedLongFromInt

-- | Creates a `UInt64` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `UInt64` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe UInt64
fromNumber = Internal.fromNumber

-- | Creates a `UInt64` from low and high bits respresented as `Int`
fromLowHighBits :: Int -> Int -> UInt64
fromLowHighBits = Internal.fromLowHighBits

-- | Reads an `Int64` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int64` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe UInt64
fromString = Internal.fromString

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe UInt64
fromStringAs = Internal.fromStringAs

-- | Get the low (least significant) bits of a `UInt64` as an `Int`
lowBits :: UInt64 -> Int
lowBits = Internal.lowBits

-- | Get the high (most significant) bits of a `UInt64` as an `Int`
highBits :: UInt64 -> Int
highBits = Internal.highBits

-- | Creates an `Int` if the `UInt64` value is within the range of `UInt64`.
toInt :: UInt64 -> Maybe Int
toInt = Internal.toInt

-- | Creates a `Number` value from a `UInt64`. Values greater than
-- | `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: UInt64 -> Number
toNumber = Internal.toNumber

-- | Like `show`, but omits the `ul` suffix.
toString :: UInt64 -> String
toString = Internal.toString

-- | Like `toStringAs`, but the integer can be specified in a different base.
toStringAs :: Radix -> UInt64 -> String
toStringAs = Internal.toStringAs

-- | Returns whether a `UInt64` is `Even` or `Odd`.
parity :: UInt64 -> Parity
parity = Internal.parity

-- | Returns whether a `UInt64` is an even number.
even :: UInt64 -> Boolean
even = Internal.even

-- | Returns whether a `UInt64` is an odd number.
odd :: UInt64 -> Boolean
odd = Internal.odd

-- | For unsigned longs, `quot` is identical to `div`.
quot :: UInt64 -> UInt64 -> UInt64
quot = Internal.quot

-- | For unsigned longs, `quot` is identical to `mod`.
rem :: UInt64 -> UInt64 -> UInt64
rem = Internal.rem

-- | Converts to an `Int64` by reading the bits as a 2's complement 64 bit signed integer.
toSigned :: UInt64 -> Int64
toSigned = Internal.unsignedToSigned

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
