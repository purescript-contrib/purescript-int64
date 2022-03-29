module Data.Long.Unsigned
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
       ) where

import Data.Int (Parity, Radix)
import Data.Long.Internal (ULong) as R
import Data.Long.Internal (ULong, Long)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe)

-- | Creates a `Long` from an `Int` value
fromInt :: Int -> Maybe ULong
fromInt = Internal.unsignedLongFromInt

-- | Creates an `Long` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Long` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe ULong
fromNumber = Internal.fromNumber

-- | Creates a signed `Long` from low and high bits respresented as `Int`
fromLowHighBits :: Int -> Int -> ULong
fromLowHighBits = Internal.fromLowHighBits

-- | Reads an `Long` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Long` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe ULong
fromString = Internal.fromString

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe ULong
fromStringAs = Internal.fromStringAs

-- | Get low bits of a `Long` as an `Int`
lowBits :: ULong -> Int
lowBits = Internal.lowBits

-- | Get high bits of a `Long` as an `Int`
highBits :: ULong -> Int
highBits = Internal.highBits

-- | Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: ULong -> Maybe Int
toInt = Internal.toInt

-- | Creates a `Number` value from a `Long`. Values greater than
-- | `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: ULong -> Number
toNumber = Internal.toNumber

-- | Like `show`, but omits the `ul` suffix.
toString :: ULong -> String
toString = Internal.toString

-- | Like `toStringAs`, but the integer can be specified in a different base.
toStringAs :: Radix -> ULong -> String
toStringAs = Internal.toStringAs

-- | Returns whether a `Long` is `Even` or `Odd`.
parity :: ULong -> Parity
parity = Internal.parity

-- | Returns whether a `Long` is an even number.
even :: ULong -> Boolean
even = Internal.even

-- | Returns whether a `Long` is an odd number.
odd :: ULong -> Boolean
odd = Internal.odd

-- | For unsigned longs, `quot` is identical to `div`.
quot :: ULong -> ULong -> ULong
quot = Internal.quot

-- | For unsigned longs, `quot` is identical to `mod`.
rem :: ULong -> ULong -> ULong
rem = Internal.rem

-- | Converts to a signed long by reading the bits as a 2's complement 64 bit signed integer.
toSigned :: ULong -> Long
toSigned = Internal.unsignedToSigned
