module Data.Int64.Internal.FFI
       ( Long
       , IsUnsigned(..)
       , IsLittleEndian(..)
       -- Constants
       , zero
       , one
       , negOne
       , uzero
       , uone
       , maxValue
       , minValue
       , maxUnsignedValue
       -- Utilities
       , isLong_
       , fromBits_
       , fromBytes_
       , fromBytesLE_
       , fromBytesBE_
       , fromInt_
       , fromNumber_
       , fromString_
       , fromValue_
       -- Fields
       , unsigned
       -- Methods
       , add
       , and
       , compare
       , divide
       , equals
       , getHighBits
       , getHighBitsUnsigned
       , getLowBits
       , getLowBitsUnsigned
       , getNumBitsAbs
       , greaterThan
       , greaterThanOrEqual
       , isEven
       , isNegative
       , isOdd
       , isPositive
       , isZero
       , lessThan
       , lessThanOrEqual
       , modulo
       , multiply
       , negate
       , not
       , notEquals
       , or
       , shiftLeft
       , shiftRight
       , shiftRightUnsigned
       , rotateLeft
       , rotateRight
       , subtract
       , toBytes
       , toInt
       , toNumber
       , toSigned
       , toString
       , toUnsigned
       , xor
       ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn3)
import Data.Int (Radix, decimal)
import Effect.Uncurried (EffectFn3)
import Foreign (Foreign)

-- A 64 bit two's-complement integer
foreign import data Long :: Type

instance showLong :: Show Long where
  show l = toString l decimal <> suffix
    where
      suffix = case unsigned l of
        (IsUnsigned false) -> "l"
        (IsUnsigned true) -> "ul"

instance eqLong :: Eq Long where
  eq a b = equals a b && (unsigned a == unsigned b)

newtype IsUnsigned = IsUnsigned Boolean
derive newtype instance eqIsUnsigned :: Eq IsUnsigned
instance showIsUnsigned :: Show IsUnsigned where
  show (IsUnsigned v) = "(IsUnsigned " <> (show v) <> ")"

newtype IsLittleEndian = IsLittleEndian Boolean
derive newtype instance eqIsLittleEndian :: Eq IsLittleEndian
instance showIsLittleEndian :: Show IsLittleEndian where
  show (IsLittleEndian v) = "(IsLittleEndian " <> (show v) <> ")"


--
-- Constants
--

--| Signed zero
foreign import zero :: Long

--| Signed one
foreign import one :: Long

--| Signed negative one
foreign import negOne :: Long

--| Unsigned zero
foreign import uzero :: Long

--| Unsigned one
foreign import uone :: Long

--| Maximum signed value
foreign import maxValue :: Long

--| Minimum signed value
foreign import minValue :: Long

--| Maximum unsigned value
foreign import maxUnsignedValue :: Long

--
-- Utilities
--

--| Tests if the specified object is a Long
foreign import isLong_ :: Fn1 Foreign Boolean

--| Returns a Long representing the 64 bit integer that comes by concatenating the given low and high bits. Each is assumed to use 32 bits.
foreign import fromBits_ :: Fn3 Int Int IsUnsigned Long

--| Creates a Long from its byte representation.
foreign import fromBytes_ :: Fn3 (Array Int) IsUnsigned IsLittleEndian Long

--| Creates a Long from its little endian byte representation.
foreign import fromBytesLE_ :: Fn2 (Array Int) IsUnsigned Long

--| Creates a Long from its big endian byte representation.
foreign import fromBytesBE_ :: Fn2 (Array Int) IsUnsigned Long

--| Returns a Long representing the given 32 bit integer value.
foreign import fromInt_ :: Fn2 Int IsUnsigned Long

--| Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
foreign import fromNumber_ :: Fn2 Number IsUnsigned Long

--| Returns a Long representation of the given string, written using the specified radix.
foreign import fromString_ :: EffectFn3 String IsUnsigned Radix Long

--| Converts the specified value to a Long using the appropriate from* function for its type.
foreign import fromValue_ :: Fn2 Foreign IsUnsigned Long

--
-- Fields
--

foreign import unsigned :: Long -> IsUnsigned

--
-- Methods
--

--| Returns the sum of this and the specified Long.
foreign import add :: Long -> Fn1 Long Long

--| Returns the bitwise AND of this Long and the specified.
foreign import and :: Long -> Fn1 Long Long

--| Compares this Long's value with the specified's. Returns 0 if they are the same, 1 if the this is greater and -1 if the given one is greater.
foreign import compare :: Long -> Fn1 Long Int

--| Returns this Long divided by the specified.
foreign import divide :: Long -> Fn1 Long Long

--| Tests if this Long's value equals the specified's.
--| Note: The original API accepts any number like input.
--| If provided with another Long, this does not compare signedness.
foreign import equals :: Long -> Fn1 Long Boolean

--| Gets the high 32 bits as a signed integer.
--| Note: Use Internal.getNumberBitsToInt to get back Int
foreign import getHighBits :: Long -> Number

--| Gets the high 32 bits as an unsigned integer.
--| Note: Use Internal.getNumberBitsToInt to get back Int
foreign import getHighBitsUnsigned :: Long -> Number

--| Gets the low 32 bits as a signed integer.
--| Use Internal.getNumberBitsToInt to get back Int
foreign import getLowBits :: Long -> Number

--| Gets the low 32 bits as an unsigned integer.
--| Use Internal.getNumberBitsToInt to get back Int
foreign import getLowBitsUnsigned :: Long -> Number

--| Gets the number of bits needed to represent the absolute value of this Long.
foreign import getNumBitsAbs :: Long -> Int

--| Tests if this Long's value is greater than the specified's.
foreign import greaterThan :: Long -> Fn1 Long Boolean

--| Tests if this Long's value is greater than or equal the specified's.
foreign import greaterThanOrEqual :: Long -> Fn1 Long Boolean

--| Tests if this Long's value is even.
foreign import isEven :: Long -> Boolean

--| Tests if this Long's value is negative.
foreign import isNegative :: Long -> Boolean

--| Tests if this Long's value is odd.
foreign import isOdd :: Long -> Boolean

--| Tests if this Long's value is positive.
foreign import isPositive :: Long -> Boolean

--| Tests if this Long's value equals zero.
foreign import isZero :: Long -> Boolean

--| Tests if this Long's value is less than the specified's.
foreign import lessThan :: Long -> Fn1 Long Boolean

--| Tests if this Long's value is less than or equal the specified's.
foreign import lessThanOrEqual :: Long -> Fn1 Long Boolean

--| Returns this Long modulo the specified.
foreign import modulo :: Long -> Fn1 Long Long

--| Returns the product of this and the specified Long.
foreign import multiply :: Long -> Fn1 Long Long

--| Negates this Long's value.
foreign import negate :: Long -> Long

--| Returns the bitwise NOT of this Long.
foreign import not :: Long -> Long

--| Tests if this Long's value differs from the specified's.
foreign import notEquals :: Long -> Fn1 Long Boolean

--| Returns the bitwise OR of this Long and the specified.
foreign import or :: Long -> Fn1 Long Long

--| Returns this Long with bits shifted to the left by the given amount.
foreign import shiftLeft :: Long -> Fn1 Long Long

--| Returns this Long with bits arithmetically shifted to the right by the given amount.
foreign import shiftRight :: Long -> Fn1 Long Long

--| Returns this Long with bits logically shifted to the right by the given amount.
foreign import shiftRightUnsigned :: Long -> Fn1 Long Long

--| Returns this Long with bits rotated to the left by the given amount.
foreign import rotateLeft :: Long -> Fn1 Long Long

--| Returns this Long with bits rotated to the right by the given amount.
foreign import rotateRight :: Long -> Fn1 Long Long

--| Returns the difference of this and the specified Long.
foreign import subtract :: Long -> Fn1 Long Long

--| Converts this Long to its byte representation.
foreign import toBytes :: Long -> IsLittleEndian -> Array Int

--| Converts the Long to a 32 bit integer, assuming it is a 32 bit integer.
foreign import toInt :: Long -> Int

--| Converts the Long to a the nearest floating-point representation of this value (double, 53 bit mantissa).
foreign import toNumber :: Long -> Number

--| Converts this Long to signed.
foreign import toSigned :: Long -> Long

--| Converts the Long to a string written in the specified radix.
foreign import toString :: Long -> Fn1 Radix String

--| Converts this Long to unsigned.
foreign import toUnsigned :: Long -> Long

--| Returns the bitwise XOR of this Long and the given one.
foreign import xor :: Long -> Fn1 Long Long
