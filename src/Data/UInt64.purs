-- | Unsigned 64-bit integers and operations.
-- |
-- | All of the usual arithmetic operations are supplied by typeclass
-- | instances.
-- |
-- | #### Usage
-- |
-- | ```purescript
-- | import Prelude
-- | import Data.UInt64 as UInt64
-- |
-- | let
-- |   hundred = UInt64.unsafeFromInt 100
-- |   billion = UInt64.unsafeFromInt 1000000000
-- | ```
-- | ---
-- | ```purescript
-- | > hundred * billion
-- | 100000000000ul
-- | ```
-- | ---
-- | ```purescript
-- | > billion / hundred
-- | 10000000ul
-- | ```
-- | ---
-- | ```purescript
-- | > hundred + one
-- | 101ul
-- | ```
-- | ---
-- | ```purescript
-- | > hundred * zero
-- | 0ul
-- | ```
module Data.UInt64
  ( UInt64
  , fromInt
  , unsafeFromInt
  , fromLowHighBits
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

import Prelude

import Data.Int (Parity, Radix, decimal)
import Data.Int64.Internal as Internal
import Data.Maybe (Maybe)
import Test.QuickCheck (class Arbitrary)

-- | Unsigned 64-bit integer.
newtype UInt64 = UInt64 (Internal.Long' Internal.Unsigned)

-- | The `Show` instance will suffix a lowercase ‘ul’ for “unsigned long”.
-- | (See `toString`.)
instance Show UInt64 where
  show (UInt64 (Internal.Long' l)) = Internal.toString_ l decimal <> "ul"

derive newtype instance Eq UInt64
derive newtype instance Ord UInt64
derive newtype instance Bounded UInt64
derive newtype instance Semiring UInt64
derive newtype instance Ring UInt64
derive newtype instance CommutativeRing UInt64
derive newtype instance EuclideanRing UInt64
derive newtype instance Arbitrary UInt64

-- | Creates a `UInt64` from an `Int` value.
fromInt :: Int -> Maybe UInt64
fromInt a = UInt64 <$> Internal.unsignedLongFromInt a

-- | Creates a `UInt64` from an `Int` value.
-- |
-- | If the `Int` is negative, the result is undefined.
unsafeFromInt :: Int -> UInt64
unsafeFromInt a = UInt64 (Internal.unsafeFromInt a)

-- | Creates a `UInt64` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `UInt64` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe UInt64
fromNumber n = UInt64 <$> Internal.fromNumber n

-- | Creates a `UInt64` from low and high bits represented as `Int`.
fromLowHighBits :: Int -> Int -> UInt64
fromLowHighBits a b = UInt64 (Internal.fromLowHighBits a b)

-- | Reads a `UInt64` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `UInt64` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe UInt64
fromString s = UInt64 <$> Internal.fromString s

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe UInt64
fromStringAs r s = UInt64 <$> Internal.fromStringAs r s

-- | Get the low (least significant) 32 bits of a `UInt64` as an `Int`.
lowBits :: UInt64 -> Int
lowBits (UInt64 a) = Internal.lowBits a

-- | Get the high (most significant) 32 bits of a `UInt64` as an `Int`.
highBits :: UInt64 -> Int
highBits (UInt64 a) = Internal.highBits a

-- | Creates an `Int` if the `UInt64` value is positive.
toInt :: UInt64 -> Maybe Int
toInt (UInt64 a) = Internal.toInt a

-- | Creates a `Number` value from a `UInt64`. Values greater than
-- | `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: UInt64 -> Number
toNumber (UInt64 a) = Internal.toNumber a

-- | Like `show`, but omits the `ul` suffix.
toString :: UInt64 -> String
toString (UInt64 a) = Internal.toString a

-- | Like `toString`, but the integer can be specified in a different base.
toStringAs :: Radix -> UInt64 -> String
toStringAs r (UInt64 a) = Internal.toStringAs r a

-- | Returns whether a `UInt64` is `Even` or `Odd`.
parity :: UInt64 -> Parity
parity (UInt64 a) = Internal.parity a

-- | Returns true if an even number.
even :: UInt64 -> Boolean
even (UInt64 a) = Internal.even a

-- | Returns true if an odd number.
odd :: UInt64 -> Boolean
odd (UInt64 a) = Internal.odd a

-- | `quot` is identical to `div`.
quot :: UInt64 -> UInt64 -> UInt64
quot (UInt64 a) (UInt64 b) = UInt64 (Internal.quot a b)

-- | `rem` is identical to `mod`.
rem :: UInt64 -> UInt64 -> UInt64
rem (UInt64 a) (UInt64 b) = UInt64 (Internal.rem a b)

-- | Bitwise AND.
and :: UInt64 -> UInt64 -> UInt64
and (UInt64 a) (UInt64 b) = UInt64 (Internal.and a b)

infixl 10 and as .&.

-- | Bitwise OR.
or :: UInt64 -> UInt64 -> UInt64
or (UInt64 a) (UInt64 b) = UInt64 (Internal.or a b)

infixl 10 or as .|.

-- | Bitwise XOR.
xor :: UInt64 -> UInt64 -> UInt64
xor (UInt64 a) (UInt64 b) = UInt64 (Internal.xor a b)

infixl 10 xor as .^.

-- | Bitwise shift left.
shl :: UInt64 -> UInt64 -> UInt64
shl (UInt64 a) (UInt64 b) = UInt64 (Internal.shl a b)

-- | Bitwise shift right.
shr :: UInt64 -> UInt64 -> UInt64
shr (UInt64 a) (UInt64 b) = UInt64 (Internal.shr a b)

-- | Bitwise zero-fill shift right.
zshr :: UInt64 -> UInt64 -> UInt64
zshr (UInt64 a) (UInt64 b) = UInt64 (Internal.zshr a b)

-- | Bitwise NOT.
complement :: UInt64 -> UInt64
complement (UInt64 a) = UInt64 (Internal.complement a)
