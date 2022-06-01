-- | Signed two’s-complement 64-bit integers and operations.
-- |
-- | All of the usual arithmetic operations are supplied by typeclass
-- | instances.
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
  ( Int64
  , fromInt
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
  , toUnsigned
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

import Prelude

import Data.Int (Parity, Radix, decimal)
import Data.Int64.Internal as Internal
import Data.Maybe (Maybe)
import Data.UInt64 (UInt64)
import Test.QuickCheck (class Arbitrary)
import Unsafe.Coerce (unsafeCoerce)

-- | Signed two’s-complement 64-bit integer.
newtype Int64 = Int64 (Internal.Long' Internal.Signed)

-- | The `Show` instance will suffix a lowercase ‘l’ for “long”.
-- | (See `toString`.)
instance Show Int64 where
  show (Int64 (Internal.Long' l)) = Internal.toString_ l decimal <> "l"

derive newtype instance Eq Int64
derive newtype instance Ord Int64
derive newtype instance Bounded Int64
derive newtype instance Semiring Int64
derive newtype instance Ring Int64
derive newtype instance CommutativeRing Int64
derive newtype instance EuclideanRing Int64
derive newtype instance Arbitrary Int64

-- | Creates an `Int64` from an `Int` value.
fromInt :: Int -> Int64
fromInt a = Int64 (Internal.signedLongFromInt a)

-- | Creates an `Int64` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int64` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Int64
fromNumber n = Int64 <$> Internal.fromNumber n

-- | Creates an `Int64` from low and high bits represented as `Int`.
fromLowHighBits :: Int -> Int -> Int64
fromLowHighBits a b = Int64 (Internal.fromLowHighBits a b)

-- | Reads an `Int64` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int64` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Int64
fromString s = Int64 <$> Internal.fromString s

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe Int64
fromStringAs r s = Int64 <$> Internal.fromStringAs r s

-- | Get the low (least significant) bits of an `Int64` as an `Int`.
lowBits :: Int64 -> Int
lowBits (Int64 a) = Internal.lowBits a

-- | Get the high (most significant) bits of an `Int64` as an `Int`.
highBits :: Int64 -> Int
highBits (Int64 a) = Internal.highBits a

-- | Creates an `Int` if the `Int64` value is within the range of `Int64`.
toInt :: Int64 -> Maybe Int
toInt (Int64 a) = Internal.toInt a

-- | Like `show`, but omits the `l` suffix.
toString :: Int64 -> String
toString (Int64 a) = Internal.toString a

-- | Like `toString`, but the integer can be specified in a different base.
toStringAs :: Radix -> Int64 -> String
toStringAs r (Int64 a) = Internal.toStringAs r a

-- | Creates a `Number` value from a `Int64`. Values not within
-- | `Number.MIN_SAFE_INTEGER` and `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: Int64 -> Number
toNumber (Int64 a) = Internal.toNumber a

-- | Returns whether a `Int64` is `Even` or `Odd`.
parity :: Int64 -> Parity
parity (Int64 a) = Internal.parity a

-- | Returns true if an even number.
even :: Int64 -> Boolean
even (Int64 a) = Internal.even a

-- | Returns true if an odd number.
odd :: Int64 -> Boolean
odd (Int64 a) = Internal.odd a

-- | The `quot` function provides _truncating_ long division (see the
-- | documentation for the `EuclideanRing` class). It is identical to `div` in
-- | the `EuclideanRing Int` instance if the dividend is positive, but will be
-- | slightly different if the dividend is negative.
quot :: Int64 -> Int64 -> Int64
quot (Int64 a) (Int64 b) = Int64 (Internal.quot a b)

-- | The `rem` function provides the remainder after _truncating_ long
-- | division (see the documentation for the `EuclideanRing` class). It is
-- | identical to `mod` in the `EuclideanRing Int` instance if the dividend is
-- | positive, but will be slightly different if the dividend is negative.
rem :: Int64 -> Int64 -> Int64
rem (Int64 a) (Int64 b) = Int64 (Internal.rem a b)

-- | Converts to a `UInt64` by casting the bits as a 64-bit unsigned integer.
toUnsigned :: Int64 -> UInt64
toUnsigned (Int64 a) = unsafeCoerce (Internal.signedToUnsigned a)

-- | Converts to an `Int64` by casting the bits as a 2’s-complement 64-bit signed integer.
toSigned :: UInt64 -> Int64
toSigned a = Int64 (Internal.unsignedToSigned (unsafeCoerce a))

-- | Bitwise AND.
and :: Int64 -> Int64 -> Int64
and (Int64 a) (Int64 b) = Int64 (Internal.and a b)

infixl 10 and as .&.

-- | Bitwise OR.
or :: Int64 -> Int64 -> Int64
or (Int64 a) (Int64 b) = Int64 (Internal.or a b)

infixl 10 or as .|.

-- | Bitwise XOR.
xor :: Int64 -> Int64 -> Int64
xor (Int64 a) (Int64 b) = Int64 (Internal.xor a b)

infixl 10 xor as .^.

-- | Bitwise shift left.
shl :: Int64 -> Int64 -> Int64
shl (Int64 a) (Int64 b) = Int64 (Internal.shl a b)

-- | Bitwise shift right.
shr :: Int64 -> Int64 -> Int64
shr (Int64 a) (Int64 b) = Int64 (Internal.shr a b)

-- | Bitwise zero-fill shift right.
zshr :: Int64 -> Int64 -> Int64
zshr (Int64 a) (Int64 b) = Int64 (Internal.zshr a b)

-- | Bitwise NOT.
complement :: Int64 -> Int64
complement (Int64 a) = Int64 (Internal.complement a)
