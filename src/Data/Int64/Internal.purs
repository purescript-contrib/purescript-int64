module Data.Int64.Internal
  ( IsLittleEndian(..)
  , IsUnsigned(..)
  , Long
  , Long'(..)
  , SignProxy(..)
  , Signed
  , Signedness
  , Unsigned
  , add_
  , and
  , and_
  , class SInfo
  , compare_
  , complement
  , divide_
  , equals_
  , even
  , ffiSignedness
  , fromBits_
  , fromBytesBE_
  , fromBytesLE_
  , fromBytes_
  , fromInt_
  , fromLowHighBits
  , fromNumber
  , fromNumber_
  , fromString
  , fromStringAs
  , fromString_
  , getHighBitsUnsigned_
  , getHighBits_
  , getLowBitsUnsigned_
  , getLowBits_
  , greaterThanOrEqual_
  , greaterThan_
  , highBits
  , isEven_
  , isLong_
  , isNegative_
  , isOdd_
  , isPositive_
  , isZero_
  , lessThanOrEqual_
  , lessThan_
  , lowBits
  , maxUnsignedValue_
  , maxValue_
  , minValue_
  , modulo_
  , multiply_
  , negOne_
  , negate_
  , negative
  , notEquals_
  , not_
  , numberBitsToInt
  , odd
  , one_
  , or
  , or_
  , parity
  , positive
  , quot
  , rem
  , rotateLeft_
  , rotateRight_
  , shiftLeft_
  , shiftRightUnsigned_
  , shiftRight_
  , shl
  , shr
  , signedLongFromInt
  , signedToUnsigned
  , subtract_
  , toBytes_
  , toInt
  , toInt_
  , toNumber
  , toNumber_
  , toSigned_
  , toString
  , toStringAs
  , toString_
  , toUnsigned_
  , unsafeFromInt
  , unsignedLongFromInt
  , unsignedToSigned
  , unsigned_
  , uone_
  , uzero_
  , xor
  , xor_
  , zero_
  , zshr
  ) where

import Prelude
import Effect.Uncurried (EffectFn3)
import Foreign (Foreign)

import Data.Function.Uncurried (runFn2, runFn3, Fn1, Fn2, Fn3)
import Data.Int (Parity(..), Radix, decimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Ord (abs)

foreign import data Signedness :: Type

foreign import data Signed :: Signedness
foreign import data Unsigned :: Signedness

data SignProxy (s :: Signedness) = SignProxy

class SInfo (s :: Signedness) where
  ffiSignedness :: SignProxy s -> IsUnsigned
  toInt :: Long' s -> Maybe Int

instance infoSigned :: SInfo Signed where
  ffiSignedness _ = (IsUnsigned false)
  toInt l =
    let
      low = lowBits l
      high = highBits l
    in
      if (high == 0 && low >= 0) || (high == (-1) && low < 0) then Just low
      else Nothing

instance infoUnsigned :: SInfo Unsigned where
  ffiSignedness _ = (IsUnsigned true)
  toInt l =
    let
      low = lowBits l
      high = highBits l
    in
      if high == 0 && low > 0 then Just low
      else Nothing

-- | Newtype wrapper for the foreign JavaScript `Long` object.
newtype Long' (s :: Signedness) = Long' Long

instance showLong' :: Show (Long' s) where
  show (Long' l) = show l

instance eqLong' :: Eq (Long' s) where
  eq (Long' l1) (Long' l2) = equals_ l1 l2

instance ordLong' :: Ord (Long' s) where
  compare (Long' l1) (Long' l2) = case compare_ l1 l2 of
    0 -> EQ
    x | x > 0 -> GT
    _ -> LT

instance boundedLong'Signed :: Bounded (Long' Signed) where
  top = Long' maxValue_
  bottom = Long' minValue_

instance boundedLong'Unsigned :: Bounded (Long' Unsigned) where
  top = Long' $ maxUnsignedValue_
  bottom = Long' $ uzero_

instance semiringLong'Signed :: Semiring (Long' Signed) where
  add (Long' l1) (Long' l2) = Long' $ add_ l1 l2
  zero = Long' zero_
  mul (Long' l1) (Long' l2) = Long' $ multiply_ l1 l2
  one = Long' $ one_

instance semiringLong'Unsigned :: Semiring (Long' Unsigned) where
  add (Long' l1) (Long' l2) = Long' $ add_ l1 l2
  zero = Long' $ uzero_
  mul (Long' l1) (Long' l2) = Long' $ multiply_ l1 l2
  one = Long' $ uone_

instance ringLong'Signed :: Ring (Long' Signed) where
  sub (Long' l1) (Long' l2) = Long' $ subtract_ l1 l2

instance ringLong'Unsigned :: Ring (Long' Unsigned) where
  sub (Long' l1) (Long' l2) = Long' $ subtract_ l1 l2

instance commutativeRingLong'Signed :: CommutativeRing (Long' Signed)
instance commutativeRingLong'Unsigned :: CommutativeRing (Long' Unsigned)

instance euclideanRingLong'Signed :: EuclideanRing (Long' Signed) where
  degree = Int.floor <<< toNumber <<< abs
  div l1 l2 =
    (l1 - (l1 `mod` l2)) `quot` l2
  -- https://github.com/purescript/purescript-prelude/blob/f4cad0ae8106185c9ab407f43cf9abf05c256af4/src/Data/EuclideanRing.js#L14
  -- https://en.m.wikipedia.org/wiki/Modulo_operation
  mod l1 l2 =
    let
      l2' = abs l2
    in
      ((l1 `rem` l2') + l2') `rem` l2'

instance euclideanRingLong'Unsigned :: EuclideanRing (Long' Unsigned) where
  degree = Int.floor <<< toNumber <<< abs
  div = quot
  mod = rem

-- Constructors

signedLongFromInt :: Int -> Long' Signed
signedLongFromInt = unsafeFromInt

unsignedLongFromInt :: Int -> Maybe (Long' Unsigned)
unsignedLongFromInt i
  | i >= 0 = Just $ unsafeFromInt i
  | otherwise = Nothing

unsafeFromInt :: forall s. SInfo s => Int -> Long' s
unsafeFromInt i = Long' $ runFn2 fromInt_ i (ffiSignedness (SignProxy :: SignProxy s))

fromLowHighBits :: forall s. SInfo s => Int -> Int -> Long' s
fromLowHighBits l h = Long' $ runFn3 fromBits_ l h (ffiSignedness (SignProxy :: SignProxy s))

fromNumber :: forall s. SInfo s => Bounded (Long' s) => Number -> Maybe (Long' s)
fromNumber n =
  if isValidNumber then Just $ Long' $ runFn2 fromNumber_ n (ffiSignedness p)
  else Nothing

  where
  isValidNumber = isWholeNumber_ n && isNumberInLongRange p n
  p = SignProxy :: SignProxy s

fromString :: forall s. SInfo s => String -> Maybe (Long' s)
fromString = fromStringAs decimal

fromStringAs :: forall s. SInfo s => Radix -> String -> Maybe (Long' s)
fromStringAs radix s =
  Long' <$> safeReadLong s (ffiSignedness (SignProxy :: SignProxy s)) radix

highBits :: forall s. Long' s -> Int
highBits (Long' l) = numberBitsToInt $ getHighBits_ l

lowBits :: forall s. Long' s -> Int
lowBits (Long' l) = numberBitsToInt $ getLowBits_ l

toString :: forall s. Long' s -> String
toString = toStringAs decimal

toStringAs :: forall s. Radix -> Long' s -> String
toStringAs r (Long' l) = toString_ l r

-- | Converts a `Long'` to a `Number`, possibly losing precision.
toNumber :: forall s. Long' s -> Number
toNumber (Long' l) = toNumber_ l

-- | Returns whether a `Long'` is `Even` or `Odd`.
parity :: forall s. Long' s -> Parity
parity l
  | even l = Even
  | otherwise = Odd

even :: forall s. Long' s -> Boolean
even (Long' l) = isEven_ l

odd :: forall s. Long' s -> Boolean
odd = not <<< even

positive :: forall s. Long' s -> Boolean
positive (Long' l) = isPositive_ l

negative :: forall s. Long' s -> Boolean
negative (Long' l) = isNegative_ l

quot :: forall s. Semiring (Long' s) => Long' s -> Long' s -> Long' s
quot (Long' x) (Long' y)
  | isZero_ y = zero
  | otherwise = Long' $ x `divide_` y

rem :: forall s. Semiring (Long' s) => Long' s -> Long' s -> Long' s
rem (Long' x) (Long' y)
  | isZero_ y = zero
  | otherwise = Long' $ x `modulo_` y

-- Bitwise operations

and :: forall s. Long' s -> Long' s -> Long' s
and (Long' x) (Long' y) = Long' $ and_ x y

or :: forall s. Long' s -> Long' s -> Long' s
or (Long' x) (Long' y) = Long' $ or_ x y

xor :: forall s. Long' s -> Long' s -> Long' s
xor (Long' x) (Long' y) = Long' $ xor_ x y

shl :: forall s. Long' s -> Long' s -> Long' s
shl (Long' x) (Long' y) = Long' $ shiftLeft_ x y

shr :: forall s. Long' s -> Long' s -> Long' s
shr (Long' x) (Long' y) = Long' $ shiftRight_ x y

zshr :: forall s. Long' s -> Long' s -> Long' s
zshr (Long' x) (Long' y) = Long' $ shiftRightUnsigned_ x y

complement :: forall s. Long' s -> Long' s
complement (Long' x) = Long' $ not_ x

-- Conversions

signedToUnsigned :: Long' Signed -> Long' Unsigned
signedToUnsigned (Long' x) = Long' $ toUnsigned_ x

unsignedToSigned :: Long' Unsigned -> Long' Signed
unsignedToSigned (Long' x) = Long' $ toSigned_ x

-- Utils

isNumberInLongRange :: forall s. Bounded (Long' s) => SignProxy s -> Number -> Boolean
isNumberInLongRange _ n =
  longBottomValueN <= n && n <= longTopValueN
  where
  longBottomValueN = toNumber (bottom :: Long' s)
  longTopValueN = toNumber (top :: Long' s)

foreign import numberBitsToInt :: Number -> Int

safeReadLong :: String -> IsUnsigned -> Radix -> Maybe Long
safeReadLong s isSigned radix =
  Nullable.toMaybe $ runFn3 safeReadLong_ s isSigned radix

foreign import safeReadLong_ :: Fn3 String IsUnsigned Radix (Nullable Long)

foreign import isWholeNumber_ :: Number -> Boolean

-- FFI.purs from here

-- | The foreign JavaScript `Long` object.
foreign import data Long :: Type

instance showLong :: Show Long where
  show l = toString_ l decimal <> suffix
    where
    suffix = case unsigned_ l of
      (IsUnsigned false) -> "l"
      (IsUnsigned true) -> "ul"

instance eqLong :: Eq Long where
  eq a b = equals_ a b && (unsigned_ a == unsigned_ b)

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

-- | Signed zero
foreign import zero_ :: Long

-- | Signed one
foreign import one_ :: Long

-- | Signed negative one TODO Unused?
foreign import negOne_ :: Long

-- | Unsigned zero
foreign import uzero_ :: Long

-- | Unsigned one
foreign import uone_ :: Long

-- | Maximum signed value
foreign import maxValue_ :: Long

-- | Minimum signed value
foreign import minValue_ :: Long

-- | Maximum unsigned value
foreign import maxUnsignedValue_ :: Long

--
-- Utilities
--

-- | Tests if the specified object is a Long
foreign import isLong_ :: Fn1 Foreign Boolean

-- | Returns a Long representing the 64 bit integer that comes by concatenating the given low and high bits. Each is assumed to use 32 bits.
foreign import fromBits_ :: Fn3 Int Int IsUnsigned Long

-- | Creates a Long from its byte representation.
foreign import fromBytes_ :: Fn3 (Array Int) IsUnsigned IsLittleEndian Long

-- | Creates a Long from its little endian byte representation.
foreign import fromBytesLE_ :: Fn2 (Array Int) IsUnsigned Long

-- | Creates a Long from its big endian byte representation.
foreign import fromBytesBE_ :: Fn2 (Array Int) IsUnsigned Long

-- | Returns a Long representing the given 32 bit integer value.
foreign import fromInt_ :: Fn2 Int IsUnsigned Long

-- | Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
foreign import fromNumber_ :: Fn2 Number IsUnsigned Long

-- | Returns a Long representation of the given string, written using the specified radix.
foreign import fromString_ :: EffectFn3 String IsUnsigned Radix Long

-- | Converts the specified value to a Long using the appropriate from* function for its type.
foreign import fromValue_ :: Fn2 Foreign IsUnsigned Long

--
-- Fields
--

foreign import unsigned_ :: Long -> IsUnsigned

--
-- Methods
--

-- | Returns the sum of this and the specified Long.
foreign import add_ :: Long -> Fn1 Long Long

-- | Returns the bitwise AND of this Long and the specified.
foreign import and_ :: Long -> Fn1 Long Long

-- | Compares this Long's value with the specified's. Returns 0 if they are the same, 1 if the this is greater and -1 if the given one is greater.
foreign import compare_ :: Long -> Fn1 Long Int

-- | Returns this Long divided by the specified.
foreign import divide_ :: Long -> Fn1 Long Long

-- | Tests if this Long's value equals the specified's.
-- | Note: The original API accepts any number like input.
-- | If provided with another Long, this does not compare signedness.
foreign import equals_ :: Long -> Fn1 Long Boolean

-- | Gets the high 32 bits as a signed integer.
-- | Note: Use Internal.getNumberBitsToInt to get back Int
foreign import getHighBits_ :: Long -> Number

-- | Gets the high 32 bits as an unsigned integer.
-- | Note: Use Internal.getNumberBitsToInt to get back Int
foreign import getHighBitsUnsigned_ :: Long -> Number

-- | Gets the low 32 bits as a signed integer.
-- | Use Internal.getNumberBitsToInt to get back Int
foreign import getLowBits_ :: Long -> Number

-- | Gets the low 32 bits as an unsigned integer.
-- | Use Internal.getNumberBitsToInt to get back Int
foreign import getLowBitsUnsigned_ :: Long -> Number

-- | Gets the number of bits needed to represent the absolute value of this Long.
foreign import getNumBitsAbs :: Long -> Int

-- | Tests if this Long's value is greater than the specified's.
foreign import greaterThan_ :: Long -> Fn1 Long Boolean

-- | Tests if this Long's value is greater than or equal the specified's.
foreign import greaterThanOrEqual_ :: Long -> Fn1 Long Boolean

-- | Tests if this Long's value is even.
foreign import isEven_ :: Long -> Boolean

-- | Tests if this Long's value is negative.
foreign import isNegative_ :: Long -> Boolean

-- | Tests if this Long's value is odd.
foreign import isOdd_ :: Long -> Boolean

-- | Tests if this Long's value is positive.
foreign import isPositive_ :: Long -> Boolean

-- | Tests if this Long's value equals zero.
foreign import isZero_ :: Long -> Boolean

-- | Tests if this Long's value is less than the specified's.
foreign import lessThan_ :: Long -> Fn1 Long Boolean

-- | Tests if this Long's value is less than or equal the specified's.
foreign import lessThanOrEqual_ :: Long -> Fn1 Long Boolean

-- | Returns this Long modulo the specified.
-- |
-- | The foreign implementation only works when the divisor is in the Int range.
foreign import modulo_ :: Long -> Fn1 Long Long

-- | Returns the product of this and the specified Long.
foreign import multiply_ :: Long -> Fn1 Long Long

-- | Negates this Long's value.
foreign import negate_ :: Long -> Long

-- | Returns the bitwise NOT of this Long.
foreign import not_ :: Long -> Long

-- | Tests if this Long's value differs from the specified's.
foreign import notEquals_ :: Long -> Fn1 Long Boolean

-- | Returns the bitwise OR of this Long and the specified.
foreign import or_ :: Long -> Fn1 Long Long

-- | Returns this Long with bits shifted to the left by the given amount.
foreign import shiftLeft_ :: Long -> Fn1 Long Long

-- | Returns this Long with bits arithmetically shifted to the right by the given amount.
foreign import shiftRight_ :: Long -> Fn1 Long Long

-- | Returns this Long with bits logically shifted to the right by the given amount.
foreign import shiftRightUnsigned_ :: Long -> Fn1 Long Long

-- | Returns this Long with bits rotated to the left by the given amount.
foreign import rotateLeft_ :: Long -> Fn1 Long Long

-- | Returns this Long with bits rotated to the right by the given amount.
foreign import rotateRight_ :: Long -> Fn1 Long Long

-- | Returns the difference of this and the specified Long.
foreign import subtract_ :: Long -> Fn1 Long Long

-- | Converts this Long to its byte representation.
foreign import toBytes_ :: Long -> IsLittleEndian -> Array Int

-- | Converts the Long to a 32 bit integer, assuming it is a 32 bit integer.
foreign import toInt_ :: Long -> Int

-- | Converts the Long to a the nearest floating-point representation of this value (double, 53 bit mantissa).
foreign import toNumber_ :: Long -> Number

-- | Converts this Long to signed.
foreign import toSigned_ :: Long -> Long

-- | Converts the Long to a string written in the specified radix.
foreign import toString_ :: Long -> Fn1 Radix String

-- | Converts this Long to unsigned.
foreign import toUnsigned_ :: Long -> Long

-- | Returns the bitwise XOR of this Long and the given one.
foreign import xor_ :: Long -> Fn1 Long Long
