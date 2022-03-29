module Data.Long.Internal
       ( Long'
       , Long
       , ULong
       , kind Signedness
       , Signed
       , Unsigned
       , class SInfo
       , ffiSignedness
       , toInt
       , SignProxy(..)
       , signedLongFromInt
       , unsignedLongFromInt
       , unsafeFromInt
       , fromLowHighBits
       , fromNumber
       , fromString
       , fromStringAs
       , highBits
       , lowBits
       , toString
       , toStringAs
       , toNumber
       , parity
       , even
       , odd
       , positive
       , negative
       , quot
       , rem
       -- Bitwise Operations
       , and
       , or
       , xor
       , shl
       , shr
       , zshr
       , complement
       -- Conversions
       , signedToUnsigned
       , unsignedToSigned
       -- Utils
       , numberBitsToInt
       ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn2, runFn3)
import Data.Int (Parity(..), Radix, decimal)
import Data.Int as Int
import Data.Long.FFI as FFI
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Ord (abs)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

foreign import kind Signedness

foreign import data Signed :: Signedness
foreign import data Unsigned :: Signedness

data SignProxy (s :: Signedness) = SignProxy

class SInfo (s :: Signedness) where
  ffiSignedness :: SignProxy s -> FFI.IsUnsigned
  toInt :: Long' s -> Maybe Int

instance infoSigned :: SInfo Signed where
  ffiSignedness _ = (FFI.IsUnsigned false)
  toInt l =
    let low = lowBits l
        high = highBits l
    in if (high == 0 && low >= 0) || (high == -1 && low < 0)
       then Just low
       else Nothing

instance infoUnsigned :: SInfo Unsigned where
  ffiSignedness _ = (FFI.IsUnsigned true)
  toInt l =
    let low = lowBits l
        high = highBits l
    in if high == 0 && low > 0
       then Just low
       else Nothing

newtype Long' (s :: Signedness) = Long' FFI.Long
type Long = Long' Signed
type ULong = Long' Unsigned

instance showLong' :: Show (Long' s) where
  show (Long' l) = show l

instance eqLong' :: Eq (Long' s) where
  eq (Long' l1) (Long' l2) = FFI.equals l1 l2

instance ordLong' :: Ord (Long' s) where
  compare (Long' l1) (Long' l2) = case FFI.compare l1 l2 of
    0 -> EQ
    x | x > 0 -> GT
    _ -> LT

instance boundedLong'Signed :: Bounded (Long' Signed) where
  top = Long' $ FFI.maxValue
  bottom = Long' $ FFI.minValue

instance boundedLong'Unsigned :: Bounded (Long' Unsigned) where
  top = Long' $ FFI.maxUnsignedValue
  bottom = Long' $ FFI.uzero

instance semiringLong'Signed :: Semiring (Long' Signed) where
  add (Long' l1) (Long' l2) = Long' $ FFI.add l1 l2
  zero = Long' $ FFI.zero
  mul (Long' l1) (Long' l2) = Long' $ FFI.multiply l1 l2
  one = Long' $ FFI.one

instance semiringLong'Unsigned :: Semiring (Long' Unsigned) where
  add (Long' l1) (Long' l2) = Long' $ FFI.add l1 l2
  zero = Long' $ FFI.uzero
  mul (Long' l1) (Long' l2) = Long' $ FFI.multiply l1 l2
  one = Long' $ FFI.uone

instance ringLong'Signed :: Ring (Long' Signed) where
  sub (Long' l1) (Long' l2) = Long' $ FFI.subtract l1 l2

instance ringLong'Unsigned :: Ring (Long' Unsigned) where
  sub (Long' l1) (Long' l2) = Long' $ FFI.subtract l1 l2

instance commutativeRingLong'Signed :: CommutativeRing (Long' Signed)
instance commutativeRingLong'Unsigned :: CommutativeRing (Long' Unsigned)

instance euclideanRingLong'Signed :: EuclideanRing (Long' Signed) where
  degree = Int.floor <<< toNumber <<< abs
  div l1 l2 =
    (l1 - (l1 `mod` l2)) `quot` l2

  mod l1 l2  =
   let l2' = abs l2
   in ((l1 `rem` l2') + l2') `rem` l2'

instance euclideanRingLong'Unsigned :: EuclideanRing (Long' Unsigned) where
  degree = Int.floor <<< toNumber <<< abs
  div = quot
  mod = rem

instance arbitraryLong' :: SInfo s => Arbitrary (Long' s) where
  arbitrary = fromLowHighBits <$> arbitrary <*> arbitrary

-- Constructors

signedLongFromInt :: Int -> Long' Signed
signedLongFromInt = unsafeFromInt

unsignedLongFromInt :: Int -> Maybe (Long' Unsigned)
unsignedLongFromInt i
  | i >= 0    = Just $ unsafeFromInt i
  | otherwise = Nothing

unsafeFromInt :: forall s. SInfo s => Int -> Long' s
unsafeFromInt i = Long' $ runFn2 FFI.fromInt i (ffiSignedness (SignProxy :: SignProxy s))

fromLowHighBits :: forall s. SInfo s => Int -> Int -> Long' s
fromLowHighBits l h = Long' $ runFn3 FFI.fromBits l h (ffiSignedness (SignProxy :: SignProxy s))

fromNumber :: forall s. SInfo s => Bounded (Long' s) => Number -> Maybe (Long' s)
fromNumber n =
  if isValidNumber
  then Just $ Long' $ runFn2 FFI.fromNumber n (ffiSignedness p)
  else Nothing

  where
    isValidNumber = isWholeNumber n && isNumberInLongRange p n
    p = SignProxy :: SignProxy s

fromString :: forall s. SInfo s => String -> Maybe (Long' s)
fromString = fromStringAs decimal

fromStringAs :: forall s. SInfo s => Radix -> String -> Maybe (Long' s)
fromStringAs radix s =
  Long' <$> safeReadLong s (ffiSignedness (SignProxy :: SignProxy s)) radix

highBits :: forall s. Long' s -> Int
highBits (Long' l) = numberBitsToInt $ FFI.getHighBits l

lowBits :: forall s. Long' s -> Int
lowBits (Long' l) = numberBitsToInt $ FFI.getLowBits l

toString :: forall s. Long' s -> String
toString = toStringAs decimal

toStringAs :: forall s. Radix -> Long' s -> String
toStringAs r (Long' l) = FFI.toString l r

--| Converts a `Long'` to a `Number`, possibly losing precision.
toNumber :: forall s. Long' s -> Number
toNumber (Long' l) = FFI.toNumber l

--| Returns whether a `Long'` is `Even` or `Odd`.
parity :: forall s. Long' s -> Parity
parity l | even l    = Even
          | otherwise = Odd

even :: forall s. Long' s -> Boolean
even (Long' l) = FFI.isEven l

odd :: forall s. Long' s -> Boolean
odd = not <<< even

positive :: forall s. Long' s -> Boolean
positive (Long' l) = FFI.isPositive l

negative :: forall s. Long' s -> Boolean
negative (Long' l) = FFI.isNegative l

quot :: forall s. Semiring (Long' s) => Long' s -> Long' s -> Long' s
quot (Long' x) (Long' y)
  | FFI.isZero y = zero
  | otherwise = Long' $ x `FFI.divide` y

rem :: forall s. Semiring (Long' s) => Long' s -> Long' s -> Long' s
rem (Long' x) (Long' y)
  | FFI.isZero y = zero
  | otherwise = Long' $ x `FFI.modulo` y

-- Bitwise operations

and :: forall s. Long' s -> Long' s -> Long' s
and (Long' x) (Long' y) = Long' $ FFI.and x y

or :: forall s. Long' s -> Long' s -> Long' s
or (Long' x) (Long' y) = Long' $ FFI.or x y

xor :: forall s. Long' s -> Long' s -> Long' s
xor (Long' x) (Long' y) = Long' $ FFI.xor x y

shl :: forall s. Long' s -> Long' s -> Long' s
shl (Long' x) (Long' y) = Long' $ FFI.shiftLeft x y

shr :: forall s. Long' s -> Long' s -> Long' s
shr (Long' x) (Long' y) = Long' $ FFI.shiftRight x y

zshr :: forall s. Long' s -> Long' s -> Long' s
zshr (Long' x) (Long' y) = Long' $ FFI.shiftRightUnsigned x y

complement :: forall s. Long' s -> Long' s
complement (Long' x) = Long' $ FFI.not x

-- Conversions

signedToUnsigned :: Long' Signed -> Long' Unsigned
signedToUnsigned (Long' x) = Long' $ FFI.toUnsigned x

unsignedToSigned :: Long' Unsigned -> Long' Signed
unsignedToSigned (Long' x) = Long' $ FFI.toSigned x

-- Utils

signedProxy :: SignProxy Signed
signedProxy = SignProxy

unsignedProxy :: SignProxy Unsigned
unsignedProxy = SignProxy

isNumberInLongRange :: forall s. Bounded (Long' s) => SignProxy s -> Number -> Boolean
isNumberInLongRange p n =
  longBottomValueN <= n && n <= longTopValueN
  where
    longBottomValueN = toNumber (bottom :: Long' s)
    longTopValueN = toNumber (top :: Long' s)

foreign import numberBitsToInt :: Number -> Int

safeReadLong :: String -> FFI.IsUnsigned -> Radix -> Maybe FFI.Long
safeReadLong s isSigned radix =
  Nullable.toMaybe $ runFn3 _safeReadLong s isSigned radix

foreign import _safeReadLong :: Fn3 String FFI.IsUnsigned Radix (Nullable FFI.Long)

foreign import isWholeNumber :: Number -> Boolean
