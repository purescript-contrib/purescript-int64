module Data.Int64.Gen
  ( chooseInt64
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Int64 (Int64, fromLowHighBits, toSigned, toUnsigned)
import Data.UInt64 (UInt64, rem)

topsigned :: UInt64
topsigned = toUnsigned top

-- | map
-- | signed bottom → zero
-- | zero → signed top + 1
-- | signed top -> unsigned top
mapToUInt64 :: Int64 -> UInt64
mapToUInt64 x = if x >= zero then toUnsigned x + topsigned + one else toUnsigned (top + x) + one

-- | map
-- | zero → signed bottom
-- | signed top + 1 → zero
-- | unsigned top → signed top
mapToInt64 :: UInt64 -> Int64
mapToInt64 x = if x > topsigned then toSigned (x - topsigned - one) else toSigned x - top - one

chooseInt64 :: forall m. MonadGen m => Int64 -> Int64 -> m Int64
chooseInt64 a b = do
  random <- fromLowHighBits <$> chooseInt bottom top <*> chooseInt bottom top
  -- We cannot subtract an Int64 from a greater Int64. But we can subtract a
  -- UInt64 from a greater UInt64. So map the random Int64 to a UInt64,
  -- constrain it between a and b, then map it back.
  let
    randomu = mapToUInt64 random
    au = mapToUInt64 a
    bu = mapToUInt64 b
  pure $ mapToInt64 $ (randomu `rem` (bu - au)) + au

