module Data.UInt64.Gen
  ( chooseUInt64
  ) where

import Prelude
import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.UInt64 (UInt64, fromLowHighBits, rem)

chooseUInt64 :: forall m. MonadGen m => UInt64 -> UInt64 -> m UInt64
chooseUInt64 a b = do
  random <- fromLowHighBits <$> chooseInt bottom top <*> chooseInt bottom top
  pure $ (random `rem` (b - a)) + a
