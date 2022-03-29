
-- | This module defines bitwise operations for the `Long` type.
module Data.Long.Bits
       ( and, (.&.)
       , or, (.|.)
       , xor, (.^.)
       , shl
       , shr
       , zshr
       , complement
       ) where

import Data.Long (Long)
import Data.Long.Internal as Internal

-- | Bitwise AND.
and :: Long -> Long -> Long
and = Internal.and

infixl 10 and as .&.

-- | Bitwise OR.
or :: Long -> Long -> Long
or = Internal.or

infixl 10 or as .|.

-- | Bitwise XOR.
xor :: Long -> Long -> Long
xor = Internal.xor

infixl 10 xor as .^.

-- | Bitwise shift left.
shl :: Long -> Long -> Long
shl = Internal.shl

-- | Bitwise shift right.
shr :: Long -> Long -> Long
shr = Internal.shr

-- | Bitwise zero-fill shift right.
zshr :: Long -> Long -> Long
zshr = Internal.zshr

-- | Bitwise NOT.
complement :: Long -> Long
complement = Internal.complement
