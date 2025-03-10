module Data.Syntax.Bytes.Util where

import Data.Bits

coerceBits :: (FiniteBits a, FiniteBits b) => a -> b
coerceBits a = foldr (.|.) b [bit i | i <- [0 .. bitSize - 1], testBit a i]
  where
    b = zeroBits
    bitSize = min (finiteBitSize a) (finiteBitSize b)
