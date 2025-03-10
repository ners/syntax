{-# LANGUAGE ConstraintKinds #-}

module Data.Syntax.Bytes.Util where

import Data.Bits
import Data.Syntax (Syntax, Seq)
import Data.MonoTraversable (Element)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Control.Lens (Iso', iso)

-- | A useful synonym for Syntax with Word8 sequences.
type SyntaxByte syn = (Syntax syn, Element (Seq syn) ~ Word8)

-- | A useful synonym for Syntax with ByteString sequences.
type SyntaxByteString syn = (Syntax syn, Seq syn ~ ByteString)

coerceBits :: (FiniteBits a, FiniteBits b) => a -> b
coerceBits a = foldr (.|.) b [bit i | i <- [0 .. bitSize - 1], testBit a i]
  where
    b = zeroBits
    bitSize = min (finiteBitSize a) (finiteBitSize b)

coerceBitsIso :: (FiniteBits a, FiniteBits b) => Iso' a b
coerceBitsIso = iso coerceBits coerceBits
