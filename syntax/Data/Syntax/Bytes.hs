{-# LANGUAGE ConstraintKinds #-}

module Data.Syntax.Bytes where

import qualified Data.Syntax.Bytes.LittleEndian as LittleEndian
import qualified Data.Syntax.Bytes.BigEndian as BigEndian
import Data.Syntax
import Data.MonoTraversable (Element)
import Data.Word (Word8)
import Data.ByteString (ByteString)

-- | A useful synonym for Syntax with Word8 sequences.
type SyntaxByte syn = (Syntax syn, Element (Seq syn) ~ Word8)

-- | A useful synonym for Syntax with ByteString sequences.
type SyntaxByteString syn = (Syntax syn, Seq syn ~ ByteString)

littleEndian :: forall a syn x y. (SyntaxByte syn, LittleEndian.Bytes a) => (LittleEndian.ByteWise a syn) x y -> syn x y
littleEndian (LittleEndian.ByteWise s) = s

bigEndian :: forall a syn x y. (SyntaxByte syn, BigEndian.Bytes a) => (BigEndian.ByteWise a syn) x y -> syn x y
bigEndian (BigEndian.ByteWise s) = s
