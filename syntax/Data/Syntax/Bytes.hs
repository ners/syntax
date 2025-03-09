module Data.Syntax.Bytes where

import qualified Data.Syntax.Bytes.LittleEndian as LittleEndian
import qualified Data.Syntax.Bytes.BigEndian as BigEndian
import Data.Syntax
import Data.MonoTraversable (Element)
import Data.Word

littleEndian :: forall a syn x y. (Syntax syn, Element (Seq syn) ~ Word8, LittleEndian.Bytes a) => (LittleEndian.ByteWise a syn) x y -> syn x y
littleEndian (LittleEndian.ByteWise s) = s

bigEndian :: forall a syn x y. (Syntax syn, Element (Seq syn) ~ Word8, BigEndian.Bytes a) => (BigEndian.ByteWise a syn) x y -> syn x y
bigEndian (BigEndian.ByteWise s) = s
