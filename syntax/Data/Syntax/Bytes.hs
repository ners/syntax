module Data.Syntax.Bytes where

import qualified Data.Syntax.Bytes.LittleEndian as LittleEndian
import Data.Syntax
import Data.MonoTraversable (Element)
import Data.Word

littleEndian :: forall a syn x y. (Syntax syn, Element (Seq syn) ~ Word8, LittleEndian.Bytes a) => (LittleEndian.ByteWise a syn) x y -> syn x y
littleEndian (LittleEndian.ByteWise s) = s
