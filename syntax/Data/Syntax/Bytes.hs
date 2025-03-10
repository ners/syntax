module Data.Syntax.Bytes
    ( module Data.Syntax.Bytes
    , module Data.Syntax.Bytes.BigEndian
    , module Data.Syntax.Bytes.LittleEndian
    , module Data.Syntax.Bytes.Util
    ) where

import Data.Syntax.Bytes.LittleEndian (littleEndian)
import Data.Syntax.Bytes.BigEndian (bigEndian)
import Data.Syntax.Bytes.Util (SyntaxByte, SyntaxByteString)
import qualified Data.Syntax.Bytes.Util as Util
import Data.ByteString (ByteString)
import Control.Lens.SemiIso (constant)
import Control.SIArrow ((#>>), SIArrow (sibind), (/$/))
import Control.Lens (iso)
import qualified Data.Syntax
import qualified Data.ByteString
import Control.Arrow ((>>>))
import Data.Int (Int8)
import Data.Word (Word8)
import Data.Syntax (Syntax(char), satisfy, anyChar)

-- | Read an integral length, followed by length bytes
stringWithLen :: forall i syn. (SyntaxByteString syn, Integral i) => syn () i -> syn () ByteString
stringWithLen getLen = getLen >>> sibind (iso f g)
  where
    f :: i -> syn i ByteString
    f len = constant len #>> Data.Syntax.take (fromIntegral len)
    g :: ByteString -> syn i ByteString
    g = f . fromIntegral . Data.ByteString.length

