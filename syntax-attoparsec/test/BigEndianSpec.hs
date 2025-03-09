{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module BigEndianSpec where

import Util

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString as BS
import Data.Syntax
import Data.Syntax.Bytes
import Data.Syntax.Bytes.BigEndian (Bytes(toBytes))
import Data.Syntax.Attoparsec.ByteString
import Data.Word

spec :: Spec
spec = do
  it "parses big endian" $ do
    let s = bigEndian @Word16 $ char 0x1234
        parse = AP.parseOnly (getParser_ s)
    (parse . BS.pack) [0x12, 0x34]
      `shouldBe` Right ()

  prop "parses Word16" $ roundtrip @Word16 bigEndian toBytes
  prop "parses Word32" $ roundtrip @Word32 bigEndian toBytes
  prop "parses Word64" $ roundtrip @Word64 bigEndian toBytes
