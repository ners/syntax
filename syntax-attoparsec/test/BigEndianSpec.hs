{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module BigEndianSpec where

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

  prop "parses Word16" $ \(x :: Word16) -> do
    let s = bigEndian @Word16 $ anyChar
        parse = AP.parseOnly (getParser_ s)
    (parse . BS.pack . toBytes) x
      `shouldBe` Right x

  prop "parses Word32" $ \(x :: Word32) -> do
    let s = bigEndian @Word32 $ anyChar
        parse = AP.parseOnly (getParser_ s)
    (parse . BS.pack . toBytes) x
      `shouldBe` Right x

  prop "parses Word64" $ \(x :: Word64) -> do
    let s = bigEndian @Word64 $ anyChar
        parse = AP.parseOnly (getParser_ s)
    (parse . BS.pack . toBytes) x
      `shouldBe` Right x
