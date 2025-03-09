{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module LittleEndianSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Syntax.Bytes.LittleEndian
import Data.Word

spec :: Spec
spec = do
  prop "coerces Word64" $ \(x :: Word64) -> do
    x `shouldBe` coerceBits x

  it "encodes little endian" $
    fromBytes @Word32 [0x78, 0x56, 0x34, 0x12] `shouldBe` 0x12345678

  it "knows the size of Word8" $ byteSize @Word8 `shouldBe` 1
  prop "roundtrips Word8" $ \(x :: Word8) -> do
    x `shouldBe` (fromBytes . toBytes) x

  it "knows the size of Word16" $ byteSize @Word16 `shouldBe` 2
  prop "roundtrips Word16" $ \(x :: Word16) -> do
    x `shouldBe` (fromBytes . toBytes) x

  it "knows the size of Word32" $ byteSize @Word32 `shouldBe` 4
  prop "roundtrips Word32" $ \(x :: Word32) -> do
    x `shouldBe` (fromBytes . toBytes) x

  it "knows the size of Word64" $ byteSize @Word64 `shouldBe` 8
  prop "roundtrips Word64" $ \(x :: Word64) -> do
    x `shouldBe` (fromBytes . toBytes) x
