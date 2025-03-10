{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BytesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Syntax.Bytes (bigEndian, littleEndian)
import qualified Data.Syntax.Bytes.BigEndian as BigEndian
import qualified Data.Syntax.Bytes.LittleEndian as LittleEndian
import Data.Word
import Data.Int
import Data.Bits (xor)
import Control.Lens

roundtrip :: forall a b. (Show a, Eq a) => (b -> a) -> (a -> b) -> a -> Expectation
roundtrip fromBytes toBytes x = (fromBytes . toBytes) x `shouldBe` x

spec :: Spec
spec = do
    describe "big endian" $ do
        prop "encodes big endian" $ \(x :: Word32) ->
            BigEndian.toBytes (x `xor` 1) `shouldBe` (BigEndian.toBytes x & _last %~ (`xor` 1))

        it "knows the size of Int8" $ BigEndian.byteSize @Int8 `shouldBe` 1
        prop "roundtrips Int8" $ roundtrip @Int8 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Int16" $ BigEndian.byteSize @Int16 `shouldBe` 2
        prop "roundtrips Int16" $ roundtrip @Int16 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Int32" $ BigEndian.byteSize @Int32 `shouldBe` 4
        prop "roundtrips Int32" $ roundtrip @Int32 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Int64" $ BigEndian.byteSize @Int64 `shouldBe` 8
        prop "roundtrips Int64" $ roundtrip @Int64 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Word8" $ BigEndian.byteSize @Word8 `shouldBe` 1
        prop "roundtrips Word8" $ roundtrip @Word8 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Word16" $ BigEndian.byteSize @Word16 `shouldBe` 2
        prop "roundtrips Word16" $ roundtrip @Word16 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Word32" $ BigEndian.byteSize @Word32 `shouldBe` 4
        prop "roundtrips Word32" $ roundtrip @Word32 BigEndian.fromBytes BigEndian.toBytes

        it "knows the size of Word64" $ BigEndian.byteSize @Word64 `shouldBe` 8
        prop "roundtrips Word64" $ roundtrip @Word64 BigEndian.fromBytes BigEndian.toBytes

    describe "little endian" $ do
        prop "encodes little endian" $ \(x :: Word32) ->
            LittleEndian.toBytes (x `xor` 1) `shouldBe` (LittleEndian.toBytes x & _head %~ (`xor` 1))

        it "knows the size of Int8" $ LittleEndian.byteSize @Int8 `shouldBe` 1
        prop "roundtrips Int8" $ roundtrip @Int8 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Int16" $ LittleEndian.byteSize @Int16 `shouldBe` 2
        prop "roundtrips Int16" $ roundtrip @Int16 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Int32" $ LittleEndian.byteSize @Int32 `shouldBe` 4
        prop "roundtrips Int32" $ roundtrip @Int32 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Int64" $ LittleEndian.byteSize @Int64 `shouldBe` 8
        prop "roundtrips Int64" $ roundtrip @Int64 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Word8" $ LittleEndian.byteSize @Word8 `shouldBe` 1
        prop "roundtrips Word8" $ roundtrip @Word8 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Word16" $ LittleEndian.byteSize @Word16 `shouldBe` 2
        prop "roundtrips Word16" $ roundtrip @Word16 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Word32" $ LittleEndian.byteSize @Word32 `shouldBe` 4
        prop "roundtrips Word32" $ roundtrip @Word32 LittleEndian.fromBytes LittleEndian.toBytes

        it "knows the size of Word64" $ LittleEndian.byteSize @Word64 `shouldBe` 8
        prop "roundtrips Word64" $ roundtrip @Word64 LittleEndian.fromBytes LittleEndian.toBytes
