{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BytesSpec where

import Test.Hspec (Expectation, Spec, it, shouldBe, describe)
import Test.Hspec.QuickCheck (prop)

import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString as BS
import Data.MonoTraversable (Element)
import Data.Syntax (Syntax (Seq, anyChar, char))
import Data.Syntax.Attoparsec.ByteString (WrappedParser, getParser_)
import Data.Syntax.Bytes (bigEndian, littleEndian)
import qualified Data.Syntax.Bytes.BigEndian as BigEndian
import qualified Data.Syntax.Bytes.LittleEndian as LittleEndian
import Data.Word (Word16, Word32, Word64, Word8)

roundtrip ::
    forall a syn.
    (Show a, Element (Seq syn) ~ a, Syntax syn) =>
    (syn () a -> WrappedParser () a) ->
    (a -> [Word8]) ->
    a ->
    Expectation
roundtrip encoding toBytes x = do
    (parse . BS.pack . toBytes) x `shouldBe` Right x
  where
    parse = AP.parseOnly (getParser_ $ encoding anyChar)

spec :: Spec
spec = do
    describe "little endian" $ do
        prop "Word16" $ roundtrip @Word16 littleEndian LittleEndian.toBytes
        prop "Word32" $ roundtrip @Word32 littleEndian LittleEndian.toBytes
        prop "Word64" $ roundtrip @Word64 littleEndian LittleEndian.toBytes

    describe "big endian" $ do
        prop "Word16" $ roundtrip @Word16 bigEndian BigEndian.toBytes
        prop "Word32" $ roundtrip @Word32 bigEndian BigEndian.toBytes
        prop "Word64" $ roundtrip @Word64 bigEndian BigEndian.toBytes
