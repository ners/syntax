{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Util where

import Test.Hspec

import Data.Syntax.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString as BS
import Data.MonoTraversable
import Data.Syntax

import Data.Word

roundtrip :: forall a syn. (Show a, Element (Seq syn) ~ a, Syntax syn)
          => (syn () a -> WrappedParser () a) -> (a -> [Word8]) -> a -> Expectation
roundtrip encoding toBytes x = do
    let s = encoding anyChar
        parse = AP.parseOnly (getParser_ s)
    (parse . BS.pack . toBytes) x
      `shouldBe` Right x
