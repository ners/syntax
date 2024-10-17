{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Attoparsec.ByteString
Description :  Syntax instance for Attoparsec.ByteString.Parser.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental

Provides a Syntax instance for Attoparsec.ByteString.Parser.
-}
module Data.Syntax.Attoparsec.ByteString (
    WrappedParser,
    getParser,
    getParser_
    ) where

import           Control.Arrow (Kleisli(..))
import           Control.Category
import           Control.Category.Structures
import           Control.Monad
import           Control.SIArrow
import qualified Data.Attoparsec.ByteString as AP
import           Data.ByteString (ByteString)
import           Data.Syntax
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (id, (.))

-- | A wrapped 'Data.Attoparsec.ByteString.Parser'.
newtype WrappedParser a b = Wrapped (Kleisli AP.Parser a b)
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: AP.Parser b -> WrappedParser a b
wrap = Wrapped . Kleisli . const

unwrap :: WrappedParser a b -> a -> AP.Parser b
unwrap (Wrapped f) = runKleisli f

instance Syntax WrappedParser where
    type Seq WrappedParser = ByteString
    anyChar = wrap AP.anyWord8
    char = wrap . void . AP.word8
    notChar = wrap . AP.notWord8
    satisfy = wrap . AP.satisfy
    string = wrap . void . AP.string
    take = wrap . AP.take
    takeWhile = wrap . AP.takeWhile
    takeWhile1 = wrap . AP.takeWhile1
    takeTill = wrap . AP.takeTill
    vecN n f = wrap $ V.replicateM n $ unwrap f ()
    ivecN n f = wrap $ V.generateM n $ fmap snd . unwrap f
    uvecN n f = wrap $ VU.replicateM n $ unwrap f ()
    uivecN n f = wrap $ VU.generateM n $ fmap snd . unwrap f

instance Isolable WrappedParser where
    isolate p = Wrapped $ Kleisli $ either fail return . AP.parseOnly (unwrap p ())

-- | Extracts the parser.
getParser :: WrappedParser a b -> a -> AP.Parser b
getParser (Wrapped (Kleisli f)) = f

-- | Extracts the parser.
getParser_ :: WrappedParser () b -> AP.Parser b
getParser_ (Wrapped (Kleisli f)) = f ()
