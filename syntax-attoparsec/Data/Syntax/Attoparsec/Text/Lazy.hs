{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Attoparsec.Text.Laxy
Description :  Syntax instance for Attoparsec.Text.Lazy.Parser.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental

Provides a Syntax instance for Attoparsec.Text.Lazy.Parser.
-}
module Data.Syntax.Attoparsec.Text.Lazy (
    WrappedParser,
    getParser,
    getParser_
    ) where

import           Control.Arrow (Kleisli(..))
import           Control.Category
import           Control.Category.Structures
import           Control.Monad
import           Control.SIArrow
import qualified Data.Attoparsec.Text.Lazy as AP
import           Data.Scientific
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Text (Text)
import           Data.Text.Lazy (fromStrict)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (id, (.))

-- | A wrapped 'Data.Attoparsec.Text.Parser'.
newtype WrappedParser a b = Wrapped (Kleisli AP.Parser a b)
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: AP.Parser b -> WrappedParser a b
wrap = Wrapped . Kleisli . const

unwrap :: WrappedParser a b -> a -> AP.Parser b
unwrap (Wrapped f) = runKleisli f

instance Syntax WrappedParser where
    type Seq WrappedParser = Text
    anyChar = wrap AP.anyChar
    char = wrap . void . AP.char
    notChar = wrap . AP.notChar
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
    isolate p = Wrapped $ Kleisli $ either fail return . AP.parseOnly (unwrap p ()) . fromStrict

instance SyntaxChar WrappedParser where
    decimal = wrap AP.decimal
    hexadecimal = wrap AP.hexadecimal
    realFloat = wrap $ fmap toRealFloat AP.scientific
    scientific = wrap AP.scientific

-- | Extracts the parser.
getParser :: WrappedParser a b -> a -> AP.Parser b
getParser (Wrapped (Kleisli f)) = f

-- | Extracts the parser.
getParser_ :: WrappedParser () b -> AP.Parser b
getParser_ (Wrapped (Kleisli f)) = f ()
