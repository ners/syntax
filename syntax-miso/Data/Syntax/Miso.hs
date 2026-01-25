{- |
Module      :  Data.Syntax.Miso
Description :  Syntax instance for Miso.Util.Parser.
Copyright   :  (c) ners
License     :  Apache-2.0

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental

Provides a Syntax instance for Miso.Util.Parser.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Syntax.Miso where

import           Control.Arrow (Kleisli(..))
import           Control.Category (Category)
import           Control.Category.Structures
import           Control.Monad
import           Control.SIArrow
import           Data.Syntax
import           Miso.Prelude
import qualified Miso.Util.Parser as Miso
import qualified Miso.String as MisoString

-- | A wrapped 'Data.Attoparsec.Text.Parser'.
newtype WrappedParser a b = Wrapped (Kleisli (Miso.ParserT () [Char] []) a b)
    deriving newtype (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: Miso.Parser Char b -> WrappedParser a b
wrap = Wrapped . Kleisli . const

unwrap :: WrappedParser a b -> a -> Miso.Parser Char b
unwrap (Wrapped f) = runKleisli f

instance Syntax WrappedParser where
    type Seq WrappedParser = MisoString
    anyChar = satisfy $ const True
    char = wrap . void . Miso.token_
    satisfy = wrap . Miso.satisfy

-- | Extracts the parser.
getParser :: WrappedParser a b -> a -> Miso.Parser Char b
getParser (Wrapped (Kleisli f)) = f

-- | Extracts the parser.
getParser_ :: WrappedParser () b -> Miso.Parser Char b
getParser_ (Wrapped (Kleisli f)) = f ()

parse :: (Show b) => WrappedParser () b -> MisoString -> Either MisoString b
parse p = either (Left . toMisoString . show) Right . Miso.parse (getParser_ p) . MisoString.unpack
