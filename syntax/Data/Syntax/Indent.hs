{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.Syntax.Indent
Description :  Simple indentation.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental

Provides a very simple indentation as a category transformer.
-}
module Data.Syntax.Indent (
    Indent,
    runIndent,
    breakLine,
    indented
    ) where

import Control.Category
import Control.Category.Reader
import Control.Category.Structures
import Control.SIArrow
import Data.Syntax
import Data.Syntax.Char
import Data.Syntax.Combinator
import Prelude hiding (takeWhile, take, id, (.))

-- | Adds indentation to a syntax description.
newtype Indent cat a b = Indent { getIndent :: ReaderCT (Int, cat () ()) cat a b }
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

instance CatTrans Indent where
    clift = Indent . clift

-- Generalized newtype deriving cannot derive this :(

instance Syntax syn => Syntax (Indent syn) where
    type Seq (Indent syn) = Seq syn
    anyChar = Indent anyChar
    char = Indent . char
    notChar = Indent . notChar
    satisfy = Indent . satisfy
    satisfyWith ai = Indent . satisfyWith ai
    string = Indent . string
    take = Indent . take
    takeWhile = Indent . takeWhile
    takeWhile1 = Indent . takeWhile1
    takeTill = Indent . takeTill
    takeTill1 = Indent . takeTill1

instance SyntaxChar syn => SyntaxChar (Indent syn) where
    decimal = Indent decimal
    hexadecimal = Indent hexadecimal
    scientific = Indent scientific
    realFloat = Indent realFloat

-- | @runIndent m tab@ runs the 'Indent' transformer using @tab@ once for each
-- level of indentation.
runIndent :: Indent cat a b -> cat () () -> cat a b
runIndent (Indent m) tab = runReaderCT m (0, tab)

-- | Inserts a new line and correct indentation, but does not 
-- require any formatting when parsing (it just skips all white space).
breakLine :: SyntaxChar syn => Indent syn () ()
breakLine = Indent . ReaderCT $ \(i, tab) -> 
    opt (char '\n') /* opt (sireplicate_ i tab) /* spaces_

-- | Increases the indentation level of its argument by one.
indented :: Indent cat a b -> Indent cat a b
indented (Indent f) = Indent . ReaderCT $ \(i, tab) -> runReaderCT f (i + 1, tab)
