{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{- |
Module      :  Data.Syntax.Char
Description :  Char specific combinators.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental

Common combinators that work with sequences of chars.

-}
module Data.Syntax.Char (
    SyntaxChar(..),
    SyntaxText,
    spaces,
    spaces_,
    spaces1,
    endOfLine,
    ) where

import Control.Category.Reader
import Control.Category.Structures
import Control.Lens.SemiIso
import Control.SIArrow
import Data.Bits
import Data.Char
import Data.MonoTraversable
import Data.Syntax
import Data.Syntax.Combinator
import Data.Text (Text)
import Control.Lens (iso)

-- | Syntax constrainted to sequences of chars.
type SyntaxChar syn = (Syntax syn, Element (Seq syn) ~ Char)

-- | Syntax constrainted to sequences of chars.
type SyntaxText syn = (SyntaxChar syn, Seq syn ~ Text)

-- | Accepts zero or more spaces. Generates a single space.
spaces :: SyntaxChar syn => syn () ()
spaces = opt spaces1

-- | Accepts zero or more spaces. Generates no output.
spaces_ :: SyntaxChar syn => syn () ()
spaces_ = opt_ spaces1

-- | Accepts one or more spaces. Generates a single space.
spaces1 :: SyntaxChar syn => syn () ()
spaces1 = constant (opoint ' ') /$/ takeWhile1 isSpace

-- | Accepts a single newline. Generates a newline.
endOfLine :: SyntaxChar syn => syn () ()
endOfLine = char '\n'
