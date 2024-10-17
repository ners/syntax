{-# LANGUAGE DeriveFunctor #-}
{- |
Module      :  Data.Syntax.Printer.Consumer
Description :  Common base for both Text and ByteString printers.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental
-}
module Data.Syntax.Printer.Consumer where

import Control.Applicative
import Control.Monad
import Data.Bifunctor.Apply
import Data.Monoid

-- | A writer monad combined with Either String.
newtype Consumer m a = Consumer { runConsumer :: Either String (m, a) }
    deriving (Functor)

instance Monoid m => Applicative (Consumer m) where
    pure x = Consumer $ Right (mempty, x)
    f <*> x = Consumer $ bilift2 (flip (<>)) ($) <$> runConsumer f <*> runConsumer x

instance Monoid m => Alternative (Consumer m) where
    empty = Consumer $ Left "empty"
    f <|> g = Consumer $ case runConsumer f of
                           Left _ -> case runConsumer g of
                                       Left e2 -> Left e2
                                       Right x -> Right x
                           Right x -> Right x

instance Monoid m => Monad (Consumer m) where
    return = pure
    m >>= f = Consumer $ do
        (m1, x) <- runConsumer m
        (m2, y) <- runConsumer (f x)
        return (m2 <> m1, y)

instance Monoid m => MonadFail (Consumer m) where
    fail = Consumer . Left

instance Monoid m => MonadPlus (Consumer m) where
    mzero = empty
    mplus = (<|>)
