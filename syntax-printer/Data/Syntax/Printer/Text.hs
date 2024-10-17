{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Printer.Text
Description :  Prints to a Text Builder using strict Text as the sequence.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental
-}
module Data.Syntax.Printer.Text (
    Printer,
    runPrinter,
    runPrinter_
    )
    where

import           Control.Arrow (Kleisli(..))
import           Control.Category
import           Control.Category.Structures
import           Control.Monad
import           Control.SIArrow
import           Data.Monoid (mempty)
import           Data.Semigroupoid.Dual
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Syntax.Printer.Consumer
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Lazy.Builder.Scientific as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (id, (.))

-- | Prints a value to a Text Builder using a syntax description.
newtype Printer a b = Printer { unPrinter :: Dual (Kleisli (Consumer Builder)) a b }
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: (b -> Either String Builder) -> Printer () b
wrap f = Printer $ Dual $ Kleisli $ (Consumer . fmap (, ())) . f

unwrap :: Printer a b -> b -> Consumer Builder a
unwrap = runKleisli . getDual . unPrinter

instance Syntax Printer where
    type Seq Printer = Text
    anyChar = wrap $ Right . singleton
    take n = wrap $ Right . fromText . T.take n
    takeWhile p = wrap $ Right . fromText . T.takeWhile p
    takeWhile1 p = wrap $ Right . fromText <=< notNull . T.takeWhile p
      where notNull t | T.null t  = Left "takeWhile1 failed"
                      | otherwise = Right t
    takeTill1 p = wrap $ Right . fromText <=< notNull . T.takeWhile (not . p)
      where notNull t | T.null t  = Left "takeTill1 failed"
                      | otherwise = Right t
    vecN n e = wrap $ \v -> if V.length v == n
                               then fmap fst $ runConsumer (V.mapM_ (unwrap e) v)
                               else Left "vecN: invalid vector size"
    ivecN n e = wrap $ \v -> if V.length v == n
                                then fmap fst $ runConsumer (V.mapM_ (unwrap e) (V.indexed v))
                                else Left "ivecN: invalid vector size"
    uvecN n e = wrap $ \v -> if VU.length v == n
                                then fmap fst $ runConsumer (VU.mapM_ (unwrap e) v)
                                else Left "uvecN: invalid vector size"
    uivecN n e = wrap $ \v -> if VU.length v == n
                                 then fmap fst $ runConsumer (VU.mapM_ (unwrap e) (VU.indexed v))
                                 else Left "uivecN: invalid vector size"

instance Isolable Printer where
    isolate p = Printer $ Dual $ Kleisli $
        Consumer . fmap ((mempty, ) . toStrict . toLazyText) . runPrinter_ p

instance SyntaxChar Printer where
    decimal = wrap $ Right . B.decimal
    hexadecimal = wrap $ Right . B.hexadecimal
    realFloat = wrap $ Right . B.realFloat
    scientific = wrap $ Right . B.scientificBuilder

-- | Runs the printer.
runPrinter :: Printer a b -> b -> Either String (Builder, a)
runPrinter = (runConsumer .) . runKleisli . getDual . unPrinter

-- | Runs the printer and discards the result.
runPrinter_ :: Printer a b -> b -> Either String Builder
runPrinter_ = (fmap fst .) . runPrinter
