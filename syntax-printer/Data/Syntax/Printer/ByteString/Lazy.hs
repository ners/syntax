{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Printer.ByteString.Lazy
Description :  Prints to a ByteString Builder using lazy ByteString as the sequence.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  ners <ners@gmx.ch>
Stability   :  experimental
-}
module Data.Syntax.Printer.ByteString.Lazy (
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
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Builder
import           Data.Monoid (mempty)
import           Data.Semigroupoid.Dual
import           Data.Syntax
import           Data.Syntax.Printer.Consumer
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (id, (.))

-- | Prints a value to a ByteString Builder using a syntax description.
newtype Printer a b = Printer { unPrinter :: Dual (Kleisli (Consumer Builder)) a b }
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: (b -> Either String Builder) -> Printer () b
wrap f = Printer $ Dual $ Kleisli $ (Consumer . fmap (, ())) . f

unwrap :: Printer a b -> b -> Consumer Builder a
unwrap = runKleisli . getDual . unPrinter

instance Syntax Printer where
    type Seq Printer = ByteString
    anyChar = wrap $ Right . word8
    take n = wrap $ Right . lazyByteString . BS.take (fromIntegral n)
    takeWhile p = wrap $ Right . lazyByteString . BS.takeWhile p
    takeWhile1 p = wrap $ Right . lazyByteString <=< notNull . BS.takeWhile p
      where notNull t | BS.null t  = Left "takeWhile1 failed"
                      | otherwise = Right t
    takeTill1 p = wrap $ Right . lazyByteString <=< notNull . BS.takeWhile (not . p)
      where notNull t | BS.null t  = Left "takeTill1 failed"
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
        Consumer . fmap ((mempty, ) . toLazyByteString) . runPrinter_ p

-- | Runs the printer.
runPrinter :: Printer a b -> b -> Either String (Builder, a)
runPrinter = (runConsumer .) . runKleisli . getDual . unPrinter

-- | Runs the printer and discards the result.
runPrinter_ :: Printer a b -> b -> Either String Builder
runPrinter_ = (fmap fst .) . runPrinter
