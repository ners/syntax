{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Syntax.Bytes.LittleEndian where

import Control.Category (Category)
import Control.Category.Reader
import Control.Category.Structures
import Control.Lens (Iso', from, iso, view, (^.))
import Control.Lens.SemiIso
import Control.SIArrow
import Data.Bits
import Data.Char
import qualified Data.Int as Data
import Data.Kind (Type)
import qualified Data.List.Extra as List
import Data.MonoTraversable
import Data.Scientific (Scientific)
import Data.Sequences (IsSequence (..), SemiSequence (..))
import qualified Data.Sequences as Sequence
import Data.Syntax
import qualified Data.Syntax as Syntax
import Data.Syntax.Bytes.Util
import Data.Syntax.Combinator
import Data.Text (Text)
import qualified Data.Word as Data

class Bytes a where
    byteSize :: Int
    default byteSize :: (FiniteBits a) => Int
    byteSize = finiteBitSize (zeroBits :: a) `div` 8

    getByte :: Int -> a -> Data.Word8
    default getByte :: (FiniteBits a) => Int -> a -> Data.Word8
    getByte i a = coerceBits (a .>>. (i * 8))

    toBytes :: a -> [Data.Word8]
    default toBytes :: (FiniteBits a) => a -> [Data.Word8]
    toBytes a = flip getByte a <$> [0 .. byteSize @a - 1]

    fromBytes :: [Data.Word8] -> a
    default fromBytes :: (FiniteBits a) => [Data.Word8] -> a
    fromBytes = foldr (.|.) zeroBits . zipWith (\i b -> coerceBits b .<<. (i * 8)) [0 ..]

bytes :: (Bytes a) => Iso' a [Data.Word8]
bytes = iso toBytes fromBytes

instance Bytes Data.Word8
instance Bytes Data.Word16
instance Bytes Data.Word32
instance Bytes Data.Word64
instance Bytes Data.Int8
instance Bytes Data.Int16
instance Bytes Data.Int32
instance Bytes Data.Int64

newtype ByteWise (a :: Type) (syn :: Type -> Type -> Type) (x :: Type) (y :: Type) = ByteWise (syn x y)

deriving instance (Category syn) => Category (ByteWise a syn)
deriving instance (CatPlus syn) => CatPlus (ByteWise a syn)
deriving instance (Coproducts syn) => Coproducts (ByteWise a syn)
deriving instance (Products syn) => Products (ByteWise a syn)
deriving instance (SIArrow syn) => SIArrow (ByteWise a syn)

newtype BytesSeq a seq = BytesSeq seq
    deriving newtype (Eq)

type instance Element (BytesSeq a seq) = a

stupidToList :: forall a seq. (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => BytesSeq a seq -> [a]
stupidToList = fmap (view $ from bytes) . List.chunksOf (byteSize @a) . otoList . (\(BytesSeq s) -> s)

stupidFromList :: (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => [a] -> BytesSeq a seq
stupidFromList = BytesSeq . fromList . concatMap (view bytes)

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => MonoFoldable (BytesSeq a seq) where
    ofoldMap f = ofoldMap f . stupidToList
    ofoldr f i = ofoldr f i . stupidToList
    ofoldl' f i = ofoldl' f i . stupidToList
    ofoldr1Ex f = ofoldr1Ex f . stupidToList
    ofoldl1Ex' f = ofoldl1Ex' f . stupidToList

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => MonoFunctor (BytesSeq a seq) where
    omap f = stupidFromList . omap f . stupidToList

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => MonoTraversable (BytesSeq a seq) where
    otraverse f = fmap stupidFromList . otraverse f . stupidToList
    omapM f = fmap stupidFromList . omapM f . stupidToList

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => GrowingAppend (BytesSeq a seq)

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => SemiSequence (BytesSeq a seq) where
    type Index (BytesSeq a seq) = Index seq
    intersperse x = stupidFromList . intersperse x . stupidToList
    reverse = stupidFromList . Sequence.reverse . stupidToList
    find f = find f . stupidToList
    sortBy f = stupidFromList . sortBy f . stupidToList
    cons x = stupidFromList . cons x . stupidToList
    snoc xs x = stupidFromList . flip snoc x . stupidToList $ xs

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => MonoPointed (BytesSeq a seq) where
    opoint = stupidFromList . List.singleton

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => Semigroup (BytesSeq a seq) where
    a <> b = stupidFromList $ stupidToList a <> stupidToList b

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => Monoid (BytesSeq a seq) where
    mempty = stupidFromList mempty

instance (IsSequence seq, Element seq ~ Data.Word8, Bytes a) => IsSequence (BytesSeq a seq)

instance (Syntax syn, Element (Seq syn) ~ Data.Word8, Eq a, Bytes a) => Syntax (ByteWise a syn) where
    type Seq (ByteWise a syn) = BytesSeq a (Seq syn)
    anyChar = ByteWise $ (bytes . rev packed) /$/ Syntax.take (byteSize @a)
