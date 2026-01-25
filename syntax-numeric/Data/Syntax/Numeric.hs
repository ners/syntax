module Data.Syntax.Numeric where

import Data.Syntax.Char
import Data.Scientific (Scientific)
import Control.Category.Structures
import Control.Lens.SemiIso
import Control.SIArrow
import Data.Bits
import Data.Char
import Data.Syntax
import Data.Syntax.Combinator
import Data.Syntax.Indent (Indent(..))
import Control.Category.Reader (ReaderCT)

class SyntaxChar syn => SyntaxNum syn where
    -- | An unsigned decimal number.
    decimal :: Integral a => syn () a

    -- | An unsigned hexadecimal number.
    hexadecimal :: (Integral a, Bits a) => syn () a

    -- | A signed real number.
    realFloat :: RealFloat a => syn () a

    -- | A scientific number.
    scientific :: syn () Scientific

    {-# MINIMAL decimal, hexadecimal, realFloat, scientific #-}

instance SyntaxNum syn => SyntaxNum (ReaderCT env syn) where
    decimal = clift decimal
    hexadecimal = clift hexadecimal
    scientific = clift scientific
    realFloat = clift realFloat

instance SyntaxNum syn => SyntaxNum (Indent syn) where
    decimal = Indent decimal
    hexadecimal = Indent hexadecimal
    scientific = Indent scientific
    realFloat = Indent realFloat

-- | A number with an optional leading '+' or '-' sign character.
signed :: (Real a, SyntaxChar syn) => syn () a -> syn () a
signed n =  _Negative /$/ char '-' */ n
        /+/ opt_ (char '+') */ n

-- | A decimal digit.
digitDec :: (SyntaxChar syn, Integral a) => syn () a
digitDec = semiIso toChar toInt /$/ anyChar
  where toInt c | isDigit c = Right (fromIntegral $ digitToInt c)
                | otherwise = Left ("Expected a decimal digit, got " ++ [c])
        toChar i | i >= 0 && i <= 9 = Right (intToDigit $ fromIntegral i)
                 | otherwise        = Left ("Expected a decimal digit, got number " ++ show (toInteger i))

-- | An octal digit.
digitOct :: (SyntaxChar syn, Integral a) => syn () a
digitOct = semiIso toChar toInt /$/ anyChar
  where toInt c | isOctDigit c = Right (fromIntegral $ digitToInt c)
                | otherwise    = Left ("Expected an octal digit, got " ++ [c])
        toChar i | i >= 0 && i <= 7 = Right (intToDigit $ fromIntegral i)
                 | otherwise        = Left ("Expected an octal digit, got number " ++ show (toInteger i))

-- | A hex digit.
digitHex :: (SyntaxChar syn, Integral a) => syn () a
digitHex = semiIso toChar toInt /$/ anyChar
  where toInt c | isHexDigit c = Right (fromIntegral $ digitToInt c)
                | otherwise    = Left ("Expected a hex digit, got " ++ [c])
        toChar i | i >= 0 && i <= 15 = Right (intToDigit $ fromIntegral i)
                 | otherwise         = Left ("Expected a hex digit, got number " ++ show (toInteger i))
