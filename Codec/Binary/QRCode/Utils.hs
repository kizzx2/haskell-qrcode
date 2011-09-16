{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Utils where

import Numeric
import Data.Char

import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.GaloisField

import Control.Monad.Reader

type Input = String
type BitStream = String

type ReaderQR = Reader Version

-- | The smallest unit in a QR Code symbol (i.e. one "square").
data Module = Dark | Light deriving (Eq)

type Modules = [Module]

instance Bounded Module where
    maxBound = Light
    minBound = Dark

instance Show Module where
    show Dark = "*"
    show Light = " "

qrXor :: Module -> Module -> Module
qrXor Dark Dark = Light
qrXor Light Light = Light
qrXor Dark Light = Dark
qrXor Light Dark = Dark

toModules :: BitStream -> Modules
toModules = map conv
    where conv '1' = Dark
          conv '0' = Light
          conv x = error $ "Invalid BitStream element " ++ show x

showBinPadded :: Int -> Int -> String
showBinPadded len n = replicate (len - length str) '0' ++ str
    where
        str = showIntAtBase 2 intToDigit n ""

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = go xs (length xs) []
    where go xs' remLen acc | remLen <= n = reverse (xs' : acc)
                            | otherwise = go (drop n xs') (remLen - n) (take n xs' : acc)

toPoly :: BitStream -> GFPolynomial
toPoly = mkPolynomial . map digitToInt
