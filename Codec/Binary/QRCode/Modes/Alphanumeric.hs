{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Modes.Alphanumeric where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec

import Data.Char

chars :: [Char]
chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-.:"

table :: [(Char, Int)]
table = zip chars [0..41]

convert :: Char -> Maybe Int
convert = flip lookup table

toCharValues :: Input -> Maybe [Int]
toCharValues = sequence . map convert

encode :: Version -> Input -> Maybe BitStream
encode ver input = ((modeIndicatorBits ++ characterCountBits) ++) `fmap` dataBits
    where
        input' = map toUpper input
        charValues = toCharValues input'
        dataBits = (concatMap convertGroup . chunksOf 2) `fmap` charValues

        convertGroup [x] = showBinPadded 6 x
        convertGroup [x,y] = showBinPadded 11 $ x * 45 + y
        convertGroup _ = error "Impossible chunk size"

        modeIndicatorBits = "0010"
        characterCountBits = showBinPadded cciLength $ length input
        cciLength = qrLengthOfCCI ver Alphanumeric
