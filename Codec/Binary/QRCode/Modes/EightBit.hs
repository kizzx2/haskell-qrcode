{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Modes.EightBit where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec

import Data.Char

convert :: Char -> Maybe Int
convert c 
    | isLatin1 c = Just $ ord c
    | otherwise  = Nothing

toCharValues :: Input -> Maybe [Int]
toCharValues = mapM convert

encode :: Version -> Input -> Maybe BitStream
encode ver input = ((modeIndicatorBits ++ characterCountBits) ++) `fmap` dataBits
    where
        charValues = toCharValues input
        dataBits = concatMap (showBinPadded 8) `fmap` charValues

        modeIndicatorBits = "0100"
        characterCountBits = showBinPadded cciLength $ length input
        cciLength = qrLengthOfCCI ver EightBit
