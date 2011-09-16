{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Modes.Numeric where

import Numeric
import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec

safeRead :: Num a => String -> Maybe a
safeRead x = case readDec x of
    [] -> Nothing
    ((x',_):_) -> Just x'

convert :: Input -> Maybe String
convert chunk = showBin `fmap` safeRead chunk
    where
        showBin = case length chunk of
            3 -> showBinPadded 10
            2 -> showBinPadded 7
            1 -> showBinPadded 4
            _ -> error "Invalid chunk size"

encode :: Version -> Input -> Maybe BitStream
encode ver input = ((modeIndicatorBits ++ characterCountBits) ++) `fmap` dataBits
    where
        dataBits = concat `fmap` sequence (map convert . chunksOf 3 $ input)
        modeIndicatorBits = "0001"
        characterCountBits = showBinPadded cciLength $ length input
        cciLength = qrLengthOfCCI ver Numeric
