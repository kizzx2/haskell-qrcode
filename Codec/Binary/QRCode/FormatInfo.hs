{-# OPTIONS_HADDOCK hide #-}

module Codec.Binary.QRCode.FormatInfo where

import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.Utils

import Codec.Binary.QRCode.GaloisField

import Data.Bits

encode :: ErrorLevel -> BitStream -> BitStream
encode ec maskPatRef = showBinPadded 15 $ encoded `xor` qrFormatInfoMask
    where
        ecIndicator = qrErrorLevelIndicators ec
        input = ecIndicator ++ maskPatRef
        inputPoly = gfpRightPad 10 $ toPoly input
        encoded = gfpToBinaryRepr $ gfpAdd inputPoly (snd $ gfpQuotRem inputPoly qrFormatInfoGenPoly)
