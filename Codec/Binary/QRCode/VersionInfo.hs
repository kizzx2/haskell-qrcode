{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.VersionInfo where

import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.Utils

import Codec.Binary.QRCode.GaloisField

encode :: Version -> BitStream
encode (Version v) = showBinPadded 18 encoded
    where
        inputPoly = gfpRightPad 12 $ toPoly (showBinPadded 6 v)
        encoded = gfpToBinaryRepr $ gfpAdd inputPoly (snd $ gfpQuotRem inputPoly qrVersionInfoGenPoly)
