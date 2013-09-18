{-# OPTIONS_HADDOCK hide #-}

module Codec.Binary.QRCode.Blocks where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.GaloisField

import Control.Monad

type Codewords = [BitStream]

mkPolyForEncode :: Version -> ErrorLevel -> BitStream -> GFPolynomial
mkPolyForEncode (Version v) errLevel bitstream = gfpRightPad numErrorWords $ mkPolynomial $ map readBin $ chunksOf 8 bitstream
    where numErrorWords = qrNumErrorCodewordsPerBlock v errLevel

interleave :: Version -> ErrorLevel -> BitStream -> BitStream
interleave ver@(Version v) ecl rawCoded = result'
    where
        blocks :: [[BitStream]]
        blocks = chunks (chunksOf 8 $ mkDataCodewords ver ecl rawCoded) (qrDCWSizes v ecl)

        codewordPairs = map (genCodewords ver ecl . concat) blocks

        dataCodewords :: [[BitStream]]
        dataCodewords = map fst codewordPairs

        ecCodewords :: [[BitStream]]
        ecCodewords = map snd codewordPairs

        padRemainderBits i' = i' ++ take (qrRemainderBits info) "0000000"

        info = qrGetInfo ver
        result = concat $ concat (transpose dataCodewords) ++ concat (transpose ecCodewords)
        result' = padRemainderBits result

pad :: MonadPlus m => [[m a]] -> [[m a]]
pad xs = map go xs
    where
        go l = take len $ l ++ repeat mzero
        len = maximum . map length $ xs

transpose :: [[a]] -> [[a]]
transpose xs = foldl1 (zipWith mplus) xs'
    where xs' = pad $ map (map (:[])) xs

chunks :: [a] -> [Int] -> [[a]]
chunks = go []
    where
        go acc xs (n:ns) = go (take n xs : acc) (drop n xs) ns
        go acc _ [] = reverse acc

toCodewords :: BitStream -> Codewords
toCodewords = chunksOf 8

genCodewords :: Version -> ErrorLevel -> BitStream -> (Codewords, Codewords)
genCodewords ver@(Version v) ecl input = (toCodewords dataCodewords, toCodewords errorCodewords)
    where
        dataCodewords = input

        numErrorWords = qrNumErrorCodewordsPerBlock v ecl
        genPoly = mkPolynomial $ qrGenPoly numErrorWords

        poly = toECPoly ver ecl dataCodewords
        errorCodewords = gfpShowBin $ snd $ gfpQuotRem poly genPoly

mkDataCodewords :: Version -> ErrorLevel -> BitStream -> BitStream
mkDataCodewords (Version v) errLevel = fillPadCodewords . padBits . terminate
    where
        numDataBits = qrNumDataBits v errLevel
        terminate i' = i' ++ take (numDataBits - length i') "0000"
        padBits i' = i' ++ take padLength "0000000"
            where padLength = 8 - (length i' `rem` 8)
        fillPadCodewords i' = take numDataBits (i' ++ cycle "1110110000010001")

toECPoly :: Version -> ErrorLevel -> BitStream -> GFPolynomial
toECPoly (Version v) errLevel bitstream = gfpRightPad numErrorWords $ mkPolynomial $ map readBin $ chunksOf 8 bitstream
    where numErrorWords = qrNumErrorCodewordsPerBlock v errLevel
