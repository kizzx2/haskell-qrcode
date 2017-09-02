{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Masks where

import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Placement
import Codec.Binary.QRCode.Matrix

import Control.Monad.Reader

import Data.Function

import Data.Array.IArray
import Data.List

type Block = [(Int,Int)]

mask000Cond, mask001Cond, mask010Cond, mask011Cond, mask100Cond, mask101Cond, mask110Cond, mask111Cond :: Integral a => a -> a -> Bool
mask000Cond i j = (i+j) `mod` 2 == 0
mask001Cond i _ = i `mod` 2 == 0
mask010Cond _ j = j `mod` 3 == 0
mask011Cond i j = (i+j) `mod` 3 == 0
mask100Cond i j = ((i`div`2) + (j`div`3)) `mod` 2 == 0
mask101Cond i j = (i*j) `mod` 2 + (i*j) `mod` 3 == 0
mask110Cond i j = ((i*j) `mod` 2 + (i*j) `mod` 3) `mod` 2 == 0
mask111Cond i j = ((i*j) `mod` 3 + (i+j) `mod` 2) `mod` 2 == 0

mkMask :: (Int -> Int -> Bool) -> [(Int, Int)] -> ReaderQR [Module]
mkMask cond coords = do
    ver <- ask
    let width = qrGetWidth ver
    -- FIXME we need to convert a bottom right coord to top left coord, ARGH!
    let unnatural n = width - 1 - n
        transform (i,j) = if (cond `on` unnatural) i j then Dark else Light
    return $ map transform coords

mask000, mask001, mask010, mask011, mask100, mask101, mask110, mask111 :: Coords -> ReaderQR Modules
mask000 = mkMask mask000Cond
mask001 = mkMask mask001Cond
mask010 = mkMask mask010Cond
mask011 = mkMask mask011Cond
mask100 = mkMask mask100Cond
mask101 = mkMask mask101Cond
mask110 = mkMask mask110Cond
mask111 = mkMask mask111Cond

mask :: Modules -> ReaderQR (Matrix, BitStream)
mask mods = do
    ver <- ask
    coords <- mkPath
    masks <- sequence $ [mask000, mask001, mask010, mask011, mask100, mask101, mask110, mask111] <*> pure coords
    let maskRefs = ["000", "001", "010", "011", "100", "101", "110", "111"]
        mkMaskedSym x y = mkSymbol coords ver $ applyMask x y
        syms = zipWith mkMaskedSym (repeat mods) masks
        symsWithRefsScores = zip (map score syms) $ zip syms maskRefs-- (score,(sym,ref))
        best = minimumBy (compare `on` fst) symsWithRefsScores
    return $ snd best

applyMask :: Modules -> Modules -> Modules
applyMask = zipWith qrXor

-- run length encode
rle :: (Eq a) => [a] -> [(a,Int)]
rle = foldl' go []
    where
        go [] x = [(x,1)]
        go acc@((y,n):ys) x = if x == y then (y,n+1):ys else (x,1):acc

score :: Matrix -> Int
score mat = sum . zipWith ($) funcs . repeat $ mat
    where
        funcs = [ scoreRule1 rows cols
                , all2x2Blocks
                , countFinderRatio rows cols
                , proportionOfDarkModules
                ]
        width = qrmWidth mat
        rows = [qrmRow n mat | n <- [0..width] ]
        cols = [qrmCol n mat | n <- [0..width] ]

-- Adjacent modules in row/column in same color
scoreRule1 :: Eq a => [[a]] -> [[a]] -> t -> Int
scoreRule1 rows cols _ = sumOver rows + sumOver cols
    where
        sumOver = sum . map countOne
        countOne = sum . map (subtract 2) . filter (>5) . map snd . rle -- -5 + 3 (N1) = -2

all2x2Blocks :: Matrix -> Int
all2x2Blocks mat = 3 * total
    where 
        total = sum . map (sum . map fromEnum) $ blockRows
        width = qrmWidth mat
        mods = getModules mat
        
        blockRows :: [[Bool]]
        blockRows = [zipWith go (statusRows !! n) (statusRows !! (n+1)) | n <- [0..width-1]]
            where
                go :: Int -> Int -> Bool
                go a b = a == b && a /= 0

        statusRows = [rowToStatuses n | n <- [0..width]]
        calcStatus (a,b) =
            if mods ! a == mods ! b 
                then if mods ! a == Light then bothLight else bothDark
                else different
        rowToStatuses n = map calcStatus $ pairsOfRow n
        pairsOfRow n = [((n,x),(n,x+1)) | x <- [0..width-1]]

        different = 0
        bothLight = -1
        bothDark = 1

countFinderRatio :: Num a => [[Module]] -> [[Module]] -> t -> a
countFinderRatio rows cols _ = (sum rowCounts + sum colCounts) * 40
    where
        rowCounts = map count rows
        colCounts = map count cols

        count = go 0

        go acc (Dark:Light:Dark:Dark:Dark:Light:Dark:xs) = go (acc+1) (Dark:xs)
        go acc (_:xs) = go acc xs
        go acc [] = acc

proportionOfDarkModules :: Matrix -> Int
proportionOfDarkModules mat = total
    where
        total = 10 * k
        k = truncate $ abs (0.5 - proportion) / 0.05

        proportion :: Double
        proportion = ((/) `on` fromIntegral) (numDarks mat) numModules

        numModules :: Int
        numModules = (qrmWidth mat + 1) ^ (2 :: Int)

        numDarks = sum . map darkToOne . elems . getModules

        darkToOne Dark = 1
        darkToOne _ = 0
