{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module Codec.Binary.QRCode.Placement where

import Codec.Binary.QRCode.Matrix
import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.Utils

import Data.Tuple
import Data.List
import qualified Data.Set as S

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Array.IArray

type Coords = [(Int,Int)]
type ReaderCoords = ReaderQR Coords
type MatrixPart = [((Int,Int),Module)]

intertwine :: [a] -> [a] -> [a]
intertwine (x:xs) (y:ys) = x:y:intertwine xs ys
intertwine (x:xs) [] = x:intertwine xs []
intertwine [] (y:ys) = y:intertwine [] ys
intertwine [] [] = []

-- mkCleanBoardMatrix :: Int -> Matrix
-- mkCleanBoardMatrix width = QRM $ array ((0,0), (width-1,width-1)) combined
--     where
--         finderTL = genFinderPatternTopLeft

mkMatrix :: Int -> [((Int, Int), Module)] -> Matrix
mkMatrix width = QRM . array ((0,0),(width-1,width-1))

mkSymbolWithFunctionPatterns :: Version -> Modules -> Matrix
mkSymbolWithFunctionPatterns ver mods = mkMatrix width combinedMods
    where
        width = qrNumModulesPerSide $ qrGetInfo ver
        combinedMods = flip runReader ver $ do
            let genAllLight = liftM $ flip zip $ repeat Light

            formats <- formatInfoRegions
            versions <- versionInfoRegions
            finderTL <- genAllLight finderPatternTopLeft
            finderBL <- genAllLight finderPatternBottomLeft
            finderTR <- genAllLight finderPatternTopRight
            timingH <- genAllLight timingPatternHorizontal
            timingV <- genAllLight timingPatternVertical
            hardcoded <- hardcodedDarkModule

            path <- mkPath

            let dat = zip path mods
                finders = finderTL ++ finderBL ++ finderTR
                timings = timingH ++ timingV
                hc = zip hardcoded (repeat Light)

                -- These will be applied to the symbol after masking
                formats' = zip formats (repeat Light)
                versions' = zip versions (repeat Light)

            return $ dat ++ finders ++ timings ++ hc ++ formats' ++ versions'

mkSymbol :: Coords -> Version -> Modules -> Matrix
mkSymbol path ver mods = mkMatrix width combinedMods
    where
        width = qrNumModulesPerSide $ qrGetInfo ver
        combinedMods = flip runReader ver $ do
            formats <- formatInfoRegions
            versions <- versionInfoRegions
            finderTL <- genFinderPatternTopLeft finderPatternTopLeft
            finderBL <- genFinderPatternBottomLeft finderPatternBottomLeft
            finderTR <- genFinderPatternTopRight finderPatternTopRight
            timingH <- genTimingPattern timingPatternHorizontal
            timingV <- genTimingPattern timingPatternVertical
            alignments <- liftM genAlignmentPatterns alignmentCoords
            hardcoded <- hardcodedDarkModule
            let dat = zip path mods
                finders = finderTL ++ finderBL ++ finderTR
                timings = timingH ++ timingV
                hc = zip hardcoded [Dark]
                formats' = zip formats $ repeat Light
                versions' = zip versions $ repeat Light
            return $ dat ++ finders ++ timings ++ hc ++ formats' ++ alignments ++ versions'

qrmApplyInfo :: ReaderQR Coords ->  ReaderQR Coords  -> Version -> Matrix-> BitStream -> Matrix
qrmApplyInfo region1 region2 ver mat bitstream = qrmOverlay mat overlays
    where
        mods = toModules bitstream
        overlays = flip runReader ver $ do
            path1 <- region1 
            path2 <- region2
            return $ zip path1 mods ++ zip path2 mods

qrmApplyFormatInfo :: Version -> Matrix -> BitStream -> Matrix
qrmApplyFormatInfo = qrmApplyInfo formatInfoRegionHorizontal formatInfoRegionVertical

qrmApplyVersionInfo :: Version -> Matrix -> BitStream -> Matrix
qrmApplyVersionInfo = qrmApplyInfo versionInfoRegionBottomLeft versionInfoRegionTopRight

newtype MyChar = MC Char deriving (Eq, Ord, Enum)

instance Show MyChar where
    show (MC '\n') = "\n"
    show (MC c) = [c]

mkDebugPath :: Version -> Array (Int,Int) MyChar
mkDebugPath ver = base // trail
    where
        width = qrNumModulesPerSide $ qrGetInfo ver
        ix = ((0,0),(width-1,width-1))
        blanks = repeat (MC ' ')
        trail = zip (runReader mkPath ver) $ cycle $ reverse [(MC '0')..(MC '7')]
        base = listArray ix blanks

-- fred = putStrLn $ show2DArray $ mkDebugPath 21
-- bar = runReader mkRawPath 6 \\ mask
--     where
--         mask = [(x,2) | x <- [0..5]]

-- Create the coordinates, in order, where modules should 
-- be placed in a matrix. The path excludes function 
-- patterns so a bitstream can be zipped one to one to the 
-- coordinates into a matrix. The path flows from the most 
-- significant bit to the least
mkPath :: ReaderQR Coords
mkPath = mkRawPath `subtractPatterns` allFunctionPatterns

-- Return a "raw" path in coordinates. This is the path that 
-- bits will follow according to the placement strategy in the spec.
--
-- The path is created by intervolving up-row pairs and down-row pairs
--
-- This creates a raw path. The real path can be obtained by simply 
-- subtracting functional patterns' coordinates. Note that 
-- the vertical timing pattern presents a special case because it will 
-- reverse the orientation. This is kind of difficult to explain in 
-- words but is apparent when you draw out the path on paper.
mkRawPath :: ReaderQR Coords
mkRawPath = do
    ver <- ask
    time <- natural 7

    let width = qrGetWidth ver
        upRowPair = concatMap (replicate 2) [0..(width-1)]
        downRowPair = reverse upRowPair

        mkCols = concat . concatMap (replicate width) . chunksOf 2
        mkRows = concat . cycle

        -- rows and cols "before" (to the right of) the vert timing pattern
        cols1 = mkCols [0..(time-1)]
        rows1 = mkRows [upRowPair, downRowPair]

        -- "after"
        cols2 = mkCols [(time+1)..(width-1)]
        rows2 = mkRows [downRowPair, upRowPair]

    return $ filter ((/=time) . fst) $ zip rows1 cols1 ++ zip rows2 cols2

genTimingPattern :: Monad m => m [a] -> m [(a, Module)]
genTimingPattern path = do
    p <- path
    return $ zip p (cycle [Dark, Light])

joinPatterns :: Applicative f => f [a] -> f [a] -> f [a]
joinPatterns = (<*>) . ((++) <$>)

fastDiff :: (Ord a) => [a] -> [a] -> [a]
xs `fastDiff` ys = filter (flip S.notMember ys') xs
    where ys' = S.fromList ys

subtractPatterns :: (Applicative f, Ord a) => f [a] -> f [a] -> f [a]
subtractPatterns = (<*>) . (fastDiff <$>)

(/+/) :: Applicative f => f [a] -> f [a] -> f [a]
(/+/) = joinPatterns

allFunctionPatterns :: ReaderQR Coords
allFunctionPatterns = timingPatterns /+/ finderPatterns
                    /+/ formatInfoRegions
                    /+/ hardcodedDarkModule
                    /+/ alignmentCoords
                    /+/ versionInfoRegions

-- Convert a top-left-origin position to a bottom-right-origin
-- This is here so that we can input numbers according to the spec
--
-- When the spec says the "6-th column", we can just say "natural 6" 
-- to get to the right position.
--
-- i.e. the top-left-origin here is (1,1)
natural :: Int -> ReaderQR Int
natural n = do
    ver <- ask
    return $ qrGetWidth ver - n

timingPatterns :: ReaderQR Coords
timingPatterns = timingPatternHorizontal /+/ timingPatternVertical

timingPatternHorizontal :: ReaderQR Coords
timingPatternHorizontal = do
    ver <- ask
    row <- natural 7
    let width = qrGetWidth ver
        v = [(row,y) | y <- [0..(width-1)]]
    finder <- finderPatterns
    return $ v \\ finder

timingPatternVertical :: ReaderQR Coords
timingPatternVertical = map swap `fmap` timingPatternHorizontal

versionInfoRegion' :: (Num a, Enum a) => (a -> Int -> b) -> ReaderQR [b]
versionInfoRegion' f = do
    ver@(Version v) <- ask
    a <- natural 6
    let width = qrGetWidth ver
        rows = cycle [8..10]
        cols = concatMap (replicate 3) [a..width-1]
    return $ do
        guard $ v >= 7
        zipWith f rows cols

versionInfoRegionBottomLeft :: (Num a, Enum a) => ReaderQR [(a, Int)]
versionInfoRegionBottomLeft = versionInfoRegion' (,)

versionInfoRegionTopRight :: (Num a, Enum a) => ReaderQR [(Int, a)]
versionInfoRegionTopRight = versionInfoRegion' $ flip (,)

versionInfoRegions :: ReaderQR [(Int, Int)]
versionInfoRegions = versionInfoRegionBottomLeft /+/ versionInfoRegionTopRight

-- Figure 19 in spec
hardcodedDarkModule :: Num t => ReaderQR [(t, Int)]
hardcodedDarkModule = do
    col <- natural 9
    return [(7,col)]

formatInfoRegions :: ReaderQR [(Int, Int)]
formatInfoRegions = formatInfoRegionHorizontal /+/ formatInfoRegionVertical

formatInfoRegionHorizontal :: ReaderQR [(Int, Int)]
formatInfoRegionHorizontal = do
    ver <- ask
    row <- natural 9
    c' <- natural 8
    let width = qrGetWidth ver
    return $ reverse [(row,col) | col <- [0..7] ++ [c'] ++ [c'+2..width-1]]

formatInfoRegionVertical :: ReaderQR [(Int, Int)]
formatInfoRegionVertical = do
    col <- natural 9

    let a = 0
        b = 6
    c <- natural 9
    d <- natural 8
    e <- natural 6
    f <- natural 1

    return [(row,col) | row <- [a..b] ++ [c..d] ++ [e..f]]

finderPatterns :: ReaderQR [(Int, Int)]
finderPatterns = finderPatternTopLeft /+/ finderPatternBottomLeft /+/ finderPatternTopRight

-- Includes separators
finderPatternTopLeft :: ReaderQR Coords
finderPatternTopLeft = do
    ver <- ask
    r' <- natural 8
    let width = qrGetWidth ver
    return [(row,col) | let vals = [r'..(width-1)], row <- vals, col <- vals]

finderPatternTopRight :: ReaderQR Coords
finderPatternTopRight = do
    ver <- ask
    r' <- natural 8
    let width = qrGetWidth ver
    return [(row,col) | row <- [r'..width-1], col <- [0..7]]

finderPatternBottomLeft :: ReaderQR Coords
finderPatternBottomLeft = do
    ver <- ask
    let width = qrGetWidth ver
    c' <- natural 8
    return [(row,col) | row <- [0..7], col <- [c'..width-1]]

-- This assumes a bottom-left origin, right to 
-- left bottom to top path
genFinderPattern :: Monad m => Modules -> Modules -> Modules -> Modules -> m [a] -> m [(a, Module)]
genFinderPattern prepend append lpadCol rpadCol path = do
    p <- path
    -- Add the separator to the raw finder pattern so 
    -- that it matches exactly with the path given
    let pat = prepend ++ rawFinderPattern ++ append
    return $ zip p pat
    where
        rawFinderPattern = r1 ++ r2 ++ r3 ++ r3 ++ r3 ++ r2 ++ r1
        r1 = rpadCol ++ replicate 7 Dark ++ lpadCol
        r2 = rpadCol ++ [Dark, Light, Light, Light, Light, Light, Dark] ++ lpadCol
        r3 = rpadCol ++ [Dark, Light, Dark, Dark, Dark, Light, Dark] ++ lpadCol

emptyFinderPatternRow :: Modules
emptyFinderPatternRow = replicate 8 Light

genFinderPatternTopLeft ::  Monad m => m [a] -> m [(a, Module)]
genFinderPatternTopLeft = genFinderPattern emptyFinderPatternRow [] [] [Light]

genFinderPatternTopRight ::  Monad m => m [a] -> m [(a, Module)]
genFinderPatternTopRight = genFinderPattern emptyFinderPatternRow [] [Light] []

genFinderPatternBottomLeft ::  Monad m => m [a] -> m [(a, Module)]
genFinderPatternBottomLeft = genFinderPattern [] emptyFinderPatternRow [] [Light]

genAlignmentPatterns :: [a] -> [(a, Module)]
genAlignmentPatterns = flip zip (cycle patternMods)
    where
        patternMods = [ Dark, Dark, Dark, Dark, Dark
                      , Dark, Light, Light, Light, Dark
                      , Dark, Light, Dark, Light, Dark
                      , Dark, Light, Light, Light, Dark
                      , Dark, Dark, Dark, Dark, Dark
                      ]

overlapsFinder :: (Int,Int) -> ReaderQR Bool
overlapsFinder (r,c) = do
    a <- natural 8
    return $ (r >= a && c >= a) || (r <= 8 && c >= a) || (r >= a && c <= 8)

alignmentCoords :: MonadReader Version m => m [(Int, Int)]
alignmentCoords = do
    ver <- ask
    let centers = qrAlignmentCenters ver
        validCenters = [(x,y) | x <- centers, y <- centers, let inFinder = runReader (overlapsFinder (x,y)) ver, not inFinder]

        mkPat (r,c) = [(r',c') | c' <- [c-2..c+2], r' <- [r-2..r+2]]

        pats = concatMap mkPat validCenters
    return pats
