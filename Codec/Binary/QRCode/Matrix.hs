{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Matrix where

import Data.Array.IArray
import Codec.Binary.QRCode.Utils

-- | Represents a QR Code symbol.
newtype Matrix = QRM { getModules :: Array (Int,Int) Module }

instance Show Matrix where
    show = showMatrix

getWidth :: Array (Int, Int) Module -> Int
getWidth = fst . snd . bounds

-- | Convert a 'Matrix' to an array of 'Bounded'
-- 'Light' modules will have the value 'maxBound';
-- 'Dark' modules will have the value 'minBound'
toArray :: Bounded a => Matrix -> Array (Int, Int) a
toArray (QRM m) = amap conv . ixmap bs inv $ m
    where
        conv Dark = minBound
        conv Light = maxBound

        bs = bounds m
        width = getWidth m
        inv (r,c) = (width - r, width - c)

qrmCol :: Int -> Matrix -> Modules
qrmCol n (QRM mat) = [mat ! (r,n) | r <- [0..(getWidth mat)]]

qrmRow :: Int -> Matrix -> Modules
qrmRow n (QRM mat) = [mat ! (n,c) | c <- [0..(getWidth mat)]]

qrmWidth :: Matrix -> Int
qrmWidth = getWidth . getModules

qrmOverlay :: Matrix -> [((Int, Int), Module)] -> Matrix
qrmOverlay (QRM mat) associations = QRM $ mat // associations

show2DArray :: (Enum i, Num i, Ix i, Show e) => Array (i,i) e -> String
show2DArray mods = rows
    where
        bound = fst . snd . bounds $ mods
        showFs = amap shows mods
        row r = foldr (.) ("\n"++) [showFs ! (r,col) | col <- reverse [0..bound]]
        rows = foldr (.) id [row r | r <- reverse [0..bound]] $ ""

-- Show Matrix "top-down"
-- i.e. (0,0) is displayed in bottom right
showMatrix :: Matrix -> String
showMatrix = show2DArray . getModules
