{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.GaloisField where

import Data.Bits

import Numeric
import Data.Char

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU

qrGFSize :: Int
qrGFSize = 256 -- QR code symbols are GF(2^8)

qrFieldPolynomial :: Int
qrFieldPolynomial = 285 -- 100011101, from the standard

readBin :: String -> Int
readBin s = go 0 0 (reverse s)
    where
        go :: Int -> Int -> String -> Int
        go acc _ "" = acc
        go acc pow ('0':xs) = go acc (pow+1) xs
        go acc pow ('1':xs) = go (acc + 2 ^ pow) (pow+1) xs
        go _ _ xs = error $ "Malformed binary string '" ++ xs ++ "'"

newtype GFElement = GFElement { gfGet :: Int } deriving (Eq)
newtype GFPolynomial = GFPolynomial { gfpGetTerms :: [GFElement] } deriving (Show, Eq)
type GFPTerm = (GFElement, Int) -- (coefficient, order)

instance Show GFElement where
    show (GFElement x) = "GFE " ++ show x

gfAdd :: GFElement -> GFElement -> GFElement
gfAdd (GFElement x) (GFElement y) = GFElement (x `xor` y)

gfMinus :: GFElement -> GFElement -> GFElement
gfMinus = gfAdd

gfALogs :: [Int]
gfALogs = 1:(map f gfALogs)
    where
        f x = let x' = x * 2 in
            if x' >= qrGFSize
                then x' `xor` qrFieldPolynomial
                else x'

gfALogsVec :: VU.Vector Int
gfALogsVec = VU.fromList . take 255 $ gfALogs

gfALog :: Int -> Int
gfALog = (gfALogsVec VU.!) . (`mod` 255)

gfLogs :: M.Map Int Int
gfLogs = M.fromList $ take qrGFSize $ zip gfALogs [0..]

gfLog :: Int -> Int
gfLog = (M.!) gfLogs

gfMult :: GFElement -> GFElement -> GFElement
gfMult z@(GFElement 0) _ = z
gfMult _ z@(GFElement 0) = z
gfMult x (GFElement 1) = x
gfMult (GFElement 1) x = x
gfMult (GFElement x) (GFElement y) =
    GFElement $ gfALog(gfLog x + gfLog y) `mod` qrGFSize

gfQuot :: GFElement -> GFElement -> GFElement
gfQuot x (GFElement 1) = x
gfQuot _ (GFElement 0) = error "div by zero"
gfQuot (GFElement x) (GFElement y) =
    GFElement $ gfALog(gfLog x - gfLog y + (qrGFSize-1)) `mod` (qrGFSize-1)

instance Num GFElement where
    (+) = gfAdd
    (-) = gfMinus
    (*) = gfMult
    negate = id
    abs = id
    signum = const $ GFElement 1
    fromInteger = GFElement . fromInteger

gfShowBin :: GFElement -> String
gfShowBin (GFElement n) = replicate padLength '0' ++ str
    where str = showIntAtBase 2 intToDigit n ""
          padLength = 8 - length str

gfQuotRem :: GFElement -> GFElement -> (GFElement, GFElement)
gfQuotRem x y = let q = gfQuot x y in (q, x - q)

gfpOrder :: GFPolynomial -> Int
gfpOrder (GFPolynomial terms) = length terms - 1

gfZeroes :: [GFElement]
gfZeroes = map GFElement (repeat 0);

gfpEnlarge :: Int -> GFPolynomial -> GFPolynomial
gfpEnlarge n p@(GFPolynomial terms)
    | order >= n = p
    | otherwise = GFPolynomial $ take (n-order) gfZeroes ++ terms
    where order = gfpOrder p

gfpShowBin :: GFPolynomial -> String
gfpShowBin (GFPolynomial xs) = concatMap gfShowBin xs

gfpHead :: GFPolynomial -> GFElement
gfpHead = head . gfpGetTerms

gfpZipWith :: (GFElement -> GFElement -> GFElement) -> GFPolynomial -> GFPolynomial -> GFPolynomial
gfpZipWith f a b = GFPolynomial $ dropWhile (== GFElement 0) $ zipWith f aTerms bTerms
    where
        maxOrder = max (gfpOrder a) (gfpOrder b)
        (GFPolynomial aTerms) = gfpEnlarge maxOrder a
        (GFPolynomial bTerms) = gfpEnlarge maxOrder b

gfpAdd :: GFPolynomial -> GFPolynomial -> GFPolynomial
gfpAdd = gfpZipWith (+)

gfpMinus :: GFPolynomial -> GFPolynomial -> GFPolynomial
gfpMinus = gfpZipWith (-)

gfpMultTerm :: GFPolynomial -> GFPTerm -> GFPolynomial
gfpMultTerm (GFPolynomial terms) (coefficient,order) =
    GFPolynomial $ map (*coefficient) (terms ++ take order gfZeroes)

gfpAddTerm :: GFPolynomial -> GFPTerm -> GFPolynomial
gfpAddTerm p (coefficient, order) = gfpZipWith (+) p additive
    where
        additive = GFPolynomial $ coefficient : take order gfZeroes

gfpQuotRem :: GFPolynomial -> GFPolynomial -> (GFPolynomial, GFPolynomial)
gfpQuotRem dividend divisor = go dividend (GFPolynomial [])
    where
        divHead = gfpHead divisor
        go currentDividend q
            | order < 0 = (q, currentDividend)
            | gfpOrder nextDividend == 0 = (q', (GFPolynomial []))
            | otherwise =
                go nextDividend (q |+| currentTerm)
            where
                nextDividend = currentDividend |-| currentQuotient
                q' = q |+| currentTerm

                coefficient = gfQuot (gfpHead currentDividend) divHead
                order = gfpOrder currentDividend - gfpOrder divisor
                currentTerm = (coefficient,order)
                currentQuotient = divisor |*| currentTerm
        (|-|) = gfpMinus
        (|*|) = gfpMultTerm
        (|+|) = gfpAddTerm

gfpRightPad :: Int -> GFPolynomial -> GFPolynomial
gfpRightPad n (GFPolynomial terms) = GFPolynomial $ terms ++ replicate n 0

mkPolynomial :: [Int] -> GFPolynomial
mkPolynomial = GFPolynomial . map GFElement

gfpToBinaryRepr :: GFPolynomial -> Int
gfpToBinaryRepr (GFPolynomial terms) = readBin bits
    where bits = concatMap (show . gfGet) terms
