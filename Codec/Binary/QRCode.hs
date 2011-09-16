-- |
-- Module : Codec.Binary.QRCode
-- License : BSD3
-- 
-- Maintainer : chris@kizzx2.com
-- Stability : Experimental
-- Portability : Portable
--
-- An evolving QR Code encoder (and future decoder) in pure Haskell.
--
-- Currently supports encoding 'Numeric' and 'Alphanumeric' data.
--
-- Example
--
-- > encode (version 1) M Alphanumeric "hello world"

module Codec.Binary.QRCode
    (
    -- * Operations
      encode
    , toArray
    , width

    -- * Data Constructors
    , version

    -- * Data Types
    , Matrix
    , Module(..)
    , Mode(Numeric, Alphanumeric)
    , ErrorLevel(..)
    , Version
    ) where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec
import Codec.Binary.QRCode.Placement
import Codec.Binary.QRCode.Masks
import Codec.Binary.QRCode.Blocks
import Codec.Binary.QRCode.Matrix

import qualified Codec.Binary.QRCode.Modes.Numeric as N
import qualified Codec.Binary.QRCode.Modes.Alphanumeric as A
import qualified Codec.Binary.QRCode.FormatInfo as F
import qualified Codec.Binary.QRCode.VersionInfo as V

import Control.Monad.Reader

-- | Returns 'Nothing' if the input is invalid for the 'Mode' specified.
encode :: Version -> ErrorLevel -> Mode -> String -> Maybe Matrix
encode ver ecl mode input =
    inputEncode ver input >>= return . encode' ver ecl
    where
        inputEncode = case mode of
            Numeric -> N.encode
            Alphanumeric -> A.encode
            _ -> undefined

encode' :: Version -> ErrorLevel -> BitStream -> Matrix
encode' ver ecl encodedInput = final'
    where
        bitstream = interleave ver ecl encodedInput
        modules = toModules bitstream
        (matrix,maskRef) = runReader (mask modules) ver

        format = F.encode ecl maskRef
        ver' = V.encode ver

        final = qrmApplyFormatInfo ver matrix format
        final' = qrmApplyVersionInfo ver final ver'

-- | The number of modules per side.
width :: Matrix -> Int
width = qrmWidth

-- | Valid version number is /[1, 40]/
version :: Int -> Maybe Version
version n | n >= 1 && n <= 40 = Just (Version n)
          | otherwise = Nothing
