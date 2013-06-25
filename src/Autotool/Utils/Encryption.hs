module Autotool.Utils.Encryption
  ( encrypt
  , decrypt
  ) where

import           Data.Char (chr, ord)
import           Numeric (readHex, showHex)


-----------------------------------------------------------------------------
-- | Very simple encryption.
encrypt :: String -> String
encrypt = concatMap (flip showHex "") . map ord


-----------------------------------------------------------------------------
-- | Decryption for the encryption above.
decrypt :: String -> String
decrypt = map chr . concatMap (map fst) . map readHex . partition
  where
    partition [] = []
    partition (x:y:ys) = [x,y] : partition ys
    partition _ = error "invalid encryption"
