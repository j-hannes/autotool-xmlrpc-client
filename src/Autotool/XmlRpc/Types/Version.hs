------------------------------------------------------------------------------
-- | This module contains the Version data type.
--
module Autotool.XmlRpc.Types.Version (
    Version (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

------------------------------------------------------------------------------
data Version = Version {
    major :: Int
  , minor :: Int
  , micro :: Int
  }

instance Show Version where
  show v = show (major v) ++ "." ++ show (minor v) ++ "." ++ show (micro v)

instance XmlRpcType Version where
  toValue v = toValue [("major", toValue (major v)),
                       ("minor", toValue (minor v)),
                       ("micro", toValue (micro v))]
  fromValue v = do t <- fromValue v
                   s <- getField "Version" t
                   majo <- getField "major" s
                   mino <- getField "minor" s
                   micr <- getField "micro" s
                   return $ Version (toInt majo) (toInt mino) (toInt micr)
  getType _ = TStruct

toInt :: String -> Int
toInt s = read s :: Int
