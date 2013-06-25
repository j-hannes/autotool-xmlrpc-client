-- {-# OPTIONS_GHC -fth #-}

------------------------------------------------------------------------------
-- | This module contains the Signed data type.
--
module Autotool.XmlRpc.Types.Signed (
    Signed (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals
-- import Network.XmlRpc.THDeriveXmlRpcType
------------------------------------------------------------------------------
import Autotool.XmlRpc.Types.Basic (Signature)

------------------------------------------------------------------------------
data Signed a = Signed {
    contents :: a
  , signature :: Signature
  } deriving (Read, Show)

instance XmlRpcType a => XmlRpcType (Signed a) where
  toValue s = toValue [("Signed", [("contents",  toValue (contents s)),
                       ("signature", toValue (signature s))])]
  fromValue v = do t <- fromValue v
                   s <- getField "Signed" t
                   c <- getField "contents" s
                   d <- getField "signature" s
                   return $ Signed c d
  getType _ = TStruct

{-
-}

-- $(asXmlRpcStruct ''Person)
