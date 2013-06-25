------------------------------------------------------------------------------
-- | This module contains the documented data type.
--
module Autotool.XmlRpc.Types.Documented (
    Documented (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

import Autotool.XmlRpc.Types.Basic (Description)

------------------------------------------------------------------------------
data Documented a = Documented {
    contents :: a
  , documentation :: Description
  } deriving Show

instance XmlRpcType a => XmlRpcType (Documented a) where
  toValue s = toValue [("contents",      toValue (contents s)),
                       ("documentation", toValue (documentation s))]
  fromValue v = do t <- fromValue v
                   s <- getField "Documented" t
                   c <- getField "contents" s
                   d <- getField "documentation" s
                   return $ Documented c d
  getType _ = TStruct
