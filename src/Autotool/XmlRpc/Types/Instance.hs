------------------------------------------------------------------------------
-- | This module contains the Instance data type.
--
module Autotool.XmlRpc.Types.Instance (
    Instance (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

------------------------------------------------------------------------------
data Instance = Instance {
    tag :: String
  , contents :: String
  } deriving (Read, Show)

instance XmlRpcType Instance where
  toValue s = toValue [("Instance", [("tag",      toValue (tag s)),
                       ("contents", toValue (contents s))])]
  fromValue v = do t <- fromValue v
                   s <- getField "Instance" t
                   c <- getField "tag" s
                   d <- getField "contents" s
                   return $ Instance c d
  getType _ = TStruct
