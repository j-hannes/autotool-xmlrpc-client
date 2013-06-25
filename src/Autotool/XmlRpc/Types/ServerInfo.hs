------------------------------------------------------------------------------
-- | This module contains the ServerInfo data tyoe.
--
module Autotool.XmlRpc.Types.ServerInfo (
    ServerInfo (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

import Autotool.XmlRpc.Types.Basic (Name)
import Autotool.XmlRpc.Types.Version (Version)

------------------------------------------------------------------------------
data ServerInfo = ServerInfo {
    protocolVersion :: Version
  , serverName      :: Name
  , serverVersion   :: Version
  } deriving (Show)

instance XmlRpcType ServerInfo where
  toValue s = toValue [("protocol_version", toValue (protocolVersion s)),
                       ("server_name",      toValue (serverName s)),
                       ("server_version",   toValue (serverVersion s))]
  fromValue v = do t <- fromValue v
                   s <- getField "ServerInfo" t
                   pv <- getField "protocol_version" s
                   sn <- getField "server_name" s
                   sv <- getField "server_version" s
                   return $ ServerInfo pv sn sv
  getType _ = TStruct
