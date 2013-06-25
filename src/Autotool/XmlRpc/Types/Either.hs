------------------------------------------------------------------------------
-- | This module contains just the XmlRpcType instance declaration for the
--   Either data type.
--
module Autotool.XmlRpc.Types.Either () where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

------------------------------------------------------------------------------
instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (Either a b) where
  toValue (Left s)  = toValue [("Left",  toValue s)]
  toValue (Right s) = toValue [("Right", toValue s)]
  fromValue v = do value <- fromValue v
                   eitherVal <- getFieldMaybe "Right" value
                   case eitherVal of
                     (Just r) ->
                       return $ Right r
                     Nothing -> do
                       l <- getField "Left" value
                       return $ Left l
  getType _ = TStruct
