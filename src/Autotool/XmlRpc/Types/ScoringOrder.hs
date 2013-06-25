------------------------------------------------------------------------------
-- | This module contains the ScoringOrder data type.
--
module Autotool.XmlRpc.Types.ScoringOrder (
    ScoringOrder(..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

------------------------------------------------------------------------------
data ScoringOrder = Increasing
                  | None
                  | Decreasing
                  deriving (Eq, Read, Show)

instance XmlRpcType ScoringOrder where
  toValue Increasing = toValue "Increasing"
  toValue None       = toValue "None"
  toValue Decreasing = toValue "Decreasing"
  fromValue v = do value <- fromValue v
                   option1 <- helper "Increasing" value
                   option2 <- helper "Decreasing" value
                   case option1 of
                     (Just _) -> return Increasing
                     Nothing  -> case option2 of
                       (Just _) -> return Decreasing
                       Nothing  -> return None
  getType _ = TStruct

helper :: (Monad m) => String -> [(String, Value)] -> Err m (Maybe [Value])
helper = getFieldMaybe
