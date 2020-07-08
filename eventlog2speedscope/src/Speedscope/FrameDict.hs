module Speedscope.FrameDict
  ( FrameName (..)
  , FrameId (..)
  , FrameDict
  , empty
  , insert
  , lookup
  , toList
  ) where

import Prelude hiding (lookup)

import Data.Aeson (ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite

newtype FrameName = FrameName { unFrameName :: Text }
  deriving (Eq, Ord, Show)
  deriving (Generic)

instance Hashable FrameName

instance ToJSON FrameName where
  toJSON FrameName { unFrameName } =
    Aeson.object [ "name" Aeson..= unFrameName ]

newtype FrameId = FrameId { unFrameId :: Int }
  deriving (Eq, Ord, Show)
  deriving (Generic)

instance ToField FrameId where
  toField = SQLite.toField . unFrameId

instance FromField FrameId where
  fromField = \x -> FrameId <$> SQLite.fromField x

instance ToJSON FrameId where
  toJSON = Aeson.toJSON . unFrameId

data FrameDict =
  FrameDict
  { frameNames :: !(Map FrameId FrameName)
  , frameIds :: !(HashMap FrameName FrameId)
  , size :: !Int
  }

empty :: FrameDict
empty =
  FrameDict
  { frameNames = Map.empty
  , frameIds = HashMap.empty
  , size = 0
  }

insert :: FrameName -> FrameDict -> (FrameId, FrameDict)
insert frameName frameDict =
  case HashMap.lookup frameName (frameIds frameDict) of
    Just frameId -> (frameId, frameDict)
    Nothing ->
      let
        frameId = FrameId (size frameDict)
        frameDict' =
          FrameDict
          { frameNames = Map.insert frameId frameName (frameNames frameDict)
          , frameIds = HashMap.insert frameName frameId (frameIds frameDict)
          , size = unFrameId frameId + 1
          }
      in (frameId, frameDict')

lookup :: FrameId -> FrameDict -> Maybe FrameName
lookup frameId frameDict = Map.lookup frameId (frameNames frameDict)

toList :: FrameDict -> [FrameName]
toList = Map.elems . frameNames
