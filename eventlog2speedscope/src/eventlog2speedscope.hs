module Main (main) where

import Control.Monad.Fail (MonadFail)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict (StateT)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple (ToRow)
import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Pipes (Consumer)
import Pipes (MonadIO)
import Pipes (Pipe)
import Pipes (Producer)
import Pipes.Safe (MonadSafe)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event)
import GHC.Stack (HasCallStack)

import Control.Applicative ((<|>))
import Control.Lens ((.=))
import Control.Lens ((%=))
import Control.Lens (at)
import Control.Monad.State.Strict (runStateT)
import Data.Function ((&))
import Data.Generics.Product (field)
import Data.IORef (newIORef)
import Data.IORef (readIORef)
import Data.IORef (writeIORef)
import Pipes ((>->))
import Pipes.GHC.RTS.Events (decodeEventLog)

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified GHC.RTS.Events as GHC
import qualified Options.Applicative as Options
import qualified Pipes as Pipes
import qualified Pipes.Aeson.Unchecked as Pipes.Aeson
import qualified Pipes.ByteString
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Safe as Pipes
import qualified Pipes.SQLite
import qualified System.IO as System

main :: IO ()
main =
  do
    let
      info =
        Options.info
          (parseOptions Options.<**> Options.helper)
          (Options.fullDesc)
    options <- Options.execParser info
    let Options { filename } = options
    withEventLog filename \producer -> withDatabase \conn ->
      do
        createTables conn
        (frames, endTime) <-
          analyzeEventLog producer >-> exportThreadEvents conn
          & Pipes.runEffect
        (exportProfile conn frames endTime >-> Pipes.ByteString.stdout)
          & Pipes.runEffect
          & Pipes.runSafeT
        pure ()

data Options =
  Options
  { filename :: !FilePath
  }

parseOptions :: Options.Parser Options
parseOptions =
  Options
  <$> Options.strArgument
    (mconcat
      [ Options.metavar "FILENAME"
      , Options.help "eventlog file"
      ]
    )

withEventLog :: FilePath -> (Producer GHC.Event IO () -> IO a) -> IO a
withEventLog filename act =
  System.withFile filename System.ReadMode
  (act . decodeEventLog . Pipes.ByteString.fromHandle)

withDatabase :: (SQLite.Connection -> IO a) -> IO a
withDatabase = SQLite.withConnection ":memory:"

createTables :: SQLite.Connection -> IO ()
createTables conn =
  SQLite.execute_ conn
    "CREATE TABLE IF NOT EXISTS\
    \ events (at INTEGER, thread INTEGER, type TEXT, frame INTEGER)"


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

data Frames =
  Frames
  { frameNames :: !(Map FrameId FrameName)
  , frameIds :: !(HashMap FrameName FrameId)
  , size :: !Int
  }

emptyFrames :: Frames
emptyFrames =
  Frames
  { frameNames = Map.empty
  , frameIds = HashMap.empty
  , size = 0
  }

insertFrameName :: FrameName -> Frames -> (FrameId, Frames)
insertFrameName frameName frames =
  case HashMap.lookup frameName (frameIds frames) of
    Just frameId -> (frameId, frames)
    Nothing ->
      let
        frameId = FrameId (size frames)
        frames' =
          Frames
          { frameNames = Map.insert frameId frameName (frameNames frames)
          , frameIds = HashMap.insert frameName frameId (frameIds frames)
          , size = unFrameId frameId + 1
          }
      in (frameId, frames')

data FrameEventType = Open | Close
  deriving (Eq, Ord, Show)

instance ToField FrameEventType where
  toField Open = SQLite.SQLText "open"
  toField Close = SQLite.SQLText "close"

instance ToJSON FrameEventType where
  toJSON Open = Aeson.String "O"
  toJSON Close = Aeson.String "C"

data FrameEvent =
  FrameEvent
  { eventType :: !FrameEventType
  , eventAt :: !GHC.Timestamp
  , eventFrameId :: !FrameId
  }
  deriving (Eq, Ord, Show)
  deriving (Generic)

instance ToJSON FrameEvent where
  toJSON FrameEvent { eventType, eventAt, eventFrameId } =
    Aeson.object
    [ "type" Aeson..= eventType
    , "at" Aeson..= eventAt
    , "frame" Aeson..= eventFrameId
    ]

instance FromRow FrameEvent where
  fromRow =
    do
      eventAt <- SQLite.field
      eventType <-
        SQLite.field @Text >>= \case
          "open" -> pure Open
          "close" -> pure Close
          _ -> error "expected open or close"
      eventFrameId <- SQLite.field
      pure FrameEvent { eventType, eventAt, eventFrameId }

instance ToRow FrameEvent where
  toRow FrameEvent { eventAt, eventType, eventFrameId } =
    [ SQLite.toField eventAt
    , SQLite.toField eventType
    , SQLite.toField eventFrameId
    ]

data ThreadEventType
  = ThreadFrameEvent !(FrameEventType, FrameId)
  deriving (Eq, Ord, Show)

data ThreadEvent =
  ThreadEvent
  { frameEvent :: !FrameEvent
  , eventThreadId :: !GHC.ThreadId
  }
  deriving (Eq, Ord, Show)
  deriving (Generic)

instance ToRow ThreadEvent where
  toRow ThreadEvent { eventThreadId, frameEvent } =
    SQLite.toField eventThreadId : SQLite.toRow frameEvent

instance FromRow ThreadEvent where
  fromRow =
    do
      eventThreadId <- SQLite.field
      frameEvent <- SQLite.fromRow
      pure ThreadEvent { frameEvent, eventThreadId }

data ProcessAnalysis =
  ProcessAnalysis
  { frames :: !Frames
  , caps :: !(IntMap GHC.ThreadId)
  , endTime :: !GHC.Timestamp
  }
  deriving (Generic)

emptyProcessAnalysis :: ProcessAnalysis
emptyProcessAnalysis =
  ProcessAnalysis
  { frames = emptyFrames
  , caps = IntMap.empty
  , endTime = 0
  }

refStateT :: MonadIO m => IORef st -> StateT st m x -> m x
refStateT ioRef act =
  do
    st <- readIORef ioRef & Pipes.liftIO
    (x, st') <- runStateT act st
    writeIORef ioRef st' & Pipes.liftIO
    pure x

analyzeEventLog
  :: MonadIO m
  => Producer Event m x
  -> Producer ThreadEvent m (Frames, GHC.Timestamp)
analyzeEventLog producer =
  do
    ref <- newIORef emptyProcessAnalysis & Pipes.liftIO
    _ <- Pipes.for producer \event ->
      do
        updateCaps event
        updateEndTime event
        processThreadFrameEvent event
      & Pipes.hoist (refStateT ref)
    st <- readIORef ref & Pipes.liftIO
    pure (frames st, endTime st)

updateEndTime :: MonadState ProcessAnalysis m => GHC.Event -> m ()
updateEndTime event = field @"endTime" %= max (GHC.evTime event)

expectCap :: HasCallStack => Applicative m => GHC.Event -> m Int
expectCap event =
  case GHC.evCap event of
    Just cap -> pure cap
    Nothing -> error "expected capability"

updateCaps :: MonadState ProcessAnalysis m => GHC.Event -> Producer ThreadEvent m ()
updateCaps event =
  case GHC.evSpec event of
    GHC.RunThread { GHC.thread = threadId } ->
      do
        cap <- expectCap event
        field @"caps" . at cap .= Just threadId
    GHC.StopThread { } ->
      do
        cap <- expectCap event
        field @"caps" . at cap .= Nothing
    _ -> pure ()

decodeMessage :: Text -> Maybe (FrameEventType, FrameName)
decodeMessage txt = openFrame <|> closeFrame
  where
    openFrame = (,) Open . FrameName <$> Text.stripPrefix "START " txt
    closeFrame = (,) Close . FrameName <$> Text.stripPrefix "STOP " txt

expectThreadId
  :: HasCallStack
  => MonadState ProcessAnalysis m
  => GHC.Event
  -> m GHC.ThreadId
expectThreadId event =
  expectCap event >>= \cap ->
  fmap (IntMap.lookup cap) (Lens.use (field @"caps")) >>=
    \case
      Just threadId -> pure threadId
      Nothing -> error "expected thread"

getFrameId :: MonadState Frames m => FrameName -> m FrameId
getFrameId frameName = State.state (insertFrameName frameName)

processThreadFrameEvent
  :: MonadIO m
  => GHC.Event
  -> Producer ThreadEvent (StateT ProcessAnalysis m) ()
processThreadFrameEvent event =
  case GHC.evSpec event of
    GHC.UserMessage { GHC.msg = msg }
      | Just (eventType, frameName) <- decodeMessage msg ->
        do
          let eventAt = GHC.evTime event
          eventThreadId <- expectThreadId event
          eventFrameId <-
            getFrameId frameName
            & Lens.zoom (field @"frames")
            & Pipes.lift
          Pipes.yield ThreadEvent
              { frameEvent =
                FrameEvent
                { eventAt
                , eventType
                , eventFrameId
                }
              , eventThreadId
              }
    _ -> pure ()

exportThreadEvents :: MonadIO m => Connection -> Consumer ThreadEvent m x
exportThreadEvents conn =
  Pipes.SQLite.execute conn
  "INSERT INTO events (thread, at, type, frame) VALUES (?, ?, ?, ?)"

importThreadEvents
  :: MonadSafe m
  => Connection
  -> GHC.ThreadId
  -> Producer ThreadEvent m ()
importThreadEvents conn threadId =
  Pipes.SQLite.query conn
    "SELECT thread, at, type, frame FROM events\
    \ WHERE thread = ? ORDER BY at ASC"
    (SQLite.Only threadId)

countUserEvents
  :: (MonadFail m, MonadIO m)
  => Connection
  -> GHC.ThreadId
  -> m Int
countUserEvents conn threadId =
  do
    [SQLite.Only r] <-
      SQLite.query conn
        "SELECT COUNT (frame) FROM events WHERE thread = ?"
        (SQLite.Only threadId)
      & Pipes.liftIO
    pure r

data ThreadAnalysis =
  ThreadAnalysis
  { stack :: ![FrameId]
  }
  deriving (Show)
  deriving (Generic)

emptyThreadAnalysis :: ThreadAnalysis
emptyThreadAnalysis =
  ThreadAnalysis
  { stack = []
  }

analyzeThread
  :: MonadSafe m
  => Connection
  -> GHC.ThreadId
  -> Producer FrameEvent m ()
analyzeThread conn threadId =
  do
    ref <- newIORef emptyThreadAnalysis & Pipes.liftIO
    Pipes.for (importThreadEvents conn threadId)
      (\event -> processThreadEvent event & Pipes.hoist (refStateT ref))
    pure ()

processThreadEvent
  :: MonadState ThreadAnalysis m
  => ThreadEvent
  -> Producer FrameEvent m ()
processThreadEvent ThreadEvent { frameEvent } = threadFrame frameEvent

threadFrame
  :: MonadState ThreadAnalysis m
  => FrameEvent
  -> Producer FrameEvent m ()
threadFrame frameEvent@FrameEvent { eventType, eventFrameId } =
  do
    isRecursive <-
      case eventType of
        Open ->
          do
            isRecursive <- (==) (Just eventFrameId) <$> peekThread
            pushThread eventFrameId
            pure isRecursive
        Close ->
          do
            popThread eventFrameId
            (==) (Just eventFrameId) <$> peekThread
    Monad.unless isRecursive (Pipes.yield frameEvent)

peekThread :: MonadState ThreadAnalysis m => m (Maybe FrameId)
peekThread =
  Lens.use (field @"stack") >>= \case
    [] -> pure Nothing
    frameId : _ -> pure (Just frameId)

pushThread :: MonadState ThreadAnalysis m => FrameId -> m ()
pushThread frameId = field @"stack" %= (:) frameId

popThread :: HasCallStack => MonadState ThreadAnalysis m => FrameId -> m ()
popThread frameId =
  do
    ThreadAnalysis { stack } <- State.get
    case stack of
      [] ->
        (error . unwords)
        [ "expected"
        , show frameId
        , "but found nothing"
        ]
      (frameId' : stack')
        | frameId' == frameId -> field @"stack" .= stack'
        | otherwise ->
          (error.unwords)
          [ "expected"
          , show frameId
          , "but found"
          , show frameId'
          ]

exportFrameEvents :: Functor m => Pipe FrameEvent Aeson.Value m r
exportFrameEvents = Pipes.map Aeson.toJSON

array :: MonadIO m => Producer Aeson.Value m r -> Producer ByteString m r
array producer =
  do
    Pipes.yield "["
    r <-
      Pipes.for
        (intersperse Nothing (producer >-> Pipes.map Just))
        (maybe comma Pipes.Aeson.encode)
    Pipes.yield "]"
    pure r

pair
  :: forall a m
  .  (Monad m, Aeson.ToJSON a)
  => String
  -> a
  -> Producer ByteString m ()
pair name value =
  do
    Pipes.yield "\""
    Pipes.yield (ByteString.Char8.pack name)
    Pipes.yield "\":"
    Pipes.Aeson.encode value

intersperse :: MonadIO m => a -> Producer a m r -> Producer a m r
intersperse a producer =
  do
    ref <- newIORef True & Pipes.liftIO
    Pipes.for producer \a' ->
      do
        isEmpty <-
          do
            r <- readIORef ref
            writeIORef ref False
            pure r
          & Pipes.liftIO
        Monad.unless isEmpty (Pipes.yield a)
        Pipes.yield a'

schema :: Text
schema = "https://www.speedscope.app/file-format-schema.json"

exportFrames :: Functor m => Frames -> Producer Aeson.Value m ()
exportFrames Frames { frameNames } =
  Pipes.each frameNames >-> Pipes.map Aeson.toJSON

comma :: Functor m => Producer ByteString m ()
comma = Pipes.yield ","

exportProfile
  :: (MonadFail m, MonadSafe m)
  => Connection
  -> Frames
  -> GHC.Timestamp
  -> Producer ByteString m ()
exportProfile conn frames endTime =
  do
    Pipes.yield "{"
    pair "$schema" schema
    comma
    Pipes.yield "\"shared\":{"
    Pipes.yield "\"frames\":"
    array (exportFrames frames)
    Pipes.yield "}"
    comma
    Pipes.yield "\"profiles\":["

    threads <-
      getThreads conn >>= Monad.filterM \threadId ->
      do
        count <- countUserEvents conn threadId
        pure (count > 0)
    Foldable.for_ (List.intersperse Nothing $ map Just threads)
      \case
        Nothing -> comma
        Just thread ->
          do
            Pipes.yield "{"
            pair "type" ("evented" :: Text)
            comma
            pair "name" ("thread " ++ show thread)
            comma
            pair "unit" ("nanoseconds" :: Text)
            comma
            pair "startValue" (0 :: GHC.Timestamp)
            comma
            pair "endValue" (endTime :: GHC.Timestamp)
            comma
            Pipes.yield "\"events\":"
            array (analyzeThread conn thread >-> exportFrameEvents)
            Pipes.yield "}"

    Pipes.yield "]}"

getThreads :: MonadIO m => Connection -> m [GHC.ThreadId]
getThreads conn =
      SQLite.query_ conn "SELECT DISTINCT thread FROM events"
      & fmap (map SQLite.fromOnly)
      & Pipes.liftIO

-- TODO: Print entry counts
-- TODO: Omit empty profiles
-- TODO: --omit-children of select symbols
