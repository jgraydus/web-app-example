module Logger (
  HasLogger(..),
  Logger(..),
  LogLevel(..),
  toLogStr,
  withLogger,
  withPrefix,
) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import GHC.Records (getField, HasField)
import System.Log.FastLogger
  ( LogStr
  , LogType'(LogStdout)
  , newTimeCache
  , newTimedFastLogger
  , simpleTimeFormat'
  , toLogStr )

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving stock (Eq, Ord, Read, Show)

class Monad m => HasLogger m where
  log_ :: LogLevel -> LogStr -> m ()

newtype Logger = Logger { runLogger :: LogLevel -> LogStr -> IO () }

newLogger :: LogLevel -> IO (Logger, IO ())
newLogger thresholdLevel = do
  timeCache <- newTimeCache simpleTimeFormat'
  (logger, cleanup) <- newTimedFastLogger timeCache (LogStdout 4096)
  let runLogger logLevel msg = when (logLevel >= thresholdLevel) $
        logger (\time -> toLogStr time
                         <> " [" <> toLogStr (show logLevel) <> "]"
                         <> msg <> "\n")
  return (Logger runLogger, cleanup)

withLogger :: LogLevel -> (Logger -> IO a) -> IO a
withLogger logLevel program = do
  (logger, cleanup) <- newLogger logLevel
  bracket (pure logger) (const cleanup) program

withPrefix :: LogStr -> Logger -> Logger
withPrefix prefix (Logger { runLogger }) =
  let runLogger' logLevel logStr = runLogger logLevel (prefix <> logStr)
  in Logger runLogger'

instance
  ( Monad m
  , MonadIO m
  , MonadReader r m
  , HasField "logger" r Logger
  ) => HasLogger m where

  log_ logLevel logStr = do
    Logger { runLogger } <- asks (getField @"logger")
    liftIO $ runLogger logLevel logStr

