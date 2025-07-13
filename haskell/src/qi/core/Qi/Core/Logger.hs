{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Qi.Core.Logger
  ( -- * Logger Types
    Logger(..)
  , LogLevel(..)
  , LogFormat(..)
  , LogDestination(..)
  , LoggerConfig(..)
  , LogContext
    -- * Factory Operations
  , create
  , createDefault
    -- * Logging Operations
  , debug
  , info
  , warn
  , error
  , fatal
    -- * Performance Operations  
  , isLevelEnabled
  , withContext
    -- * Utilities
  , formatMessage
  , levelToInt
  ) where

import Prelude hiding (error)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar, atomically)
import System.IO (Handle, stdout, stderr, hPutStrLn)
import GHC.Generics (Generic)

import Qi.Base.Result (Result)
import qualified Qi.Base.Result as R
import Qi.Base.Error (QiError, ErrorCategory(..))
import qualified Qi.Base.Error as E

-- | Log levels with hierarchy: DEBUG < INFO < WARN < ERROR < FATAL
data LogLevel 
  = DEBUG   -- 10
  | INFO    -- 20  
  | WARN    -- 30
  | ERROR   -- 40
  | FATAL   -- 50
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Log output format
data LogFormat
  = TEXT    -- Human-readable text format
  | JSON    -- Structured JSON format
  | CUSTOM  -- Custom formatter function
  deriving stock (Show, Eq, Generic)

-- | Log destination
data LogDestination
  = Console Handle          -- Console output (stdout/stderr)
  | File FilePath          -- File output
  | Multiple [LogDestination] -- Multiple destinations
  deriving stock (Show, Eq, Generic)

-- | Log context for structured logging
type LogContext = Map.Map T.Text A.Value

-- | Logger configuration
data LoggerConfig = LoggerConfig
  { logLevel :: LogLevel           -- Minimum level to log
  , logFormat :: LogFormat         -- Output format
  , logDestination :: LogDestination -- Where to output logs
  , logContext :: LogContext       -- Default context
  } deriving stock (Show, Eq, Generic)

-- | Logger instance with configuration and state
data Logger = Logger
  { loggerConfig :: LoggerConfig
  , loggerContext :: TVar LogContext  -- Accumulated context
  } deriving stock (Generic)

-- | Create logger instance with configuration
create :: (MonadIO m) => LoggerConfig -> m (Result Logger)
create config = liftIO $ do
  contextVar <- newTVarIO (logContext config)
  if validateConfig config
    then pure $ R.success $ Logger config contextVar
    else do
      timestamp <- getCurrentTime
      pure $ R.failure $ 
        E.create "VALIDATION_ERROR" "Invalid logger configuration" 
                 VALIDATION Map.empty Nothing timestamp

-- | Create logger with sensible defaults
createDefault :: (MonadIO m) => m (Result Logger)
createDefault = create defaultConfig
  where
    defaultConfig = LoggerConfig
      { logLevel = INFO
      , logFormat = TEXT
      , logDestination = Console stdout
      , logContext = Map.empty
      }

-- | Log debug-level message
debug :: (MonadIO m) => T.Text -> Maybe LogContext -> Logger -> m ()
debug message contextMaybe logger = 
  logAtLevel DEBUG message contextMaybe logger

-- | Log info-level message  
info :: (MonadIO m) => T.Text -> Maybe LogContext -> Logger -> m ()
info message contextMaybe logger = 
  logAtLevel INFO message contextMaybe logger

-- | Log warning-level message
warn :: (MonadIO m) => T.Text -> Maybe LogContext -> Logger -> m ()
warn message contextMaybe logger = 
  logAtLevel WARN message contextMaybe logger

-- | Log error-level message
error :: (MonadIO m) => T.Text -> Maybe QiError -> Maybe LogContext -> Logger -> m ()
error message errorMaybe contextMaybe logger = do
  let errorContext = case errorMaybe of
        Nothing -> Map.empty
        Just err -> Map.singleton "error" (A.String $ E.toString err)
  let fullContext = case contextMaybe of
        Nothing -> errorContext
        Just ctx -> Map.union ctx errorContext
  logAtLevel ERROR message (Just fullContext) logger

-- | Log fatal-level message
fatal :: (MonadIO m) => T.Text -> Maybe QiError -> Maybe LogContext -> Logger -> m ()
fatal message errorMaybe contextMaybe logger = do
  let errorContext = case errorMaybe of
        Nothing -> Map.empty
        Just err -> Map.singleton "error" (A.String $ E.toString err)
  let fullContext = case contextMaybe of
        Nothing -> errorContext
        Just ctx -> Map.union ctx errorContext
  logAtLevel FATAL message (Just fullContext) logger

-- | Check if level would be logged (performance optimization)
isLevelEnabled :: LogLevel -> Logger -> Bool
isLevelEnabled level logger = level >= logLevel (loggerConfig logger)

-- | Create logger with additional context
withContext :: (MonadIO m) => LogContext -> Logger -> m Logger
withContext newContext logger = liftIO $ do
  currentContext <- atomically $ readTVar (loggerContext logger)
  let mergedContext = Map.union newContext currentContext
  newContextVar <- newTVarIO mergedContext
  pure $ logger { loggerContext = newContextVar }

-- Internal functions

logAtLevel :: (MonadIO m) => LogLevel -> T.Text -> Maybe LogContext -> Logger -> m ()
logAtLevel level message contextMaybe logger = liftIO $ do
  if isLevelEnabled level logger
    then do
      timestamp <- getCurrentTime
      currentContext <- atomically $ readTVar (loggerContext logger)
      let finalContext = case contextMaybe of
            Nothing -> currentContext
            Just ctx -> Map.union ctx currentContext
      
      let logEntry = LogEntry
            { entryTimestamp = timestamp
            , entryLevel = level
            , entryMessage = message
            , entryContext = finalContext
            }
      
      writeLogEntry (loggerConfig logger) logEntry
    else pure ()

-- | Log entry data structure
data LogEntry = LogEntry
  { entryTimestamp :: UTCTime
  , entryLevel :: LogLevel
  , entryMessage :: T.Text
  , entryContext :: LogContext
  } deriving stock (Show, Generic)

writeLogEntry :: LoggerConfig -> LogEntry -> IO ()
writeLogEntry config entry = do
  let formatted = formatMessage (logFormat config) entry
  writeToDestination (logDestination config) formatted

writeToDestination :: LogDestination -> T.Text -> IO ()
writeToDestination destination message = 
  case destination of
    Console handle -> TIO.hPutStrLn handle message
    File path -> TIO.appendFile path (message <> "\n")
    Multiple dests -> mapM_ (`writeToDestination` message) dests

-- | Format log message according to format
formatMessage :: LogFormat -> LogEntry -> T.Text
formatMessage format entry = 
  case format of
    TEXT -> formatText entry
    JSON -> formatJSON entry
    CUSTOM -> formatText entry -- Fallback to text for now

formatText :: LogEntry -> T.Text
formatText entry = 
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (entryTimestamp entry)
      level = T.pack $ show (entryLevel entry)
      message = entryMessage entry
      contextStr = if Map.null (entryContext entry)
        then ""
        else " | " <> T.pack (show $ entryContext entry)
  in timestamp <> " [" <> level <> "] " <> message <> contextStr

formatJSON :: LogEntry -> T.Text
formatJSON entry = 
  let logObject = A.object
        [ "timestamp" A..= entryTimestamp entry
        , "level" A..= entryLevel entry  
        , "message" A..= entryMessage entry
        , "context" A..= entryContext entry
        ]
  in T.decodeUtf8 $ A.encode logObject

-- | Convert log level to numeric value
levelToInt :: LogLevel -> Int
levelToInt DEBUG = 10
levelToInt INFO = 20
levelToInt WARN = 30
levelToInt ERROR = 40
levelToInt FATAL = 50

validateConfig :: LoggerConfig -> Bool
validateConfig config = 
  logLevel config `elem` [DEBUG, INFO, WARN, ERROR, FATAL] &&
  logFormat config `elem` [TEXT, JSON, CUSTOM]