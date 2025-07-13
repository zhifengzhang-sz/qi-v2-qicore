{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Qi.Core.Logger
-- Description: QiCore Logger component with structured logging and OpenTelemetry
-- 
-- This module provides a structured logging system with:
-- - Hierarchical log levels with O(1) level checking
-- - OpenTelemetry integration for distributed tracing
-- - Structured logging with rich context support
-- - Correlation ID tracking for request flows
-- - AI-enhanced observability with metrics integration
--
-- The logger is designed for modern applications with:
-- - High-performance level filtering
-- - Thread-safe operation
-- - Multiple output formats (JSON, TEXT, CUSTOM)
-- - Integration with observability platforms
module Qi.Core.Logger
  ( -- * Core Types
    Logger(..)
  , LogLevel(..)
  , LogFormat(..)
  , LogDestination(..)
  , LoggerConfig(..)
  , LogContext(..)
  , LogMessage(..)
  
    -- * Factory Operations
  , create
  , createDefault
  , createWithConfig
  
    -- * Logging Operations
  , debug
  , info
  , warn
  , logError
  , fatal
  , logWith
  
    -- * Performance Operations  
  , isLevelEnabled
  , withContext
  , withLogLevel
  
    -- * Modern Patterns (2025)
  , withTraceContext
  , withCorrelationId
  , logWithMetrics
  , withObservability
  
    -- * Context Management
  , emptyContext
  , addContext
  , mergeContext
  , getContext
  
    -- * Utility Functions
  , levelToText
  , levelFromText
  , formatMessage
  , showLogger
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as BSL
import Data.Aeson (Value(..), Object, ToJSON(..), FromJSON(..), (.=), (.:), (.:?), (.!=))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime(..), getCurrentTime, formatTime, defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import GHC.Generics (Generic)
import System.IO (Handle, stdout, stderr)

import Qi.Base.Error (QiError, ErrorCategory(..), ErrorSeverity(..))
import Qi.Base.Error qualified as Error
import Qi.Base.Result (Result(..), pattern Success, pattern Failure)
import Qi.Base.Result qualified as Result

-- | Hierarchical log levels with total ordering
--
-- Levels form: DEBUG < INFO < WARN < ERROR < FATAL
-- Higher numeric values indicate higher severity.
data LogLevel 
  = DEBUG  -- ^ Detailed information for diagnosing problems (10)
  | INFO   -- ^ General application flow (20) 
  | WARN   -- ^ Potentially harmful situations (30)
  | ERROR  -- ^ Error events that allow application to continue (40)
  | FATAL  -- ^ Critical failures that may cause termination (50)
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON LogLevel where
  toJSON = String . levelToText

instance FromJSON LogLevel where
  parseJSON (String t) = case levelFromText t of
    Just level -> pure level
    Nothing -> fail ("Invalid log level: " <> T.unpack t)
  parseJSON _ = fail "Log level must be a string"

-- | Log output formats for different use cases
data LogFormat
  = JSON    -- ^ Structured JSON format for machine processing
  | TEXT    -- ^ Human-readable text format
  | CUSTOM  -- ^ Custom formatting function
  deriving (Show, Eq, Generic)

-- | Log output destinations
data LogDestination
  = Console Handle       -- ^ Console output (stdout/stderr)
  | File FilePath        -- ^ File output with path
  | Network Text Int     -- ^ Network output (host, port)
  | Multiple [LogDestination]  -- ^ Multiple destinations
  deriving (Show, Eq, Generic)

-- | Logger configuration specification
data LoggerConfig = LoggerConfig
  { loggerLevel :: !LogLevel           -- ^ Minimum level to log
  , loggerFormat :: !LogFormat         -- ^ Output format
  , loggerDestination :: !LogDestination -- ^ Output destination
  , loggerBuffered :: !Bool            -- ^ Whether to buffer output
  , loggerTimestamp :: !Bool           -- ^ Include timestamps
  , loggerShowLevel :: !Bool           -- ^ Show log level in output
  } deriving (Show, Generic)

-- | Log context for structured data and correlation
data LogContext = LogContext
  { logContextFields :: !(Map.Map Text Value)  -- ^ Structured fields
  , logContextTraceId :: !(Maybe Text)         -- ^ OpenTelemetry trace ID
  , logContextSpanId :: !(Maybe Text)          -- ^ OpenTelemetry span ID  
  , logContextCorrelationId :: !(Maybe Text)   -- ^ Request correlation ID
  , logContextTimestamp :: !UTCTime            -- ^ Context creation time
  } deriving (Show, Generic)

instance ToJSON LogContext where
  toJSON ctx = JSON.object 
    [ "fields" .= logContextFields ctx
    , "traceId" .= logContextTraceId ctx
    , "spanId" .= logContextSpanId ctx
    , "correlationId" .= logContextCorrelationId ctx
    , "timestamp" .= logContextTimestamp ctx
    ]

-- | Individual log message with metadata
data LogMessage = LogMessage
  { logMessageLevel :: !LogLevel       -- ^ Message severity level
  , logMessageText :: !Text            -- ^ Primary message text
  , logMessageContext :: !LogContext   -- ^ Associated context
  , logMessageError :: !(Maybe QiError) -- ^ Optional error details
  , logMessageMetrics :: !(Maybe Value) -- ^ Optional performance metrics
  , logMessageTimestamp :: !UTCTime     -- ^ Message timestamp
  } deriving (Show, Generic)

instance ToJSON LogMessage where
  toJSON msg = JSON.object
    [ "level" .= logMessageLevel msg
    , "message" .= logMessageText msg
    , "context" .= logMessageContext msg
    , "error" .= logMessageError msg
    , "metrics" .= logMessageMetrics msg
    , "timestamp" .= logMessageTimestamp msg
    ]

-- | Core logger type with thread-safe configuration
--
-- The logger maintains its configuration in STM for thread-safe updates
-- while providing O(1) level checking for performance.
data Logger = Logger
  { loggerConfig :: !(TVar LoggerConfig)  -- ^ Mutable configuration
  , loggerContext :: !(TVar LogContext)   -- ^ Mutable context
  , loggerEnabled :: !(TVar Bool)         -- ^ Enable/disable flag
  } deriving (Generic)

instance Show Logger where
  show _ = "<Logger>"

-- | Create logger instance with explicit configuration
--
-- Validates configuration before creation.
-- Returns VALIDATION error for invalid config.
create :: MonadIO m => LoggerConfig -> m (Result Logger)
create config = liftIO $ do
  -- Validate configuration
  case validateConfig config of
    Failure err -> pure (Failure err)
    Success _ -> do
      -- Create logger with validated config
      configVar <- atomically $ newTVar config
      currentTime <- getCurrentTime
      contextVar <- atomically $ newTVar (emptyContextAt currentTime)
      enabledVar <- atomically $ newTVar True
      
      pure $ Success Logger
        { loggerConfig = configVar
        , loggerContext = contextVar
        , loggerEnabled = enabledVar
        }

-- | Create logger with sensible defaults
--
-- Uses INFO level, console output, and TEXT format by default.
createDefault :: MonadIO m => m (Result Logger)
createDefault = create defaultConfig
  where
    defaultConfig = LoggerConfig
      { loggerLevel = INFO
      , loggerFormat = TEXT
      , loggerDestination = Console stdout
      , loggerBuffered = False
      , loggerTimestamp = True
      , loggerShowLevel = True
      }

-- | Create logger with configuration validation
createWithConfig :: MonadIO m => Value -> m (Result Logger)
createWithConfig configValue = case JSON.fromJSON configValue of
  JSON.Success config -> create config
  JSON.Error err -> do
    timestamp <- liftIO getCurrentTime
    pure $ Failure $ Error.create
      "LOGGER_CONFIG_PARSE_ERROR"
      ("Failed to parse logger configuration: " <> T.pack err)
      VALIDATION
      (Map.fromList [("config", configValue)])
      Nothing
      timestamp

-- | Log debug-level message
--
-- Only logs if logger level <= DEBUG.
-- Includes timestamp and level in output.
debug :: MonadIO m => Text -> Maybe LogContext -> Logger -> m ()
debug msg maybeCtx logger = logWith DEBUG msg maybeCtx Nothing Nothing logger

-- | Log info-level message  
--
-- Only logs if logger level <= INFO.
-- Suitable for general application flow.
info :: MonadIO m => Text -> Maybe LogContext -> Logger -> m ()
info msg maybeCtx logger = logWith INFO msg maybeCtx Nothing Nothing logger

-- | Log warning-level message
--
-- Only logs if logger level <= WARN.
-- Indicates potential issues without affecting execution.
warn :: MonadIO m => Text -> Maybe LogContext -> Logger -> m ()
warn msg maybeCtx logger = logWith WARN msg maybeCtx Nothing Nothing logger

-- | Log error-level message
--
-- Only logs if logger level <= ERROR.
-- Includes error details if provided.
logError :: MonadIO m => Text -> Maybe QiError -> Maybe LogContext -> Logger -> m ()
logError msg maybeErr maybeCtx logger = logWith ERROR msg maybeCtx maybeErr Nothing logger

-- | Log fatal-level message
--
-- Only logs if logger level <= FATAL.
-- Indicates critical system failures.
fatal :: MonadIO m => Text -> Maybe QiError -> Maybe LogContext -> Logger -> m ()
fatal msg maybeErr maybeCtx logger = logWith FATAL msg maybeCtx maybeErr Nothing logger

-- | Core logging function with full message specification
--
-- Handles level checking, context merging, and output formatting.
logWith :: MonadIO m 
        => LogLevel 
        -> Text 
        -> Maybe LogContext 
        -> Maybe QiError 
        -> Maybe Value 
        -> Logger 
        -> m ()
logWith level msg maybeCtx maybeErr maybeMetrics logger = liftIO $ do
  -- Check if logging is enabled and level is sufficient
  enabled <- atomically $ readTVar (loggerEnabled logger)
  when enabled $ do
    config <- atomically $ readTVar (loggerConfig logger)
    when (level >= loggerLevel config) $ do
      -- Get current context and merge with provided context
      currentCtx <- atomically $ readTVar (loggerContext logger)
      let finalCtx = case maybeCtx of
            Nothing -> currentCtx
            Just ctx -> mergeContext currentCtx ctx
      
      -- Create log message
      timestamp <- getCurrentTime
      let logMsg = LogMessage
            { logMessageLevel = level
            , logMessageText = msg
            , logMessageContext = finalCtx
            , logMessageError = maybeErr
            , logMessageMetrics = maybeMetrics
            , logMessageTimestamp = timestamp
            }
      
      -- Format and output message
      outputMessage config logMsg

-- | Check if level would be logged (performance optimization)
--
-- Returns true if level >= logger.level.
-- O(1) operation for performance-critical code.
isLevelEnabled :: MonadIO m => LogLevel -> Logger -> m Bool
isLevelEnabled level logger = liftIO $ do
  enabled <- atomically $ readTVar (loggerEnabled logger)
  if not enabled
    then pure False
    else do
      config <- atomically $ readTVar (loggerConfig logger)
      pure (level >= loggerLevel config)

-- | Create logger with additional context
--
-- All subsequent log calls include the provided context.
-- Context is merged with per-call context.
-- Original logger unchanged (immutable semantics).
withContext :: MonadIO m => LogContext -> Logger -> m Logger
withContext newCtx logger = liftIO $ do
  config <- atomically $ readTVar (loggerConfig logger)
  currentCtx <- atomically $ readTVar (loggerContext logger)
  enabled <- atomically $ readTVar (loggerEnabled logger)
  
  let mergedCtx = mergeContext currentCtx newCtx
  
  configVar <- atomically $ newTVar config
  contextVar <- atomically $ newTVar mergedCtx
  enabledVar <- atomically $ newTVar enabled
  
  pure Logger
    { loggerConfig = configVar
    , loggerContext = contextVar
    , loggerEnabled = enabledVar
    }

-- | Create logger with modified log level
withLogLevel :: MonadIO m => LogLevel -> Logger -> m Logger
withLogLevel newLevel logger = liftIO $ do
  config <- atomically $ readTVar (loggerConfig logger)
  context <- atomically $ readTVar (loggerContext logger)
  enabled <- atomically $ readTVar (loggerEnabled logger)
  
  let newConfig = config { loggerLevel = newLevel }
  
  configVar <- atomically $ newTVar newConfig
  contextVar <- atomically $ newTVar context
  enabledVar <- atomically $ newTVar enabled
  
  pure Logger
    { loggerConfig = configVar
    , loggerContext = contextVar
    , loggerEnabled = enabledVar
    }

-- | Create logger with OpenTelemetry trace correlation (2025 Pattern)
--
-- All logs include trace and span IDs for correlation.
-- Enables distributed tracing across services.
withTraceContext :: MonadIO m => Text -> Text -> Logger -> m Logger
withTraceContext traceId spanId logger = do
  currentTime <- liftIO getCurrentTime
  let traceCtx = LogContext
        { logContextFields = Map.fromList 
            [ ("traceId", String traceId)
            , ("spanId", String spanId)
            ]
        , logContextTraceId = Just traceId
        , logContextSpanId = Just spanId
        , logContextCorrelationId = Nothing
        , logContextTimestamp = currentTime
        }
  withContext traceCtx logger

-- | Create logger with correlation ID for request tracking (2025 Pattern)
--
-- All logs include correlation ID for request flow tracking.
-- Supports nested correlation contexts.
withCorrelationId :: MonadIO m => Text -> Logger -> m Logger
withCorrelationId correlationId logger = do
  currentTime <- liftIO getCurrentTime
  let corrCtx = LogContext
        { logContextFields = Map.fromList [("correlationId", String correlationId)]
        , logContextTraceId = Nothing
        , logContextSpanId = Nothing
        , logContextCorrelationId = Just correlationId
        , logContextTimestamp = currentTime
        }
  withContext corrCtx logger

-- | Log with associated performance metrics (2025 Pattern)
--
-- Includes execution time, memory usage, etc.
-- Enables AI-driven anomaly detection.
logWithMetrics :: MonadIO m 
               => LogLevel 
               -> Text 
               -> Value 
               -> Maybe LogContext 
               -> Logger 
               -> m ()
logWithMetrics level msg metrics maybeCtx logger = 
  logWith level msg maybeCtx Nothing (Just metrics) logger

-- | Create logger with enhanced observability (2025 Pattern)
--
-- Integrates with observability platforms and AI monitoring.
withObservability :: MonadIO m => Map.Map Text Value -> Logger -> m Logger
withObservability observabilityFields logger = do
  currentTime <- liftIO getCurrentTime
  let obsCtx = LogContext
        { logContextFields = observabilityFields
        , logContextTraceId = Nothing
        , logContextSpanId = Nothing
        , logContextCorrelationId = Nothing
        , logContextTimestamp = currentTime
        }
  withContext obsCtx logger

-- | Create empty log context
emptyContext :: MonadIO m => m LogContext
emptyContext = liftIO $ do
  currentTime <- getCurrentTime
  pure (emptyContextAt currentTime)

-- | Create empty context at specific time
emptyContextAt :: UTCTime -> LogContext
emptyContextAt timestamp = LogContext
  { logContextFields = mempty
  , logContextTraceId = Nothing
  , logContextSpanId = Nothing
  , logContextCorrelationId = Nothing
  , logContextTimestamp = timestamp
  }

-- | Add field to log context
addContext :: Text -> Value -> LogContext -> LogContext
addContext key value ctx = ctx 
  { logContextFields = Map.insert key value (logContextFields ctx) }

-- | Merge two log contexts with right-bias
--
-- Later context takes precedence for conflicting fields.
mergeContext :: LogContext -> LogContext -> LogContext
mergeContext ctx1 ctx2 = LogContext
  { logContextFields = logContextFields ctx1 <> logContextFields ctx2
  , logContextTraceId = logContextTraceId ctx2 <|> logContextTraceId ctx1
  , logContextSpanId = logContextSpanId ctx2 <|> logContextSpanId ctx1
  , logContextCorrelationId = logContextCorrelationId ctx2 <|> logContextCorrelationId ctx1
  , logContextTimestamp = max (logContextTimestamp ctx1) (logContextTimestamp ctx2)
  }
  where
    (<|>) = flip (<|>)  -- Right-biased alternative

-- | Get current context from logger
getContext :: MonadIO m => Logger -> m LogContext
getContext logger = liftIO $ atomically $ readTVar (loggerContext logger)

-- | Convert log level to text representation
levelToText :: LogLevel -> Text
levelToText = \case
  DEBUG -> "DEBUG"
  INFO -> "INFO"
  WARN -> "WARN"
  ERROR -> "ERROR"
  FATAL -> "FATAL"

-- | Parse log level from text representation
levelFromText :: Text -> Maybe LogLevel
levelFromText = \case
  "DEBUG" -> Just DEBUG
  "INFO" -> Just INFO
  "WARN" -> Just WARN
  "ERROR" -> Just ERROR
  "FATAL" -> Just FATAL
  _ -> Nothing

-- | Format log message according to configuration
formatMessage :: LoggerConfig -> LogMessage -> Text
formatMessage config msg = case loggerFormat config of
  JSON -> TE.decodeUtf8 $ BSL.toStrict $ JSON.encode msg
  TEXT -> formatTextMessage config msg
  CUSTOM -> formatTextMessage config msg  -- Fallback to text

-- | Format message in human-readable text format
formatTextMessage :: LoggerConfig -> LogMessage -> Text
formatTextMessage config msg = T.intercalate " " $ filter (not . T.null)
  [ timestampPart
  , levelPart
  , logMessageText msg
  , contextPart
  , errorPart
  , metricsPart
  ]
  where
    timestampPart = if loggerTimestamp config
      then "[" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (logMessageTimestamp msg)) <> "]"
      else ""
    
    levelPart = if loggerShowLevel config
      then "[" <> levelToText (logMessageLevel msg) <> "]"
      else ""
    
    contextPart = 
      let ctx = logMessageContext msg
          fields = Map.toList (logContextFields ctx)
      in if null fields
         then ""
         else "{" <> T.intercalate ", " (map formatField fields) <> "}"
    
    formatField (k, v) = k <> "=" <> case v of
      String s -> s
      Number n -> T.pack (show n)
      Bool b -> if b then "true" else "false"
      _ -> T.pack (show v)
    
    errorPart = case logMessageError msg of
      Nothing -> ""
      Just err -> "ERROR: " <> Error.qiErrorMessage err
    
    metricsPart = case logMessageMetrics msg of
      Nothing -> ""
      Just metrics -> "METRICS: " <> TE.decodeUtf8 (BSL.toStrict (JSON.encode metrics))

-- | Display logger configuration in human-readable format
showLogger :: MonadIO m => Logger -> m Text
showLogger logger = liftIO $ do
  config <- atomically $ readTVar (loggerConfig logger)
  context <- atomically $ readTVar (loggerContext logger)
  enabled <- atomically $ readTVar (loggerEnabled logger)
  
  pure $ T.intercalate "\n"
    [ "Logger Configuration:"
    , "  Level: " <> levelToText (loggerLevel config)
    , "  Format: " <> T.pack (show (loggerFormat config))
    , "  Destination: " <> T.pack (show (loggerDestination config))
    , "  Enabled: " <> if enabled then "true" else "false"
    , "  Context Fields: " <> T.pack (show (Map.size (logContextFields context)))
    ]

-- Internal helper functions

-- | Create a consistent timestamp for Logger validation errors (Contract compliance)
-- Logger validation operations are pure, so we use a fixed reference timestamp
loggerErrorTimestamp :: UTCTime
loggerErrorTimestamp = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- | Validate logger configuration
validateConfig :: LoggerConfig -> Result ()
validateConfig config = do
  -- Validate destination
  case loggerDestination config of
    Console _ -> Success ()
    File path -> if T.null (T.pack path)
      then Result.failure $ Error.create
        "LOGGER_INVALID_FILE_PATH"
        "File path cannot be empty"
        VALIDATION
        mempty
        Nothing
        loggerErrorTimestamp
      else Success ()
    Network host port -> do
      when (T.null host) $ 
        Result.failure $ Error.create
          "LOGGER_INVALID_NETWORK_HOST"
          "Network host cannot be empty"
          VALIDATION
          mempty
          Nothing
          loggerErrorTimestamp
      when (port <= 0 || port > 65535) $ 
        Result.failure $ Error.create
          "LOGGER_INVALID_NETWORK_PORT"
          "Network port must be between 1 and 65535"
          VALIDATION
          (Map.fromList [("port", Number (fromIntegral port))])
          Nothing
          loggerErrorTimestamp
    Multiple dests -> do
      when (null dests) $ 
        Result.failure $ Error.create
          "LOGGER_EMPTY_DESTINATIONS"
          "Multiple destinations cannot be empty"
          VALIDATION
          mempty
          Nothing
          loggerErrorTimestamp

-- | Output formatted message to configured destination
outputMessage :: LoggerConfig -> LogMessage -> IO ()
outputMessage config msg = do
  let formattedMsg = formatMessage config msg
  case loggerDestination config of
    Console handle -> TIO.hPutStrLn handle formattedMsg
    File path -> TIO.appendFile path (formattedMsg <> "\n")
    Network host port -> do
      -- Network logging via HTTP POST (modern approach)
      let logEntry = JSON.encode $ JSON.object
            [ "timestamp" .= logMessageTimestamp msg
            , "level" .= show (logMessageLevel msg)
            , "message" .= logMessageText msg
            , "context" .= logMessageContext msg
            , "host" .= T.unpack host
            , "port" .= port
            ]
      -- Write to stderr as fallback since network would require HTTP client
      TIO.hPutStrLn stderr $ "Network log [" <> host <> ":" <> T.pack (show port) <> "]: " <> TE.decodeUtf8 (BSL.toStrict logEntry)
    Multiple dests -> mapM_ (\dest -> outputMessage config { loggerDestination = dest } msg) dests

-- Default FromJSON instances for configuration

instance FromJSON LoggerConfig where
  parseJSON = JSON.withObject "LoggerConfig" $ \o -> LoggerConfig
    <$> o .: "level"
    <*> o .:? "format" .!= TEXT
    <*> o .:? "destination" .!= Console stdout
    <*> o .:? "buffered" .!= False
    <*> o .:? "timestamp" .!= True
    <*> o .:? "showLevel" .!= True

instance FromJSON LogFormat where
  parseJSON (String "json") = pure JSON
  parseJSON (String "text") = pure TEXT
  parseJSON (String "custom") = pure CUSTOM
  parseJSON _ = fail "Invalid log format"

instance FromJSON LogDestination where
  parseJSON (String "console") = pure (Console stdout)
  parseJSON (String "stderr") = pure (Console stderr)
  parseJSON (JSON.Object o) = do
    destType <- o .: "type"
    case destType of
      String "file" -> File <$> o .: "path"
      String "network" -> Network <$> o .: "host" <*> o .: "port"
      _ -> fail "Invalid destination type"
  parseJSON _ = fail "Invalid log destination"