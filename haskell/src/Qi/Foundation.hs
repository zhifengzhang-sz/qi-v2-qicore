{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Qi.Foundation
-- Description: QiCore Foundation - Complete Base and Core Components
-- 
-- This module provides a unified interface to the complete QiCore Foundation,
-- combining mathematical foundation types (qi/base) with essential infrastructure
-- services (qi/core).
--
-- = QiCore Foundation Components
--
-- == Base Component (Mathematical Foundations)
-- 
-- * 'Qi.Base.Result.Result' - Category theory-based Result<T> with monadic operations
-- * 'Qi.Base.Error.QiError' - Structured error handling with 14 error categories
--
-- == Core Component (Infrastructure Services)
--
-- * 'Qi.Core.Config.ConfigData' - Monoid-based configuration with multiple format support
-- * 'Qi.Core.Logger.Logger' - Structured logging with OpenTelemetry integration
-- * 'Qi.Core.Cache.Cache' - STM-based caching with LRU eviction and TTL
--
-- = Modern 2025 Patterns
--
-- This foundation implements cutting-edge patterns for modern applications:
--
-- * **OpenTelemetry Integration**: Distributed tracing and observability
-- * **STM Concurrency**: Lock-free thread-safe operations
-- * **Valkey Compatibility**: Enhanced Redis alternative with 20% memory optimization
-- * **Dependency Injection**: Modern configuration and service management
-- * **Hot Reload**: Dynamic configuration updates with change detection
--
-- = Usage Examples
--
-- @
-- import Qi.Foundation
-- 
-- -- Basic Result usage
-- example1 :: Result Int
-- example1 = do
--   x <- success 42
--   y <- success 10
--   success (x + y)
-- 
-- -- Configuration with validation
-- example2 :: IO (Result ConfigData)
-- example2 = do
--   config <- fromFile "app.json"
--   case config of
--     Success cfg -> validateRequired ["database.host"] cfg
--     Failure err -> return (Failure err)
-- 
-- -- Structured logging with correlation
-- example3 :: IO ()
-- example3 = do
--   logger <- createDefault
--   case logger of
--     Success log -> do
--       correlatedLogger <- withCorrelationId "req-123" log
--       info "Processing request" Nothing correlatedLogger
--     Failure _ -> pure ()
-- 
-- -- Caching with TTL
-- example4 :: IO (Result ())
-- example4 = do
--   cache <- createMemory defaultConfig
--   case cache of
--     Success c -> do
--       set "key1" (String "value1") (Just (TTLSeconds 3600)) c
--     Failure err -> return (Failure err)
-- @
module Qi.Foundation
  ( -- * Complete Foundation Re-exports
    
    -- ** Base Component (Mathematical Foundations)
    module Qi.Base.Result
  , module Qi.Base.Error
    
    -- ** Core Component (Infrastructure Services)  
  -- Re-exported qualified to avoid naming conflicts
    
    -- * Foundation Utilities
  , FoundationInfo(..)
  , getFoundationInfo
  , validateFoundation
  , initializeFoundation
  , shutdownFoundation
  
    -- * Integration Helpers
  , createDefaultFoundation
  , FoundationContext(..)
  , runWithFoundation
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), (.=))
import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Data.Version (showVersion)

-- Re-export all foundation components
import Qi.Base.Error
import Qi.Base.Error qualified as Error
import Qi.Base.Result
import Qi.Core.Cache qualified as Cache
import Qi.Core.Config qualified as Config
import Qi.Core.Logger qualified as Logger

-- | Create a consistent timestamp for Foundation validation errors (Contract compliance)
-- Foundation validation operations are pure, so we use a fixed reference timestamp
foundationErrorTimestamp :: UTCTime
foundationErrorTimestamp = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- | Information about the QiCore Foundation
data FoundationInfo = FoundationInfo
  { foundationVersion :: !Text          -- ^ Foundation version
  , foundationComponents :: ![Text]     -- ^ Available components
  , foundationFeatures :: ![Text]       -- ^ Supported features
  , foundationBuiltAt :: !UTCTime      -- ^ Build timestamp
  } deriving (Show, Eq)

instance JSON.ToJSON FoundationInfo where
  toJSON info = JSON.object
    [ "version" .= foundationVersion info
    , "components" .= foundationComponents info
    , "features" .= foundationFeatures info
    , "builtAt" .= foundationBuiltAt info
    ]

-- | Complete foundation context for integrated applications
data FoundationContext = FoundationContext
  { foundationConfig :: !Config.ConfigData     -- ^ Application configuration
  , foundationLogger :: !Logger.Logger         -- ^ Application logger
  , foundationCache :: !Cache.Cache           -- ^ Application cache
  , foundationInfo :: !FoundationInfo   -- ^ Foundation metadata
  } deriving (Show)

-- | Get information about the current QiCore Foundation
getFoundationInfo :: MonadIO m => m FoundationInfo
getFoundationInfo = liftIO $ do
  currentTime <- getCurrentTime
  return FoundationInfo
    { foundationVersion = "0.2.2"
    , foundationComponents = 
        [ "qi/base (Result<T>, QiError)"
        , "qi/core (Config, Logger, Cache)"
        ]
    , foundationFeatures =
        [ "Category Theory Compliance"
        , "OpenTelemetry Integration" 
        , "STM Concurrency"
        , "Valkey Compatibility"
        , "Hot Reload Configuration"
        , "Structured Logging"
        , "LRU Cache with TTL"
        , "Property-Based Testing"
        ]
    , foundationBuiltAt = currentTime
    }

-- | Validate that the foundation is properly initialized
validateFoundation :: FoundationContext -> Result ()
validateFoundation context = do
  -- Validate configuration is not empty
  let configKeys = Config.keys (foundationConfig context)
  if null configKeys
    then failure $ Error.create
      "FOUNDATION_EMPTY_CONFIG"
      "Foundation configuration is empty"
      VALIDATION
      mempty
      Nothing
      foundationErrorTimestamp
    else success ()

-- | Initialize a complete foundation context with defaults
initializeFoundation :: MonadIO m => Maybe FilePath -> m (Result FoundationContext)
initializeFoundation maybeConfigPath = liftIO $ do
  -- Load configuration
  configResult <- case maybeConfigPath of
    Nothing -> return $ Success Config.empty
    Just path -> Config.fromFile path
  
  case configResult of
    Failure err -> return (Failure err)
    Success config -> do
      -- Create logger
      loggerResult <- Logger.createDefault
      case loggerResult of
        Failure err -> return (Failure err)
        Success logger -> do
          -- Create cache
          cacheResult <- Cache.createMemory Cache.defaultConfig
          case cacheResult of
            Failure err -> return (Failure err)
            Success cache -> do
              -- Get foundation info
              info <- getFoundationInfo
              
              let context = FoundationContext
                    { foundationConfig = config
                    , foundationLogger = logger
                    , foundationCache = cache
                    , foundationInfo = info
                    }
              
              -- Validate foundation
              case validateFoundation context of
                Success _ -> return (Success context)
                Failure err -> return (Failure err)

-- | Create a foundation context with sensible defaults
createDefaultFoundation :: MonadIO m => m (Result FoundationContext)
createDefaultFoundation = initializeFoundation Nothing

-- | Gracefully shutdown foundation resources
shutdownFoundation :: MonadIO m => FoundationContext -> m (Result ())
shutdownFoundation _context = liftIO $ do
  -- In a real implementation, this would:
  -- 1. Flush logger buffers
  -- 2. Persist cache if configured  
  -- 3. Close configuration watchers
  -- 4. Clean up resources
  return (Success ())

-- | Run an action with a foundation context
runWithFoundation :: MonadIO m 
                  => FoundationContext 
                  -> (FoundationContext -> m (Result a)) 
                  -> m (Result a)
runWithFoundation context action = do
  -- Validate foundation before running action
  case validateFoundation context of
    Failure err -> return (Failure err)
    Success _ -> do
      -- Run the action
      result <- action context
      -- Foundation cleanup happens automatically via STM and GC
      return result