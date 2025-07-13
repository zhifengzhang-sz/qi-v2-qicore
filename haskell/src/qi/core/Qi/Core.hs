{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | QiCore v4.0 Core Component
--
-- This module provides essential infrastructure services for configuration,
-- logging, and caching. All operations use Result<T> for error handling
-- and follow the behavioral contracts defined in qi.v4.core.contracts.md
--
-- Dependencies:
--   - Base Component: Uses Result<T> and QiError throughout
--
-- Component Guarantees:
--   - Independent Services: Config, Logger, and Cache can be used independently
--   - Consistent Error Handling: All operations return Result<T>
--   - Async-Aware: File I/O operations are properly async
--   - Performance: Operations meet language-tier performance targets
--   - Resource Management: Proper cleanup for persistent resources

module Qi.Core
  ( -- * Configuration
    module Qi.Core.Config
    -- * Logging  
  , module Qi.Core.Logger
    -- * Caching
  , module Qi.Core.Cache
    -- * Core Component Creation
  , CoreContainer(..)
  , createCoreContainer
  , createCoreContainerWithDefaults
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import System.IO (stdout)

import Qi.Base.Result (Result)
import qualified Qi.Base.Result as R
import Qi.Base.Error (QiError, ErrorCategory(..))
import qualified Qi.Base.Error as E

import Qi.Core.Config
import Qi.Core.Logger  
import Qi.Core.Cache

-- | Core component container holding config, logger, and cache
data CoreContainer = CoreContainer
  { coreConfig :: ConfigData
  , coreLogger :: Logger
  , coreCache :: Cache
  } deriving stock (Generic)

-- | Create complete core container with configuration
createCoreContainer :: (MonadIO m) => FilePath -> m (Result CoreContainer)
createCoreContainer configPath = liftIO $ do
  -- Load configuration
  configResult <- fromFile configPath
  case configResult of
    R.Failure err -> pure $ R.failure err
    R.Success config -> do
      
      -- Create logger from config
      let loggerConfigResult = extractLoggerConfig config
      case loggerConfigResult of
        R.Failure err -> pure $ R.failure err
        R.Success loggerConfig -> do
          loggerResult <- create loggerConfig
          case loggerResult of
            R.Failure err -> pure $ R.failure err
            R.Success logger -> do
              
              -- Create cache from config  
              let cacheConfigResult = extractCacheConfig config
              case cacheConfigResult of
                R.Failure err -> pure $ R.failure err
                R.Success cacheConfig -> do
                  cacheResult <- if persistent cacheConfig
                    then createPersistent "/tmp/qicore.cache" cacheConfig
                    else createMemory cacheConfig
                  case cacheResult of
                    R.Failure err -> do
                      warn "Cache initialization failed, continuing with memory cache" 
                           (Just $ Map.singleton "error" (A.String $ E.toString err)) logger
                      fallbackCacheResult <- createMemory defaultCacheConfig
                      case fallbackCacheResult of
                        R.Failure fallbackErr -> pure $ R.failure fallbackErr
                        R.Success fallbackCache -> 
                          pure $ R.success $ CoreContainer config logger fallbackCache
                    R.Success cache -> 
                      pure $ R.success $ CoreContainer config logger cache

-- | Create core container with sensible defaults
createCoreContainerWithDefaults :: (MonadIO m) => m (Result CoreContainer)
createCoreContainerWithDefaults = liftIO $ do
  -- Create default config
  configResult <- empty
  case configResult of
    R.Failure err -> pure $ R.failure err
    R.Success config -> do
      
      -- Create default logger
      loggerResult <- createDefault
      case loggerResult of
        R.Failure err -> pure $ R.failure err
        R.Success logger -> do
          
          -- Create default cache
          cacheResult <- createMemory defaultCacheConfig
          case cacheResult of
            R.Failure err -> pure $ R.failure err
            R.Success cache -> 
              pure $ R.success $ CoreContainer config logger cache

-- Helper functions for config extraction

extractLoggerConfig :: ConfigData -> Result LoggerConfig
extractLoggerConfig config = 
  let levelResult = getWithDefault "logging.level" (ConfigString "INFO") config
      formatResult = getWithDefault "logging.format" (ConfigString "TEXT") config
      destinationResult = getWithDefault "logging.destination" (ConfigString "console") config
  in R.success $ LoggerConfig
       { logLevel = parseLogLevel levelResult
       , logFormat = parseLogFormat formatResult  
       , logDestination = parseLogDestination destinationResult
       , logContext = Map.empty
       }

extractCacheConfig :: ConfigData -> Result CacheConfig
extractCacheConfig config = 
  let maxSizeResult = case getWithDefault "cache.maxSize" ConfigNull config of
        ConfigNumber n -> Just (round n)
        _ -> Nothing
      defaultTTLResult = case getWithDefault "cache.defaultTTL" ConfigNull config of
        ConfigNumber n -> TTL (Just $ round n)
        _ -> TTL Nothing
      policyResult = getWithDefault "cache.evictionPolicy" (ConfigString "LRU") config
      persistentResult = getWithDefault "cache.persistent" (ConfigBool False) config
  in R.success $ CacheConfig
       { maxSize = maxSizeResult
       , defaultTTL = defaultTTLResult
       , evictionPolicy = parseEvictionPolicy policyResult
       , persistent = parseBoolean persistentResult
       }

-- Default configurations

defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { maxSize = Just 1000
  , defaultTTL = TTL (Just 3600)  -- 1 hour
  , evictionPolicy = LRU
  , persistent = False
  }

-- Parsing helpers

parseLogLevel :: ConfigValue -> LogLevel
parseLogLevel (ConfigString "DEBUG") = DEBUG
parseLogLevel (ConfigString "INFO") = INFO
parseLogLevel (ConfigString "WARN") = WARN
parseLogLevel (ConfigString "ERROR") = ERROR
parseLogLevel (ConfigString "FATAL") = FATAL
parseLogLevel _ = INFO

parseLogFormat :: ConfigValue -> LogFormat
parseLogFormat (ConfigString "JSON") = JSON
parseLogFormat (ConfigString "TEXT") = TEXT
parseLogFormat (ConfigString "CUSTOM") = CUSTOM
parseLogFormat _ = TEXT

parseLogDestination :: ConfigValue -> LogDestination
parseLogDestination (ConfigString "console") = Console stdout
parseLogDestination (ConfigString "stderr") = Console stderr
parseLogDestination (ConfigString path) = File (T.unpack path)
parseLogDestination _ = Console stdout

parseEvictionPolicy :: ConfigValue -> EvictionPolicy  
parseEvictionPolicy (ConfigString "LRU") = LRU
parseEvictionPolicy (ConfigString "FIFO") = FIFO
parseEvictionPolicy (ConfigString "RANDOM") = RANDOM
parseEvictionPolicy _ = LRU

parseBoolean :: ConfigValue -> Bool
parseBoolean (ConfigBool b) = b
parseBoolean _ = False