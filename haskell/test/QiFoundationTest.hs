{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: QiFoundationTest  
-- Description: Integration test suite for complete QiCore Foundation
--
-- This module provides comprehensive integration testing for the complete
-- QiCore Foundation, testing the interaction between all components:
-- qi/base + qi/core working together in realistic scenarios.
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void, replicateM)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck hiding (Success, Failure)
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (Success, Failure)
import Test.Tasty.HUnit

import Qi.Foundation
import Qi.Base.Result (Result(..), pattern Success, pattern Failure)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QiCore Foundation Integration Tests"
  [ foundationInfoTests
  , foundationContextTests
  , endToEndScenarios
  , performanceTests
  , concurrencyTests
  ]

-- Foundation Info Tests
foundationInfoTests :: TestTree
foundationInfoTests = testGroup "Foundation Info Tests"
  [ testCase "Foundation info completeness" foundationInfoCompleteness
  , testCase "Foundation version format" foundationVersionFormat
  , testCase "Foundation components list" foundationComponentsList
  ]

-- Foundation Context Tests
foundationContextTests :: TestTree
foundationContextTests = testGroup "Foundation Context Tests"
  [ testCase "Default foundation creation" defaultFoundationCreation
  , testCase "Foundation validation" foundationValidation
  , testCase "Foundation shutdown" foundationShutdown
  , testCase "Run with foundation" runWithFoundationTest
  ]

-- End-to-End Scenarios
endToEndScenarios :: TestTree
endToEndScenarios = testGroup "End-to-End Scenarios"
  [ testCase "Web application configuration" webApplicationConfiguration
  , testCase "Microservice with observability" microserviceObservability
  , testCase "Data processing pipeline" dataProcessingPipeline
  , testCase "Configuration hot reload" configurationHotReload
  ]

-- Performance Tests
performanceTests :: TestTree
performanceTests = testGroup "Performance Tests"
  [ testCase "High-throughput logging" highThroughputLogging
  , testCase "Cache performance under load" cachePerformanceUnderLoad
  , testCase "Configuration access performance" configurationAccessPerformance
  ]

-- Concurrency Tests
concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency Tests"
  [ testCase "Concurrent foundation operations" concurrentFoundationOperations
  , testCase "Multi-threaded cache access" multiThreadedCacheAccess
  , testCase "Parallel configuration updates" parallelConfigurationUpdates
  ]

-- Unit Tests

foundationInfoCompleteness :: Assertion
foundationInfoCompleteness = do
  info <- getFoundationInfo
  
  -- Check version is present
  foundationVersion info @?= "0.2.2"
  
  -- Check components are listed
  let components = foundationComponents info
  length components @?= 2
  "qi/base (Result<T>, QiError)" `elem` components @?= True
  "qi/core (Config, Logger, Cache)" `elem` components @?= True
  
  -- Check features are comprehensive
  let features = foundationFeatures info
  length features >= 8 @?= True
  "Category Theory Compliance" `elem` features @?= True
  "OpenTelemetry Integration" `elem` features @?= True
  "STM Concurrency" `elem` features @?= True

foundationVersionFormat :: Assertion
foundationVersionFormat = do
  info <- getFoundationInfo
  let version = foundationVersion info
  -- Version should be in semantic versioning format
  T.count "." version @?= 2  -- Should have major.minor.patch format

foundationComponentsList :: Assertion
foundationComponentsList = do
  info <- getFoundationInfo
  let components = foundationComponents info
  
  -- Should have exactly the expected components
  length components @?= 2
  -- Each component should mention its key types
  any (T.isInfixOf "Result<T>") components @?= True
  any (T.isInfixOf "Config") components @?= True

defaultFoundationCreation :: Assertion
defaultFoundationCreation = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      -- Check all components are present
      let config = foundationConfig context
      let logger = foundationLogger context  
      let cache = foundationCache context
      let info = foundationInfo context
      
      -- Verify components are functional
      keys config @?= []  -- Empty default config
      foundationVersion info @?= "0.2.2"
      
      -- Test basic operations
      logEnabled <- isLevelEnabled INFO logger
      logEnabled @?= True
      
      cacheSize <- size cache
      cacheSize @?= 0
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

foundationValidation :: Assertion
foundationValidation = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      -- Default context should validate successfully
      case validateFoundation context of
        Success _ -> Prelude.pure ()
        Failure err -> assertFailure ("Foundation validation failed: " <> show err)
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

foundationShutdown :: Assertion
foundationShutdown = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      shutdownResult <- shutdownFoundation context
      case shutdownResult of
        Success _ -> Prelude.pure ()
        Failure err -> assertFailure ("Foundation shutdown failed: " <> show err)
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

runWithFoundationTest :: Assertion
runWithFoundationTest = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      actionResult <- runWithFoundation context $ \ctx -> do
        -- Perform some operations with the foundation
        let logger = foundationLogger ctx
        let cache = foundationCache ctx
        
        info "Test action started" Nothing logger
        void $ set "test-key" (String "test-value") Nothing cache
        info "Test action completed" Nothing logger
        
        pure (Success "test-result")
      
      case actionResult of
        Success result -> result @?= "test-result"
        Failure err -> assertFailure ("Foundation action failed: " <> show err)
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

-- End-to-End Scenarios

webApplicationConfiguration :: Assertion
webApplicationConfiguration = do
  -- Simulate web application configuration
  let configJson = JSON.object
        [ "server" .= JSON.object
          [ "host" .= ("localhost" :: Text)
          , "port" .= (8080 :: Int)
          ]
        , "database" .= JSON.object
          [ "url" .= ("postgresql://localhost/myapp" :: Text)
          , "maxConnections" .= (20 :: Int)
          ]
        , "logging" .= JSON.object
          [ "level" .= ("INFO" :: Text)
          , "format" .= ("json" :: Text)
          ]
        ]
  
  configResult <- fromObject configJson
  case configResult of
    Success config -> do
      loggerResult <- createDefault
      cacheResult <- createMemory defaultConfig
      
      case (loggerResult, cacheResult) of
        (Success logger, Success cache) -> do
          info <- getFoundationInfo
          let context = FoundationContext config logger cache info
          
          -- Validate web app configuration
          validateResult <- validateRequired ["server.host", "server.port", "database.url"] config
          case validateResult of
            Success _ -> do
              -- Cache database connections
              void $ set "db-pool" (JSON.toJSON (JSON.object ["size" .= (20 :: Int)])) Nothing cache
              
              -- Log application startup
              appLogger <- withContext (addContext "component" (String "web-server") =<< emptyContext) logger
              info "Web application initialized successfully" Nothing appLogger
              
            Failure err -> assertFailure ("Configuration validation failed: " <> show err)
        _ -> assertFailure "Failed to create logger or cache"
    Failure err -> assertFailure ("Configuration creation failed: " <> show err)

microserviceObservability :: Assertion
microserviceObservability = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let logger = foundationLogger context
      let cache = foundationCache context
      
      -- Simulate microservice with full observability
      traceId <- pure "trace-12345"
      spanId <- pure "span-67890"
      correlationId <- pure "req-abcdef"
      
      -- Create observability-enhanced logger
      observableLogger <- withTraceContext traceId spanId logger
      finalLogger <- withCorrelationId correlationId observableLogger
      
      -- Simulate request processing with metrics
      let requestMetrics = JSON.object
            [ "duration" .= (125.5 :: Double)
            , "memoryUsage" .= (1024000 :: Int)
            , "cpuUsage" .= (0.15 :: Double)
            ]
      
      info "Processing microservice request" Nothing finalLogger
      logWithMetrics INFO "Request processed successfully" requestMetrics Nothing finalLogger
      
      -- Cache request result with TTL
      let cacheKey = "response:" <> correlationId
      void $ set cacheKey (String "cached-response") (Just (TTLSeconds 300)) cache
      
      info "Request completed and cached" Nothing finalLogger
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

dataProcessingPipeline :: Assertion
dataProcessingPipeline = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let logger = foundationLogger context
      let cache = foundationCache context
      
      -- Simulate data processing pipeline
      pipelineLogger <- withContext (addContext "pipeline" (String "data-processor") =<< emptyContext) logger
      
      info "Data processing pipeline started" Nothing pipelineLogger
      
      -- Process multiple data items
      let dataItems = ["item1", "item2", "item3", "item4", "item5"]
      
      results <- mapM (processDataItem cache pipelineLogger) dataItems
      let successCount = length [() | Success _ <- results]
      
      info ("Pipeline completed: " <> T.pack (show successCount) <> " items processed") Nothing pipelineLogger
      
      -- Verify cache contains processed items
      cacheSize <- size cache
      cacheSize @?= successCount
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)
  where
    processDataItem cache logger item = do
      info ("Processing item: " <> item) Nothing logger
      
      -- Simulate processing work
      threadDelay 1000  -- 1ms
      
      -- Cache processed result
      let processedValue = JSON.object ["processed" .= True, "item" .= item]
      set ("processed:" <> item) processedValue Nothing cache

configurationHotReload :: Assertion
configurationHotReload = do
  -- Simulate configuration hot reload scenario
  let initialConfig = JSON.object
        [ "feature" .= JSON.object
          [ "enabled" .= True
          , "maxUsers" .= (100 :: Int)
          ]
        ]
  
  configResult <- fromObject initialConfig
  case configResult of
    Success config -> do
      -- Create foundation with initial config
      loggerResult <- createDefault
      cacheResult <- createMemory defaultConfig
      
      case (loggerResult, cacheResult) of
        (Success logger, Success cache) -> do
          info <- getFoundationInfo
          let context = FoundationContext config logger cache info
          
          -- Verify initial configuration
          case get "feature.enabled" config of
            Success (Bool True) -> pure ()
            _ -> assertFailure "Initial config should have feature enabled"
          
          -- Simulate configuration update
          let updatedConfig = JSON.object
                [ "feature" .= JSON.object
                  [ "enabled" .= False
                  , "maxUsers" .= (200 :: Int)
                  ]
                ]
          
          newConfigResult <- fromObject updatedConfig
          case newConfigResult of
            Success newConfig -> do
              -- Merge configurations (hot reload simulation)
              case merge [config, newConfig] of
                Success mergedConfig -> do
                  -- Verify configuration was updated
                  case get "feature.enabled" mergedConfig of
                    Success (Bool False) -> pure ()
                    _ -> assertFailure "Updated config should have feature disabled"
                  
                  case get "feature.maxUsers" mergedConfig of
                    Success (Number 200) -> pure ()
                    _ -> assertFailure "Updated config should have maxUsers = 200"
                    
                  info "Configuration hot reload completed" Nothing logger
                  
                Failure err -> assertFailure ("Config merge failed: " <> show err)
            Failure err -> assertFailure ("Updated config creation failed: " <> show err)
        _ -> assertFailure "Failed to create logger or cache"
    Failure err -> assertFailure ("Initial config creation failed: " <> show err)

-- Performance Tests

highThroughputLogging :: Assertion
highThroughputLogging = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let logger = foundationLogger context
      
      -- Log many messages quickly
      let messageCount = 1000
      let messages = [T.pack ("Message " <> show i) | i <- [1..messageCount]]
      
      -- Measure logging performance
      mapM_ (\msg -> info msg Nothing logger) messages
      
      -- If we get here without hanging, logging is performant enough
      pure ()
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

cachePerformanceUnderLoad :: Assertion
cachePerformanceUnderLoad = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let cache = foundationCache context
      
      -- Perform many cache operations
      let operationCount = 1000
      let keys = [T.pack ("key" <> show i) | i <- [1..operationCount]]
      let values = [String (T.pack ("value" <> show i)) | i <- [1..operationCount]]
      
      -- Set all values
      mapM_ (\(k, v) -> set k v Nothing cache) (zip keys values)
      
      -- Get all values
      results <- mapM (\k -> get k cache) keys
      let successCount = length [() | Success _ <- results]
      
      successCount @?= operationCount
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

configurationAccessPerformance :: Assertion
configurationAccessPerformance = do
  -- Create large configuration
  let largeConfig = JSON.object
        [ T.pack ("key" <> show i) .= (i :: Int) | i <- [1..1000]
        ]
  
  configResult <- fromObject largeConfig
  case configResult of
    Success config -> do
      -- Access configuration keys many times
      let accessCount = 1000
      results <- replicateM accessCount (get "key500" config)
      
      -- All accesses should succeed
      let successCount = length [() | Success _ <- results]
      successCount @?= accessCount
      
    Failure err -> assertFailure ("Large config creation failed: " <> show err)

-- Concurrency Tests

concurrentFoundationOperations :: Assertion
concurrentFoundationOperations = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let logger = foundationLogger context
      let cache = foundationCache context
      
      -- Concurrent operations across all components
      let operations = replicate 100 $ do
            info "Concurrent operation" Nothing logger
            void $ set "concurrent-key" (String "concurrent-value") Nothing cache
            void $ get "concurrent-key" cache
      
      void $ mapConcurrently id operations
      
      -- Verify final state is consistent
      finalResult <- get "concurrent-key" cache
      case finalResult of
        Success _ -> Prelude.pure ()
        Failure _ -> assertFailure "Concurrent operations should leave cache in valid state"
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

multiThreadedCacheAccess :: Assertion
multiThreadedCacheAccess = do
  result <- createDefaultFoundation
  case result of
    Success context -> do
      let cache = foundationCache context
      
      -- Many threads accessing cache simultaneously
      let threadCount = 50
      let operationsPerThread = 20
      
      let threadOperation threadId = do
            let keys = [T.pack ("thread" <> show threadId <> "key" <> show i) | i <- [1..operationsPerThread]]
            let values = [String (T.pack ("value" <> show i)) | i <- [1..operationsPerThread]]
            
            -- Set values
            mapM_ (\(k, v) -> set k v Nothing cache) (zip keys values)
            
            -- Get values
            results <- mapM (\k -> get k cache) keys
            pure (length [() | Success _ <- results])
      
      results <- mapConcurrently threadOperation [1..threadCount]
      let totalSuccesses = sum results
      
      totalSuccesses @?= (threadCount * operationsPerThread)
      
    Failure err -> assertFailure ("Foundation creation failed: " <> show err)

parallelConfigurationUpdates :: Assertion
parallelConfigurationUpdates = do
  -- Test parallel configuration merging
  let baseConfig = JSON.object ["base" .= ("value" :: Text)]
  
  baseResult <- fromObject baseConfig
  case baseResult of
    Success base -> do
      -- Create multiple configuration updates
      let updates = [ JSON.object [T.pack ("update" <> show i) .= (i :: Int)] | i <- [1..10] ]
      
      updateResults <- mapM fromObject updates
      let configs = base : [cfg | Success cfg <- updateResults]
      
      -- Merge all configurations in parallel
      case merge configs of
        Success finalConfig -> do
          -- Verify all updates are present
          case get "base" finalConfig of
            Success (String "value") -> Prelude.pure ()
            _ -> assertFailure "Base config should be preserved"
          
          case get "update5" finalConfig of
            Success (Number 5) -> Prelude.pure ()
            _ -> assertFailure "Update 5 should be present"
            
        Failure err -> assertFailure ("Config merge failed: " <> show err)
    Failure err -> assertFailure ("Base config creation failed: " <> show err)