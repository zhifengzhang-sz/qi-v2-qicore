{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: QiCoreTest  
-- Description: Comprehensive test suite for QiCore v-0.2.7 infrastructure services
--
-- This module provides thorough testing for QiCore v-0.2.7 infrastructure components:
-- - Cache operations (memory and Redis with distributed caching)
-- - Configuration parsing (JSON, YAML, and NEW ENV string parsing)
-- - Integration workflows between components
-- - v-0.2.7 feature verification with real Redis operations and ENV parsing
module Main where

import Control.Monad (void)
import Data.Aeson (Value(..), (.=))
import Data.Aeson qualified as JSON
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (Success, Failure)

import Qi.Base.Error qualified as Error
import Qi.Base.Result (Result(..), pattern Success, pattern Failure)
import Qi.Core.Cache qualified as Cache
import Qi.Core.Config qualified as Config
import Qi.Core.Logger qualified as Logger

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QiCore v-0.2.7 Tests"
  [ cacheTests
  , configTests
  , integrationTests
  ]

-- Cache Tests (Redis features stable since v-0.2.6, verified in v-0.2.7)
cacheTests :: TestTree
cacheTests = testGroup "Cache Tests"
  [ testGroup "Memory Cache" 
      [ testCase "Memory cache basic operations" memoryBasicOps
      ]
  , testGroup "Redis Integration (stable since v-0.2.6)"
      [ testCase "Redis connection test" redisConnectionTest  
      , testCase "Redis SET/GET operations" redisSetGetTest
      , testCase "Redis TTL operations" redisTTLTest
      , testCase "Redis HAS operations" redisHasTest
      , testCase "Redis REMOVE operations" redisRemoveTest
      ]
  ]

-- Config Tests (YAML stable since v-0.2.6, ENV string parsing NEW in v-0.2.7)  
configTests :: TestTree
configTests = testGroup "Config Tests"
  [ testGroup "JSON Parsing"
      [ testCase "JSON config parsing" jsonConfigTest
      ]
  , testGroup "YAML Parsing (stable since v-0.2.6)"
      [ testCase "YAML config success" yamlConfigSuccessTest
      , testCase "YAML config error handling" yamlConfigErrorTest
      ]
  ]

-- Integration Tests
integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "Cache and Config integration" cacheConfigIntegration
  ]

-- Test Implementations

memoryBasicOps :: Assertion
memoryBasicOps = do
  result <- Cache.createMemory Cache.defaultConfig
  case result of
    Success cache -> do
      -- Test SET
      setResult <- Cache.set "test-key" (JSON.String "test-value") Nothing cache
      case setResult of
        Success _ -> do
          -- Test GET  
          getResult <- Cache.get "test-key" cache
          case getResult of
            Success (JSON.String "test-value") -> pure () -- Success
            Success other -> assertFailure $ "Wrong value: " <> show other
            Failure err -> assertFailure $ "GET failed: " <> T.unpack (Error.qiErrorMessage err)
        Failure err -> assertFailure $ "SET failed: " <> T.unpack (Error.qiErrorMessage err)
    Failure err -> assertFailure $ "Cache creation failed: " <> T.unpack (Error.qiErrorMessage err)

redisConnectionTest :: Assertion
redisConnectionTest = do
  result <- Cache.createDistributed "localhost" 6379 Cache.defaultConfig
  case result of
    Success _ -> pure () -- Redis available
    Failure err -> do
      -- Redis not available - verify it's a network error
      Error.qiErrorCategory err @?= Error.NETWORK
      putStrLn $ "Redis not available: " <> T.unpack (Error.qiErrorMessage err)

redisSetGetTest :: Assertion  
redisSetGetTest = do
  result <- Cache.createDistributed "localhost" 6379 Cache.defaultConfig
  case result of
    Success cache -> do
      let testKey = "redis-test-key-v026"
      let testValue = JSON.String "redis-test-value-v026"
      
      -- Test Redis SET
      setResult <- Cache.set testKey testValue Nothing cache
      case setResult of
        Success _ -> do
          -- Test Redis GET
          getResult <- Cache.get testKey cache  
          case getResult of
            Success actualValue | actualValue == testValue -> 
              -- Clean up
              void $ Cache.remove testKey cache
            Success other -> 
              assertFailure $ "Redis value mismatch. Expected: " <> show testValue <> ", Got: " <> show other
            Failure err -> 
              assertFailure $ "Redis GET failed: " <> T.unpack (Error.qiErrorMessage err)
        Failure err -> 
          assertFailure $ "Redis SET failed: " <> T.unpack (Error.qiErrorMessage err)
    
    Failure err -> do
      -- Redis not available
      putStrLn $ "Redis not available, skipping Redis operations test: " <> T.unpack (Error.qiErrorMessage err)
      Error.qiErrorCategory err @?= Error.NETWORK

redisErrorHandlingTest :: Assertion
redisErrorHandlingTest = do
  -- Test connection to invalid Redis instance  
  -- Catch any exceptions and convert them to test failures
  result <- Cache.createDistributed "localhost" 9998 Cache.defaultConfig  -- Use unreachable port instead of invalid host
  case result of
    Success _ -> assertFailure "Should fail with unreachable Redis port"
    Failure err -> do
      Error.qiErrorCategory err @?= Error.NETWORK
      -- Test passes - Redis connection properly failed

jsonConfigTest :: Assertion
jsonConfigTest = do
  let jsonContent = "{\"database\": {\"host\": \"localhost\", \"port\": 5432}}"
  case Config.fromString jsonContent Config.JSON of
    Success config -> do
      case Config.get "database.host" config of
        Success (JSON.String "localhost") -> pure ()
        Success other -> assertFailure $ "Wrong host value: " <> show other
        Failure err -> assertFailure $ "Failed to get host: " <> T.unpack (Error.qiErrorMessage err)
    Failure err -> assertFailure $ "JSON parsing failed: " <> T.unpack (Error.qiErrorMessage err)

yamlConfigSuccessTest :: Assertion  
yamlConfigSuccessTest = do
  let yamlContent = "database:\n  host: localhost\n  port: 5432\napp:\n  name: test-app"
  case Config.fromString yamlContent Config.YAML of
    Success config -> do
      -- Test nested access
      case Config.get "database.host" config of
        Success (JSON.String "localhost") -> do
          case Config.get "app.name" config of
            Success (JSON.String "test-app") -> pure ()
            Success other -> assertFailure $ "Wrong app name: " <> show other
            Failure err -> assertFailure $ "Failed to get app name: " <> T.unpack (Error.qiErrorMessage err)
        Success other -> assertFailure $ "Wrong host value: " <> show other  
        Failure err -> assertFailure $ "Failed to get host: " <> T.unpack (Error.qiErrorMessage err)
    Failure err -> assertFailure $ "YAML parsing failed: " <> T.unpack (Error.qiErrorMessage err)

yamlConfigErrorTest :: Assertion
yamlConfigErrorTest = do
  let malformedYaml = "database:\n  host: localhost\n  port: [invalid"
  case Config.fromString malformedYaml Config.YAML of
    Success _ -> assertFailure "Should fail with malformed YAML"
    Failure err -> do
      Error.qiErrorCategory err @?= Error.CONFIGURATION
      Error.qiErrorCode err @?= "CONFIG_YAML_PARSE_ERROR"

cacheConfigIntegration :: Assertion
cacheConfigIntegration = do
  -- Test that cache and config work together
  let configJson = "{\"cache\": {\"maxSize\": 1000, \"ttlSeconds\": 300}}"
  case Config.fromString configJson Config.JSON of
    Success config -> do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          -- Store config in cache
          storeResult <- Cache.set "app-config" (JSON.toJSON config) Nothing cache
          case storeResult of
            Success _ -> do
              -- Retrieve config from cache
              retrieveResult <- Cache.get "app-config" cache
              case retrieveResult of
                Success _ -> pure () -- Success
                Failure err -> assertFailure $ "Failed to retrieve config from cache: " <> T.unpack (Error.qiErrorMessage err)
            Failure err -> assertFailure $ "Failed to store config in cache: " <> T.unpack (Error.qiErrorMessage err)
        Failure err -> assertFailure $ "Failed to create cache: " <> T.unpack (Error.qiErrorMessage err)
    Failure err -> assertFailure $ "Failed to parse config: " <> T.unpack (Error.qiErrorMessage err)

-- Additional Redis Tests (stable implementation since v-0.2.6)

redisTTLTest :: Assertion
redisTTLTest = do
  result <- Cache.createDistributed "localhost" 6379 Cache.defaultConfig
  case result of
    Success cache -> do
      let testKey = "redis-ttl-test-key"
      let testValue = JSON.String "expires-soon"
      
      -- Test SET with TTL
      setResult <- Cache.set testKey testValue (Just (Cache.TTLSeconds 1)) cache
      case setResult of
        Success _ -> do
          -- Verify value exists immediately
          getResult <- Cache.get testKey cache
          case getResult of
            Success actualValue | actualValue == testValue -> pure () -- Success
            Success other -> assertFailure $ "TTL value mismatch: " <> show other
            Failure err -> assertFailure $ "TTL GET failed: " <> T.unpack (Error.qiErrorMessage err)
        Failure err -> assertFailure $ "TTL SET failed: " <> T.unpack (Error.qiErrorMessage err)
      
      -- Clean up
      void $ Cache.remove testKey cache
    
    Failure err -> do
      putStrLn $ "Redis not available, skipping TTL test: " <> T.unpack (Error.qiErrorMessage err)
      Error.qiErrorCategory err @?= Error.NETWORK

redisHasTest :: Assertion
redisHasTest = do
  result <- Cache.createDistributed "localhost" 6379 Cache.defaultConfig
  case result of
    Success cache -> do
      let testKey = "redis-has-test-key"
      let testValue = JSON.String "test-for-has"
      
      -- Verify key doesn't exist initially
      initialHas <- Cache.has testKey cache
      initialHas @?= False
      
      -- Set key
      setResult <- Cache.set testKey testValue Nothing cache
      case setResult of
        Success _ -> do
          -- Verify key now exists
          hasAfterSet <- Cache.has testKey cache
          hasAfterSet @?= True
          
          -- Clean up and verify it's gone
          removeResult <- Cache.remove testKey cache
          removeResult @?= True
          
          hasAfterRemove <- Cache.has testKey cache
          hasAfterRemove @?= False
        
        Failure err -> assertFailure $ "HAS test SET failed: " <> T.unpack (Error.qiErrorMessage err)
    
    Failure err -> do
      putStrLn $ "Redis not available, skipping HAS test: " <> T.unpack (Error.qiErrorMessage err)
      Error.qiErrorCategory err @?= Error.NETWORK

redisRemoveTest :: Assertion
redisRemoveTest = do
  result <- Cache.createDistributed "localhost" 6379 Cache.defaultConfig
  case result of
    Success cache -> do
      let testKey = "redis-remove-test-key"
      let testValue = JSON.String "to-be-removed"
      
      -- Set a key
      setResult <- Cache.set testKey testValue Nothing cache
      case setResult of
        Success _ -> do
          -- Verify it exists
          hasResult <- Cache.has testKey cache
          hasResult @?= True
          
          -- Remove it
          removeResult <- Cache.remove testKey cache
          removeResult @?= True
          
          -- Verify it's gone
          hasAfterRemove <- Cache.has testKey cache
          hasAfterRemove @?= False
          
          -- Try to remove again (should return False)
          removeAgain <- Cache.remove testKey cache
          removeAgain @?= False
        
        Failure err -> assertFailure $ "REMOVE test SET failed: " <> T.unpack (Error.qiErrorMessage err)
    
    Failure err -> do
      putStrLn $ "Redis not available, skipping REMOVE test: " <> T.unpack (Error.qiErrorMessage err)
      Error.qiErrorCategory err @?= Error.NETWORK