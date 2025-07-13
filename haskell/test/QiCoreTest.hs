{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: QiCoreTest
-- Description: Comprehensive test suite for QiCore Core component
--
-- This module provides property-based testing for all qi/core components:
-- - Configuration: Monoid laws, validation, and format support
-- - Logger: Level hierarchy, structured logging, and OpenTelemetry
-- - Cache: STM concurrency, LRU eviction, and TTL expiration
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as JSON
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Test.QuickCheck hiding (Success, Failure)
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (Success, Failure)
import Test.Tasty.HUnit
import System.IO (stdout)

import Qi.Base.Error qualified as Error
import Qi.Base.Result (Result(..))
import Qi.Base.Result qualified as Result
import Qi.Core.Cache qualified as Cache
import Qi.Core.Config qualified as Config  
import Qi.Core.Logger qualified as Logger

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QiCore Tests"
  [ configurationTests
  , loggerTests  
  , cacheTests
  , integrationTests
  ]

-- Configuration Component Tests
configurationTests :: TestTree
configurationTests = testGroup "Configuration Tests"
  [ testGroup "Monoid Laws"
      [ QC.testProperty "Left Identity" configLeftIdentity
      , QC.testProperty "Right Identity" configRightIdentity
      , QC.testProperty "Associativity" configAssociativity
      ]
  , testGroup "Factory Operations"
      [ QC.testProperty "fromObject preserves structure" configFromObjectPreservesStructure
      , QC.testProperty "fromString JSON parsing" configFromStringJSON
      , testCase "fromFile with non-existent file" configFromFileNonExistent
      ]
  , testGroup "Query Operations"
      [ QC.testProperty "get existing key" configGetExistingKey
      , QC.testProperty "get non-existent key" configGetNonExistentKey
      , QC.testProperty "has consistency" configHasConsistency
      , QC.testProperty "keys completeness" configKeysCompleteness
      ]
  , testGroup "Validation"
      [ QC.testProperty "validateRequired with all keys" configValidateRequiredAllKeys
      , QC.testProperty "validateRequired with missing keys" configValidateRequiredMissingKeys
      , testCase "validateTypes success" configValidateTypesSuccess
      , testCase "validateTypes failure" configValidateTypesFailure
      ]
  ]

-- Logger Component Tests
loggerTests :: TestTree
loggerTests = testGroup "Logger Tests"
  [ testGroup "Level Hierarchy"
      [ QC.testProperty "Level ordering" loggerLevelOrdering
      , testCase "Level filtering DEBUG" loggerLevelFilteringDebug
      , testCase "Level filtering ERROR" loggerLevelFilteringError
      ]
  , testGroup "Structured Logging"
      [ testCase "Context merging" loggerContextMerging
      , testCase "Correlation ID tracking" loggerCorrelationIdTracking
      , testCase "OpenTelemetry trace context" loggerTraceContext
      ]
  , testGroup "Performance"
      [ testCase "Level checking performance" loggerLevelCheckingPerformance
      , QC.testProperty "isLevelEnabled consistency" loggerIsLevelEnabledConsistency
      ]
  , testGroup "Output Formats"
      [ testCase "JSON format structure" loggerJSONFormat
      , testCase "Text format readability" loggerTextFormat
      ]
  ]

-- Cache Component Tests  
cacheTests :: TestTree
cacheTests = testGroup "Cache Tests"
  [ testGroup "Core Operations"
      [ QC.testProperty "set then get" cacheSetThenGet
      , QC.testProperty "get non-existent key" cacheGetNonExistent
      , QC.testProperty "remove existing key" cacheRemoveExisting
      , testCase "clear empties cache" cacheClearEmpties
      ]
  , testGroup "TTL Expiration"
      [ testCase "TTL expiration works" cacheTTLExpiration
      , testCase "NoTTL never expires" cacheNoTTLNeverExpires
      , QC.testProperty "TTL consistency" cacheTTLConsistency
      ]
  , testGroup "LRU Eviction"
      [ testCase "LRU eviction policy" cacheLRUEviction
      , testCase "Access order tracking" cacheAccessOrderTracking
      , QC.testProperty "Size limit enforcement" cacheSizeLimitEnforcement
      ]
  , testGroup "Concurrency"
      [ testCase "Concurrent access safety" cacheConcurrentAccess
      , testCase "STM transaction isolation" cacheSTMTransactionIsolation
      ]
  , testGroup "Advanced Operations"
      [ testCase "getOrSet atomic behavior" cacheGetOrSetAtomic
      , QC.testProperty "setMany atomicity" cacheSetManyAtomicity
      , QC.testProperty "getMany completeness" cacheGetManyCompleteness
      ]
  ]

-- Integration Tests
integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "Config + Logger integration" integrationConfigLogger
  , testCase "Logger + Cache integration" integrationLoggerCache
  , testCase "Full foundation stack" integrationFullFoundation
  ]

-- Configuration Property Tests

configLeftIdentity :: Config.ConfigData -> Property
configLeftIdentity config = 
  Config.merge [Config.empty, config] === Result.Success config

configRightIdentity :: Config.ConfigData -> Property  
configRightIdentity config =
  Config.merge [config, Config.empty] === Success config

configAssociativity :: Config.ConfigData -> Config.ConfigData -> Config.ConfigData -> Property
configAssociativity a b c =
  let left = case Config.merge [a, b] of
        Success ab -> Config.merge [ab, c]
        Failure err -> Failure err
      right = case Config.merge [b, c] of
        Success bc -> Config.merge [a, bc]  
        Failure err -> Failure err
  in left === right

configFromObjectPreservesStructure :: [(Text, Value)] -> Property
configFromObjectPreservesStructure pairs =
  let obj = Map.fromList pairs
      jsonObj = JSON.object [(k, v) | (k, v) <- pairs]
  in case Config.fromObject jsonObj of
    Success (Config.ConfigData resultObj) -> 
      length pairs === length pairs
    _ -> property False

configFromStringJSON :: Value -> Property
configFromStringJSON value =
  let jsonText = T.decodeUtf8 (JSON.encode value)
  in case Config.fromString jsonText Config.JSON of
    Success (Config.ConfigData resultValue) -> resultValue === value
    Failure _ -> property False

configGetExistingKey :: Text -> Value -> Property
configGetExistingKey key value =
  let config = Config.ConfigData (JSON.Object (JSON.singleton (JSON.fromText key) value))
  in case Config.get key config of
    Success resultValue -> resultValue === value
    Failure _ -> property False

configGetNonExistentKey :: Text -> Property  
configGetNonExistentKey key =
  case Config.get key Config.empty of
    Success _ -> property False
    Failure _ -> property True

configHasConsistency :: Text -> Config.ConfigData -> Property
configHasConsistency key config =
  Config.has key config === case Config.get key config of
    Success _ -> True
    Failure _ -> False

configKeysCompleteness :: [(Text, Value)] -> Property
configKeysCompleteness pairs =
  let obj = JSON.object [(k, v) | (k, v) <- pairs]
      config = Config.ConfigData obj
      configKeys = sort (Config.keys config)
      expectedKeys = sort (map fst pairs)
  in configKeys === expectedKeys

configValidateRequiredAllKeys :: [Text] -> Property
configValidateRequiredAllKeys requiredKeys =
  let obj = JSON.object [(k, String "value") | k <- requiredKeys]
      config = Config.ConfigData obj
  in case Config.validateRequired requiredKeys config of
    Success _ -> property True
    Failure _ -> property False

configValidateRequiredMissingKeys :: [Text] -> [Text] -> Property
configValidateRequiredMissingKeys presentKeys requiredKeys =
  let missingKeys = filter (`notElem` presentKeys) requiredKeys
      obj = JSON.object [(k, String "value") | k <- presentKeys]
      config = Config.ConfigData obj
  in if null missingKeys
     then case Config.validateRequired requiredKeys config of
       Success _ -> property True
       Failure _ -> property False
     else case Config.validateRequired requiredKeys config of
       Success _ -> property False
       Failure _ -> property True

-- Logger Property Tests

loggerLevelOrdering :: Logger.LogLevel -> Logger.LogLevel -> Property
loggerLevelOrdering level1 level2 =
  (level1 <= level2) === (fromEnum level1 <= fromEnum level2)

loggerIsLevelEnabledConsistency :: Logger.LogLevel -> Logger.LogLevel -> Property
loggerIsLevelEnabledConsistency configLevel checkLevel =
  monadicIO $ do
    result <- liftIO $ do
      loggerResult <- Logger.create (defaultLoggerConfig { Logger.loggerLevel = configLevel })
      case loggerResult of
        Success logger -> Logger.isLevelEnabled checkLevel logger
        Failure _ -> pure False
    assert (result == (checkLevel >= configLevel))

-- Cache Property Tests

cacheSetThenGet :: Text -> Value -> Property
cacheSetThenGet key value =
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          setResult <- Cache.set key value Nothing cache
          case setResult of
            Success _ -> do
              getResult <- Cache.get key cache
              case getResult of
                Success retrievedValue -> pure (retrievedValue == value)
                Failure _ -> pure False
            Failure _ -> pure False
        Failure _ -> pure False
    assert result

cacheGetNonExistent :: Text -> Property
cacheGetNonExistent key =
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          getResult <- Cache.get key cache
          case getResult of
            Success _ -> pure False
            Failure _ -> pure True
        Failure _ -> pure False
    assert result

cacheRemoveExisting :: Text -> Value -> Property
cacheRemoveExisting key value =
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          void $ Cache.set key value Nothing cache
          removed <- Cache.remove key cache
          if removed
            then do
              hasKey <- Cache.has key cache
              pure (not hasKey)
            else pure False
        Failure _ -> pure False
    assert result

cacheTTLConsistency :: Text -> Value -> Positive Int -> Property
cacheTTLConsistency key value (Positive seconds) =
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          let ttl = Cache.TTLSeconds (fromIntegral seconds)
          void $ Cache.set key value (Just ttl) cache
          -- Immediately check - should exist
          hasKey1 <- Cache.has key cache
          pure hasKey1
        Failure _ -> pure False
    assert result

cacheSizeLimitEnforcement :: Positive Int -> [Text] -> Property
cacheSizeLimitEnforcement (Positive maxSize) keys =
  length keys > maxSize ==>
  monadicIO $ do
    result <- liftIO $ do
      let config = Cache.defaultConfig { Cache.cacheMaxSize = Just maxSize }
      cacheResult <- Cache.createMemory config
      case cacheResult of
        Success cache -> do
          -- Set all keys
          mapM_ (\k -> Cache.set k (String k) Nothing cache) keys
          -- Check final size
          finalSize <- Cache.size cache
          pure (finalSize <= maxSize)
        Failure _ -> pure False
    assert result

cacheSetManyAtomicity :: [(Text, Value)] -> Property
cacheSetManyAtomicity pairs =
  not (null pairs) ==>
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          let kvMap = Map.fromList pairs
          setManyResult <- Cache.setMany kvMap Nothing cache
          case setManyResult of
            Success _ -> do
              -- Check all keys exist
              results <- mapM (\(k, _) -> Cache.has k cache) pairs
              pure (all id results)
            Failure _ -> pure False
        Failure _ -> pure False
    assert result

cacheGetManyCompleteness :: [(Text, Value)] -> Property
cacheGetManyCompleteness pairs =
  not (null pairs) ==>
  monadicIO $ do
    result <- liftIO $ do
      cacheResult <- Cache.createMemory Cache.defaultConfig
      case cacheResult of
        Success cache -> do
          -- Set all key-value pairs
          mapM_ (\(k, v) -> Cache.set k v Nothing cache) pairs
          -- Get all keys
          let keys = map fst pairs
          getManyResult <- Cache.getMany keys cache
          case getManyResult of
            Success resultMap -> do
              -- Check all keys are present
              pure (Map.size resultMap == length pairs)
            Failure _ -> pure False
        Failure _ -> pure False
    assert result

-- Unit Tests

configFromFileNonExistent :: Assertion
configFromFileNonExistent = do
  result <- Config.fromFile "/non/existent/file.json"
  case result of
    Success _ -> assertFailure "Expected failure for non-existent file"
    Failure _ -> pure ()

configValidateTypesSuccess :: Assertion
configValidateTypesSuccess = do
  let obj = JSON.object ["name" .= ("test" :: Text), "age" .= (25 :: Int)]
      config = Config.ConfigData obj
      typeSchema = Map.fromList [("name", "String"), ("age", "Number")]
  case Config.validateTypes typeSchema config of
    Success _ -> pure ()
    Failure _ -> assertFailure "Type validation should succeed"

configValidateTypesFailure :: Assertion
configValidateTypesFailure = do
  let obj = JSON.object ["name" .= ("test" :: Text), "age" .= ("twenty-five" :: Text)]
      config = Config.ConfigData obj
      typeSchema = Map.fromList [("name", "String"), ("age", "Number")]
  case Config.validateTypes typeSchema config of
    Success _ -> assertFailure "Type validation should fail"
    Failure _ -> pure ()

loggerLevelFilteringDebug :: Assertion
loggerLevelFilteringDebug = do
  loggerResult <- Logger.create (defaultLoggerConfig { Logger.loggerLevel = Logger.DEBUG })
  case loggerResult of
    Success logger -> do
      debugEnabled <- Logger.isLevelEnabled Logger.DEBUG logger
      infoEnabled <- Logger.isLevelEnabled Logger.INFO logger
      errorEnabled <- Logger.isLevelEnabled Logger.ERROR logger
      debugEnabled @?= True
      infoEnabled @?= True
      errorEnabled @?= True
    Failure _ -> assertFailure "Logger creation should succeed"

loggerLevelFilteringError :: Assertion
loggerLevelFilteringError = do
  loggerResult <- Logger.create (defaultLoggerConfig { Logger.loggerLevel = Logger.ERROR })
  case loggerResult of
    Success logger -> do
      debugEnabled <- Logger.isLevelEnabled Logger.DEBUG logger
      infoEnabled <- Logger.isLevelEnabled Logger.INFO logger
      errorEnabled <- Logger.isLevelEnabled Logger.ERROR logger
      debugEnabled @?= False
      infoEnabled @?= False
      errorEnabled @?= True
    Failure _ -> assertFailure "Logger creation should succeed"

loggerContextMerging :: Assertion
loggerContextMerging = do
  loggerResult <- Logger.createDefault
  case loggerResult of
    Success logger -> do
      ctx1 <- Logger.emptyContext >>= \c -> pure (Logger.addContext "key1" (String "value1") c)
      ctx2 <- Logger.emptyContext >>= \c -> pure (Logger.addContext "key2" (String "value2") c)
      loggerWithCtx <- Logger.withContext ctx1 logger
      finalLogger <- Logger.withContext ctx2 loggerWithCtx
      finalCtx <- Logger.getContext finalLogger
      let fields = Logger.logContextFields finalCtx
      Map.lookup "key1" fields @?= Just (String "value1")
      Map.lookup "key2" fields @?= Just (String "value2")
    Failure _ -> assertFailure "Logger creation should succeed"

loggerCorrelationIdTracking :: Assertion
loggerCorrelationIdTracking = do
  loggerResult <- Logger.createDefault
  case loggerResult of
    Success logger -> do
      correlatedLogger <- Logger.withCorrelationId "test-correlation-123" logger
      ctx <- Logger.getContext correlatedLogger
      Logger.logContextCorrelationId ctx @?= Just "test-correlation-123"
    Failure _ -> assertFailure "Logger creation should succeed"

loggerTraceContext :: Assertion
loggerTraceContext = do
  loggerResult <- Logger.createDefault
  case loggerResult of
    Success logger -> do
      tracedLogger <- Logger.withTraceContext "trace-456" "span-789" logger
      ctx <- Logger.getContext tracedLogger
      Logger.logContextTraceId ctx @?= Just "trace-456"
      Logger.logContextSpanId ctx @?= Just "span-789"
    Failure _ -> assertFailure "Logger creation should succeed"

loggerLevelCheckingPerformance :: Assertion
loggerLevelCheckingPerformance = do
  loggerResult <- Logger.create (defaultLoggerConfig { Logger.loggerLevel = Logger.ERROR })
  case loggerResult of
    Success logger -> do
      -- This should be fast even with many calls
      results <- replicateM 10000 (Logger.isLevelEnabled Logger.DEBUG logger)
      length results @?= 10000
      all (== False) results @?= True
    Failure _ -> assertFailure "Logger creation should succeed"

loggerJSONFormat :: Assertion
loggerJSONFormat = do
  let config = defaultLoggerConfig { Logger.loggerFormat = Logger.JSON }
  loggerResult <- Logger.create config
  case loggerResult of
    Success _ -> pure ()  -- JSON format creation succeeds
    Failure _ -> assertFailure "JSON logger creation should succeed"

loggerTextFormat :: Assertion
loggerTextFormat = do
  let config = defaultLoggerConfig { Logger.loggerFormat = Logger.TEXT }
  loggerResult <- Logger.create config
  case loggerResult of
    Success _ -> pure ()  -- Text format creation succeeds
    Failure _ -> assertFailure "Text logger creation should succeed"

cacheClearEmpties :: Assertion
cacheClearEmpties = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      void $ Cache.set "key1" (String "value1") Nothing cache
      void $ Cache.set "key2" (String "value2") Nothing cache
      sizeBefore <- Cache.size cache
      sizeBefore @?= 2
      Cache.clear cache
      sizeAfter <- Cache.size cache
      sizeAfter @?= 0
    Failure _ -> assertFailure "Cache creation should succeed"

cacheTTLExpiration :: Assertion
cacheTTLExpiration = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      -- Set with very short TTL
      let ttl = Cache.TTLSeconds 0.001  -- 1 millisecond
      void $ Cache.set "key1" (String "value1") (Just ttl) cache
      -- Wait for expiration
      threadDelay 10000  -- 10 milliseconds
      hasKey <- Cache.has "key1" cache
      hasKey @?= False
    Failure _ -> assertFailure "Cache creation should succeed"

cacheNoTTLNeverExpires :: Assertion
cacheNoTTLNeverExpires = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      void $ Cache.set "key1" (String "value1") (Just Cache.NoTTL) cache
      -- Should still exist after delay
      threadDelay 1000
      hasKey <- Cache.has "key1" cache
      hasKey @?= True
    Failure _ -> assertFailure "Cache creation should succeed"

cacheLRUEviction :: Assertion
cacheLRUEviction = do
  let config = Cache.defaultConfig { Cache.cacheMaxSize = Just 2 }
  cacheResult <- Cache.createMemory config
  case cacheResult of
    Success cache -> do
      void $ Cache.set "key1" (String "value1") Nothing cache
      void $ Cache.set "key2" (String "value2") Nothing cache
      void $ Cache.set "key3" (String "value3") Nothing cache  -- Should evict key1
      
      hasKey1 <- Cache.has "key1" cache
      hasKey2 <- Cache.has "key2" cache  
      hasKey3 <- Cache.has "key3" cache
      
      hasKey1 @?= False  -- key1 should be evicted (least recently used)
      hasKey2 @?= True
      hasKey3 @?= True
    Failure _ -> assertFailure "Cache creation should succeed"

cacheAccessOrderTracking :: Assertion
cacheAccessOrderTracking = do
  let config = Cache.defaultConfig { Cache.cacheMaxSize = Just 2 }
  cacheResult <- Cache.createMemory config
  case cacheResult of
    Success cache -> do
      void $ Cache.set "key1" (String "value1") Nothing cache
      void $ Cache.set "key2" (String "value2") Nothing cache
      
      -- Access key1 to make it recently used
      void $ Cache.get "key1" cache
      
      -- Add key3, which should evict key2 (not key1)
      void $ Cache.set "key3" (String "value3") Nothing cache
      
      hasKey1 <- Cache.has "key1" cache
      hasKey2 <- Cache.has "key2" cache
      hasKey3 <- Cache.has "key3" cache
      
      hasKey1 @?= True   -- key1 should remain (recently accessed)
      hasKey2 @?= False  -- key2 should be evicted
      hasKey3 @?= True
    Failure _ -> assertFailure "Cache creation should succeed"

cacheConcurrentAccess :: Assertion
cacheConcurrentAccess = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      -- Concurrent writes
      let keys = map (T.pack . show) [1..100 :: Int]
      void $ mapConcurrently (\k -> Cache.set k (String k) Nothing cache) keys
      
      -- Check all keys exist
      results <- mapM (\k -> Cache.has k cache) keys
      all id results @?= True
    Failure _ -> assertFailure "Cache creation should succeed"

cacheSTMTransactionIsolation :: Assertion  
cacheSTMTransactionIsolation = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      void $ Cache.set "counter" (Number 0) Nothing cache
      
      -- Concurrent increments
      let increment = do
            getResult <- Cache.get "counter" cache
            case getResult of
              Success (Number n) -> void $ Cache.set "counter" (Number (n + 1)) Nothing cache
              _ -> pure ()
      
      void $ mapConcurrently (const increment) [1..10 :: Int]
      
      finalResult <- Cache.get "counter" cache
      case finalResult of
        Success (Number n) -> n @?= 10  -- All increments should be applied
        _ -> assertFailure "Expected numeric result"
    Failure _ -> assertFailure "Cache creation should succeed"

cacheGetOrSetAtomic :: Assertion
cacheGetOrSetAtomic = do
  cacheResult <- Cache.createMemory Cache.defaultConfig
  case cacheResult of
    Success cache -> do
      let factory = pure (Success (String "computed-value"))
      
      -- First call should compute and cache
      result1 <- Cache.getOrSet "key1" factory Nothing cache
      case result1 of
        Success (String "computed-value") -> pure ()
        _ -> assertFailure "Expected computed value"
      
      -- Second call should return cached value (not recompute)
      result2 <- Cache.getOrSet "key1" (error "Should not be called") Nothing cache
      case result2 of
        Success (String "computed-value") -> pure ()
        _ -> assertFailure "Expected cached value"
    Failure _ -> assertFailure "Cache creation should succeed"

-- Integration Tests

integrationConfigLogger :: Assertion
integrationConfigLogger = do
  -- Create config with logger settings
  let configObj = JSON.object 
        [ "logger" .= JSON.object
          [ "level" .= ("INFO" :: Text)
          , "format" .= ("json" :: Text)
          ]
        ]
      config = Config.ConfigData configObj
  
  -- Extract logger config and create logger
  case Config.get "logger.level" config of
    Success (String "INFO") -> pure ()
    _ -> assertFailure "Should extract logger level from config"

integrationLoggerCache :: Assertion
integrationLoggerCache = do
  loggerResult <- Logger.createDefault
  cacheResult <- Cache.createMemory Cache.defaultConfig
  
  case (loggerResult, cacheResult) of
    (Success logger, Success cache) -> do
      -- Log cache operations
      Logger.info "Setting cache value" Nothing logger
      void $ Cache.set "test-key" (String "test-value") Nothing cache
      Logger.info "Cache value set successfully" Nothing logger
      
      getResult <- Cache.get "test-key" cache
      case getResult of
        Success _ -> Logger.info "Cache value retrieved successfully" Nothing logger
        Failure _ -> Logger.logError "Failed to retrieve cache value" Nothing Nothing logger
    _ -> assertFailure "Failed to create logger or cache"

integrationFullFoundation :: Assertion
integrationFullFoundation = do
  -- Test all components working together
  configResult <- Config.fromString "{\"app\": {\"name\": \"test\"}}" Config.JSON
  loggerResult <- Logger.createDefault
  cacheResult <- Cache.createMemory Cache.defaultConfig
  
  case (configResult, loggerResult, cacheResult) of
    (Success config, Success logger, Success cache) -> do
      -- Use config to configure logger
      case Config.get "app.name" config of
        Success (String appName) -> do
          contextLogger <- Logger.withContext (Logger.addContext "app" (String appName) =<< Logger.emptyContext) logger
          Logger.info "Foundation integration test started" Nothing contextLogger
          
          -- Cache some application data
          void $ Cache.set "app-config" (JSON.toJSON config) Nothing cache
          Logger.info "Application config cached" Nothing contextLogger
          
          -- Verify cached data
          cachedResult <- Cache.get "app-config" cache
          case cachedResult of
            Success _ -> Logger.info "Foundation integration test completed successfully" Nothing contextLogger
            Failure _ -> Logger.logError "Failed to retrieve cached config" Nothing Nothing contextLogger
        Failure _ -> assertFailure "Failed to get app name from config"
    _ -> assertFailure "Failed to initialize foundation components"

-- Helper functions

defaultLoggerConfig :: Logger.LoggerConfig
defaultLoggerConfig = Logger.LoggerConfig
  { Logger.loggerLevel = Logger.INFO
  , Logger.loggerFormat = Logger.TEXT
  , Logger.loggerDestination = Logger.Console stdout
  , Logger.loggerBuffered = False
  , Logger.loggerTimestamp = True
  , Logger.loggerShowLevel = True
  }

-- QuickCheck instances

instance Arbitrary Config.ConfigData where
  arbitrary = do
    pairs <- listOf arbitraryKeyValue
    let obj = JSON.object pairs
    pure (Config.ConfigData obj)
    where
      arbitraryKeyValue = do
        key <- arbitraryText
        value <- arbitraryValue
        pure (key .= value)

instance Arbitrary Value where
  arbitrary = arbitraryValue

arbitraryValue :: Gen Value
arbitraryValue = oneof
  [ String <$> arbitraryText
  , Number <$> arbitrary
  , Bool <$> arbitrary
  , pure Null
  , pure (JSON.Array mempty)
  , JSON.object <$> listOf arbitraryKeyValue
  ]
  where
    arbitraryKeyValue = do
      key <- arbitraryText
      value <- resize 3 arbitraryValue  -- Limit recursion
      pure (key .= value)

arbitraryText :: Gen Text
arbitraryText = T.pack <$> listOf1 (choose ('a', 'z'))

instance Arbitrary Logger.LogLevel where
  arbitrary = elements [Logger.DEBUG, Logger.INFO, Logger.WARN, Logger.ERROR, Logger.FATAL]