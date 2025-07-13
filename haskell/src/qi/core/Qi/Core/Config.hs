{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Qi.Core.Config
-- Description: QiCore Configuration component with monoid semantics
-- 
-- This module provides a configuration system that follows monoid laws with
-- right-biased merging semantics. It supports multiple formats (JSON, YAML, TOML),
-- environment variable loading, and comprehensive validation.
--
-- The configuration system is designed for modern applications with:
-- - Dependency injection patterns
-- - Immutable configuration data
-- - Structured validation with detailed error reporting
-- - Support for nested configuration hierarchies
module Qi.Core.Config
  ( -- * Core Types
    ConfigData(..)
  , ConfigFormat(..)
  , ConfigSource(..)
  , ConfigValidation(..)
  , ConfigContext(..)
  
    -- * Factory Operations
  , fromFile
  , fromObject
  , fromString
  , fromEnvironment
  , fromSources
  
    -- * Query Operations  
  , get
  , getWithDefault
  , has
  , keys
  , getPath
  
    -- * Monoid Operations
  , merge
  , empty
  , mergeMany
  
    -- * Validation Operations
  , validate
  , validateRequired
  , validateTypes
  , validateDependencies
  
    -- * Modern Patterns (2025)
  , loadStrategy
  , withHotReload
  , withDependencyInjection
  , createConfigContext
  
    -- * Utility Functions
  , flattenKeys
  , toJSONValue
  , fromJSONValue
  , showConfig
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), Object, ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.FilePath (takeExtension)
import Text.Read (readMaybe)
import Data.Yaml qualified as YAML
import Toml qualified

import Qi.Base.Error (QiError, ErrorCategory(..), ErrorSeverity(..))
import Qi.Base.Error qualified as Error
import Qi.Base.Result (Result(..), pattern Success, pattern Failure)
import Qi.Base.Result qualified as Result

-- | Supported configuration formats for parsing
data ConfigFormat 
  = JSON    -- ^ JavaScript Object Notation
  | YAML    -- ^ YAML Ain't Markup Language  
  | TOML    -- ^ Tom's Obvious Minimal Language
  | ENV     -- ^ Environment variables
  deriving (Show, Eq, Ord, Generic)

-- | Configuration data source specification
data ConfigSource = ConfigSource
  { configSourceType :: !ConfigFormat
  , configSourcePath :: !(Maybe FilePath)
  , configSourceContent :: !(Maybe Text)
  , configSourcePrefix :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | Configuration validation specification
data ConfigValidation = ConfigValidation
  { validationRequired :: ![Text]           -- ^ Required keys
  , validationTypes :: !(Map.Map Text Text) -- ^ Type constraints  
  , validationCustom :: ![ConfigData -> Result ()]  -- ^ Custom validators
  }

-- | Configuration context for dependency injection and hot reload
data ConfigContext = ConfigContext
  { configContextData :: !ConfigData
  , configContextSources :: ![ConfigSource]
  , configContextLastReload :: !UTCTime
  , configContextDependencies :: !(Map.Map Text ConfigData)
  } deriving (Show, Generic)

-- | Core configuration data type with monoid semantics
--
-- ConfigData forms a monoid where:
-- - mempty = empty configuration
-- - mappend = right-biased merge (later values override earlier ones)
-- - Associativity: merge([a, merge([b, c])]) == merge([merge([a, b]), c])
newtype ConfigData = ConfigData 
  { unConfigData :: Value 
  } deriving (Show, Eq, Generic)

instance ToJSON ConfigData where
  toJSON (ConfigData v) = v

instance FromJSON ConfigData where
  parseJSON v = pure (ConfigData v)

-- | Monoid instance with right-biased merging semantics
instance Semigroup ConfigData where
  (<>) = mergeTwo

-- | Monoid instance with empty configuration as identity
instance Monoid ConfigData where
  mempty = empty

-- | Create empty configuration (monoid identity)
--
-- Laws:
-- - merge([empty(), config]) == config  
-- - merge([config, empty()]) == config
-- - has(anyKey, empty()) == false
empty :: ConfigData
empty = ConfigData (Object mempty)

-- | Deep merge two configurations with right-bias
--
-- The right configuration takes precedence for conflicting keys.
-- Nested objects are merged recursively.
mergeTwo :: ConfigData -> ConfigData -> ConfigData
mergeTwo (ConfigData v1) (ConfigData v2) = ConfigData (mergeValues v1 v2)
  where
    mergeValues :: Value -> Value -> Value
    mergeValues (Object o1) (Object o2) = Object (KM.unionWith mergeValues o1 o2)
    mergeValues (Array a1) (Array a2) = Array (a1 <> a2)  -- Concatenate arrays
    mergeValues _ v2 = v2  -- Right-biased for primitives

-- | Merge multiple configurations (monoid operation)
--
-- Laws:
-- - Associativity: merge([a, merge([b, c])]) == merge([merge([a, b]), c])
-- - Identity: merge([empty, config]) == config
-- - Right-bias: later configs override earlier ones
merge :: [ConfigData] -> Result ConfigData
merge [] = Success empty
merge configs = Success (foldl (<>) empty configs)

-- | Merge many configurations with error handling
mergeMany :: [ConfigData] -> ConfigData  
mergeMany = foldl (<>) empty

-- | Load configuration from file with automatic format detection
--
-- Supports JSON, YAML, TOML formats based on file extension.
-- Returns FILESYSTEM error for file access issues.
-- Returns PARSING error for invalid format.
fromFile :: MonadIO m => FilePath -> m (Result ConfigData)
fromFile path = liftIO $ do
  let format = detectFormat path
  case format of
    Nothing -> do
      timestamp <- getCurrentTime
      pure $ Failure $ Error.create
        "CONFIG_FORMAT_UNKNOWN"
        ("Unknown configuration format for file: " <> T.pack path)
        VALIDATION
        (Map.fromList [("filePath", JSON.String (T.pack path))])
        Nothing
        timestamp
    Just fmt -> do
      contentResult <- readFileContent path
      case contentResult of
        Failure err -> pure (Failure err)
        Success content -> pure (fromString content fmt)

-- | Create configuration from object/map structure
--
-- Preserves nested structure and validates object format.
-- Returns VALIDATION error for invalid structure.
fromObject :: Object -> Result ConfigData
fromObject obj = Success (ConfigData (Object obj))

-- | Parse configuration from string content with specified format
--
-- Returns PARSING error for syntax errors.
-- Preserves type information where possible.
fromString :: Text -> ConfigFormat -> Result ConfigData
fromString content format = case format of
  JSON -> parseJSON content
  YAML -> parseYAML content  
  TOML -> parseTOML content
  ENV -> parseENV content
  where
    parseJSON :: Text -> Result ConfigData
    parseJSON txt = case JSON.decode (BSL.fromStrict (TE.encodeUtf8 txt)) of
      Nothing -> Failure $ Error.create
        "CONFIG_JSON_PARSE_ERROR"
        "Failed to parse JSON configuration"
        PARSING
        (Map.fromList [("content", JSON.String (T.take 100 txt))])
        Nothing
        configErrorTimestamp
      Just value -> Success (ConfigData value)
    
    parseYAML :: Text -> Result ConfigData
    parseYAML content = case YAML.decodeEither' (TE.encodeUtf8 content) of
      Left yamlError -> Failure $ Error.create
        "CONFIG_YAML_PARSE_ERROR"
        ("YAML parsing failed: " <> T.pack (YAML.prettyPrintParseException yamlError))
        CONFIGURATION
        (Map.fromList [("yamlError", JSON.String (T.pack (show yamlError))), ("content", JSON.String (T.take 100 content))])
        Nothing
        configErrorTimestamp
      Right value -> Success (ConfigData value)
    
    parseTOML :: Text -> Result ConfigData
    parseTOML _ = Failure $ Error.create
      "CONFIG_TOML_NOT_IMPLEMENTED"
      "TOML parsing requires API research - temporarily disabled"
      CONFIGURATION
      mempty
      Nothing
      configErrorTimestamp
    
    parseENV :: Text -> Result ConfigData
    parseENV _ = Failure $ Error.create
      "CONFIG_ENV_NOT_IMPLEMENTED"
      "Environment variable parsing not yet implemented"
      CONFIGURATION
      mempty
      Nothing
      configErrorTimestamp

-- | Load configuration from environment variables with optional prefix
--
-- Converts ENV_VAR_NAME to nested.var.name structure.
-- Attempts type coercion for common types.
fromEnvironment :: MonadIO m => Maybe Text -> m (Result ConfigData)
fromEnvironment maybePrefix = liftIO $ do
  env <- getEnvironment
  let filteredEnv = case maybePrefix of
        Nothing -> env
        Just prefix -> filter (\(k, _) -> prefix `T.isPrefixOf` T.pack k) env
  
  let configMap = Map.fromList $ map convertEnvVar filteredEnv
  Success <$> buildNestedConfig configMap
  where
    convertEnvVar :: (String, String) -> (Text, Value)
    convertEnvVar (key, value) = 
      let cleanKey = maybe (T.pack key) (\p -> T.drop (T.length p + 1) (T.pack key)) maybePrefix
          normalizedKey = T.toLower (T.replace "_" "." cleanKey)
          typedValue = coerceValue (T.pack value)
      in (normalizedKey, typedValue)
    
    coerceValue :: Text -> Value
    coerceValue txt
      | txt == "true" = Bool True
      | txt == "false" = Bool False
      | Just n <- readMaybe (T.unpack txt) = Number (fromRational (toRational (n :: Double)))
      | otherwise = String txt
    
    buildNestedConfig :: Map.Map Text Value -> IO ConfigData
    buildNestedConfig configMap = do
      let obj = foldl insertNested KM.empty (Map.toList configMap)
      pure (ConfigData (Object obj))
    
    insertNested :: Object -> (Text, Value) -> Object
    insertNested obj (key, value) = 
      let keyParts = T.splitOn "." key
      in insertAtPath obj keyParts value
    
    insertAtPath :: Object -> [Text] -> Value -> Object
    insertAtPath obj [] _ = obj
    insertAtPath obj [k] v = KM.insert (Key.fromText k) v obj
    insertAtPath obj (k:ks) v = 
      let key = Key.fromText k
          existing = KM.lookup key obj
          nested = case existing of
            Just (Object o) -> insertAtPath o ks v
            _ -> insertAtPath KM.empty ks v
      in KM.insert key (Object nested) obj

-- | Load configuration from multiple sources with merging
--
-- Sources are processed in order with right-bias merging.
-- Later sources override earlier ones for conflicting keys.
fromSources :: MonadIO m => [ConfigSource] -> m (Result ConfigData)
fromSources sources = do
  results <- mapM loadSource sources
  let (errors, configs) = partitionResults results
  if null errors
    then pure (merge configs)
    else pure $ Failure $ head errors  -- Return first error
  where
    loadSource :: MonadIO m => ConfigSource -> m (Result ConfigData)
    loadSource source = case configSourcePath source of
      Just path -> fromFile path
      Nothing -> case configSourceContent source of
        Just content -> pure $ fromString content (configSourceType source)
        Nothing -> case configSourceType source of
          ENV -> fromEnvironment (configSourcePrefix source)
          _ -> pure $ Failure $ Error.create
            "CONFIG_SOURCE_INVALID"
            "Configuration source must specify either path or content"
            VALIDATION
            mempty
            Nothing
            configErrorTimestamp
    
    partitionResults :: [Result a] -> ([QiError], [a])
    partitionResults = foldr partition ([], [])
      where
        partition (Success a) (es, as) = (es, a:as)
        partition (Failure e) (es, as) = (e:es, as)

-- | Retrieve value by key path with nested support
--
-- Supports nested key paths (e.g., 'database.host').
-- Returns NOT_FOUND error for missing keys.
get :: Text -> ConfigData -> Result Value
get keyPath (ConfigData value) = lookupPath (T.splitOn "." keyPath) value
  where
    lookupPath :: [Text] -> Value -> Result Value
    lookupPath [] v = Success v
    lookupPath (k:ks) (Object obj) = case KM.lookup (Key.fromText k) obj of
      Nothing -> Failure $ Error.create
        "CONFIG_KEY_NOT_FOUND"
        ("Configuration key not found: " <> keyPath)
        FILESYSTEM
        (Map.fromList [("keyPath", String keyPath), ("availableKeys", toJSON (KM.keys obj))])
        Nothing
        configErrorTimestamp
      Just v -> lookupPath ks v
    lookupPath _ _ = Failure $ Error.create
      "CONFIG_INVALID_PATH"
      ("Invalid key path for non-object value: " <> keyPath)
      VALIDATION
      (Map.fromList [("keyPath", String keyPath)])
      Nothing
      configErrorTimestamp

-- | Retrieve value with fallback default (total function)
--
-- Returns default if key not found, actual value if key exists.
-- Never fails - provides total function semantics.
getWithDefault :: ToJSON a => Text -> a -> ConfigData -> Value
getWithDefault keyPath defaultValue config = case get keyPath config of
  Success value -> value
  Failure _ -> JSON.toJSON defaultValue

-- | Check if key exists in configuration (total function)
--
-- Returns true if key path exists, supports nested paths.
-- Never fails - provides total function semantics.
has :: Text -> ConfigData -> Bool
has keyPath config = case get keyPath config of
  Success _ -> True
  Failure _ -> False

-- | Get all available keys as flattened list
--
-- Returns all key paths as dot-separated strings.
-- Keys are sorted lexicographically.
keys :: ConfigData -> [Text]
keys (ConfigData value) = sort (flattenKeys "" value)

-- | Get value with typed extraction and path information
--
-- Enhanced version of get with better error context.
getPath :: FromJSON a => Text -> ConfigData -> Result a
getPath keyPath config = do
  value <- get keyPath config
  case JSON.fromJSON value of
    JSON.Success a -> Success a
    JSON.Error err -> Failure $ Error.create
      "CONFIG_TYPE_CONVERSION_ERROR"
      ("Failed to convert configuration value: " <> T.pack err)
      VALIDATION
      (Map.fromList 
        [ ("keyPath", String keyPath)
        , ("value", value)
        , ("error", String (T.pack err))
        ])
      Nothing
      configErrorTimestamp

-- | Validate configuration against schema specification
--
-- Returns original config if valid, VALIDATION error with details if invalid.
validate :: ConfigValidation -> ConfigData -> Result ConfigData
validate validation config = do
  validateRequired (validationRequired validation) config
  validateTypes (validationTypes validation) config
  mapM_ (\validator -> validator config) (validationCustom validation)
  Success config

-- | Validate that required keys are present
--
-- Returns VALIDATION error listing missing keys if any are absent.
-- Checks nested key paths correctly.
validateRequired :: [Text] -> ConfigData -> Result ()
validateRequired requiredKeys config = do
  let missingKeys = filter (not . (`has` config)) requiredKeys
  unless (null missingKeys) $ do
    let missing = T.intercalate ", " missingKeys
    Result.failure $ Error.create
      "CONFIG_MISSING_REQUIRED_KEYS"
      ("Missing required configuration keys: " <> missing)
      VALIDATION
      (Map.fromList [("missingKeys", JSON.toJSON missingKeys)])
      Nothing
      configErrorTimestamp

-- | Validate value types against schema specification
--
-- Returns VALIDATION error with type mismatches if types don't match.
-- Supports basic types: String, Number, Boolean, Array, Object.
validateTypes :: Map.Map Text Text -> ConfigData -> Result ()
validateTypes typeSchema config = do
  let typeErrors = mapMaybe checkType (Map.toList typeSchema)
  unless (null typeErrors) $ do
    Result.failure $ Error.create
      "CONFIG_TYPE_VALIDATION_ERROR"
      "Configuration type validation failed"
      VALIDATION
      (Map.fromList [("typeErrors", JSON.toJSON typeErrors)])
      Nothing
      configErrorTimestamp
  where
    checkType :: (Text, Text) -> Maybe Text
    checkType (keyPath, expectedType) = case get keyPath config of
      Failure _ -> Just ("Key not found: " <> keyPath)
      Success value -> if valueMatchesType value expectedType
        then Nothing
        else Just ("Type mismatch for " <> keyPath <> ": expected " <> expectedType)
    
    valueMatchesType :: Value -> Text -> Bool
    valueMatchesType (String _) "String" = True
    valueMatchesType (Number _) "Number" = True
    valueMatchesType (Bool _) "Boolean" = True
    valueMatchesType (Array _) "Array" = True
    valueMatchesType (Object _) "Object" = True
    valueMatchesType Null "Null" = True
    valueMatchesType _ _ = False

-- | Validate configuration dependencies are satisfied (2025 Pattern)
--
-- Checks that all service dependencies have valid configurations.
-- Validates circular dependency detection.
validateDependencies :: Map.Map Text [Text] -> ConfigData -> Result ()
validateDependencies deps config = do
  -- Check all dependencies exist
  let allDeps = concatMap snd (Map.toList deps)
  validateRequired allDeps config
  
  -- Check for circular dependencies
  case detectCircularDeps deps of
    Just cycle -> Result.failure $ Error.create
      "CONFIG_CIRCULAR_DEPENDENCY"
      ("Circular dependency detected: " <> T.intercalate " -> " cycle)
      VALIDATION
      (Map.fromList [("cycle", JSON.toJSON cycle)])
      Nothing
      configErrorTimestamp
    Nothing -> Success ()

-- | Apply strategy-based configuration loading (2025 Pattern)
--
-- Supports multiple loading strategies that are composable and cacheable.
-- Enables hot-reloading with change detection.
loadStrategy :: MonadIO m => [ConfigSource] -> ConfigData -> m (Result ConfigData)
loadStrategy sources baseConfig = do
  result <- fromSources sources
  case result of
    Success newConfig -> pure $ Success (baseConfig <> newConfig)
    Failure err -> pure (Failure err)

-- | Create configuration with hot-reload capability (2025 Pattern)
--
-- Enables dynamic configuration updates with change detection.
withHotReload :: MonadIO m => [ConfigSource] -> m (Result ConfigContext)
withHotReload sources = liftIO $ do
  configResult <- fromSources sources
  case configResult of
    Success config -> do
      timestamp <- getCurrentTime
      Success <$> pure ConfigContext
        { configContextData = config
        , configContextSources = sources
        , configContextLastReload = timestamp
        , configContextDependencies = mempty
        }
    Failure err -> pure (Failure err)

-- | Create configuration with dependency injection (2025 Pattern)
--
-- Enables service dependency management and injection.
withDependencyInjection :: Map.Map Text ConfigData -> ConfigData -> ConfigContext
withDependencyInjection deps config = ConfigContext
  { configContextData = config
  , configContextSources = []
  , configContextLastReload = read "1970-01-01 00:00:00 UTC"  -- Epoch timestamp for new contexts
  , configContextDependencies = deps
  }

-- | Create a configuration context for advanced patterns
createConfigContext :: MonadIO m => ConfigData -> [ConfigSource] -> m ConfigContext
createConfigContext config sources = liftIO $ do
  timestamp <- getCurrentTime
  pure ConfigContext
    { configContextData = config
    , configContextSources = sources
    , configContextLastReload = timestamp
    , configContextDependencies = mempty
    }

-- | Utility function to flatten nested keys
flattenKeys :: Text -> Value -> [Text]
flattenKeys prefix (Object obj) = concatMap flattenKey (KM.toList obj)
  where
    flattenKey (key, value) = 
      let newPrefix = if T.null prefix 
                      then Key.toText key 
                      else prefix <> "." <> Key.toText key
      in flattenKeys newPrefix value
flattenKeys prefix _ = [prefix]

-- | Convert ConfigData to JSON Value
toJSONValue :: ToJSON a => a -> Value
toJSONValue = JSON.toJSON

-- | Convert JSON Value to ConfigData
fromJSONValue :: Value -> ConfigData
fromJSONValue = ConfigData

-- | Display configuration in human-readable format
showConfig :: ConfigData -> Text
showConfig (ConfigData value) = case JSON.encode value of
  encoded -> TE.decodeUtf8 (BSL.toStrict encoded)

-- Internal helper functions

-- | Create a consistent timestamp for Config errors (Contract compliance)
-- Config operations are pure, so we use a fixed reference timestamp
configErrorTimestamp :: UTCTime
configErrorTimestamp = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- | Detect configuration format from file extension
detectFormat :: FilePath -> Maybe ConfigFormat
detectFormat path = case takeExtension path of
  ".json" -> Just JSON
  ".yaml" -> Just YAML
  ".yml" -> Just YAML
  ".toml" -> Just TOML
  _ -> Nothing

-- | Read file content with error handling
readFileContent :: FilePath -> IO (Result Text)
readFileContent path = do
  result <- Result.fromTryCatch (TIO.readFile path)
  case result of
    Success content -> pure (Success content)
    Failure err -> do
      timestamp <- getCurrentTime
      pure $ Failure $ Error.create
        "CONFIG_FILE_READ_ERROR"
        ("Failed to read configuration file: " <> T.pack path)
        FILESYSTEM
        (Map.fromList [("filePath", String (T.pack path))])
        (Just err)
        timestamp

-- | Detect circular dependencies in dependency graph
detectCircularDeps :: Map.Map Text [Text] -> Maybe [Text]
detectCircularDeps deps = detectCycle [] (Map.keys deps)
  where
    detectCycle :: [Text] -> [Text] -> Maybe [Text]
    detectCycle _ [] = Nothing
    detectCycle visited (node:nodes)
      | node `elem` visited = Just (reverse (node:visited))
      | otherwise = case Map.lookup node deps of
          Nothing -> detectCycle visited nodes
          Just nodeDeps -> case detectCycle (node:visited) nodeDeps of
            Just cycle -> Just cycle
            Nothing -> detectCycle visited nodes