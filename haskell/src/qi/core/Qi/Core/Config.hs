{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Qi.Core.Config
  ( -- * Configuration Types
    ConfigData(..)
  , ConfigValue(..)
  , ConfigKey
  , ConfigObject
  , ConfigFormat(..)
    -- * Factory Operations
  , fromFile
  , fromObject
  , fromString
  , fromEnvironment
  , empty
    -- * Query Operations
  , get
  , getWithDefault
  , has
  , keys
    -- * Monoid Operations
  , merge
    -- * Validation Operations
  , validate
  , validateRequired
  , validateTypes
    -- * Internal utilities
  , parseByExtension
  ) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import qualified System.Environment as Env
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.FilePath (takeExtension)
import Data.List (isPrefixOf, sort)
import Data.Char (toLower, isUpper)

import Qi.Base.Result (Result)
import qualified Qi.Base.Result as R
import Qi.Base.Error (QiError, ErrorCategory(..))
import qualified Qi.Base.Error as E

-- | Configuration key path (supports nested keys like "database.host")
type ConfigKey = T.Text

-- | Configuration object as nested key-value mapping
type ConfigObject = Map.Map T.Text ConfigValue

-- | Configuration value supporting nested structures
data ConfigValue 
  = ConfigString T.Text
  | ConfigNumber Double
  | ConfigBool Bool
  | ConfigArray [ConfigValue]
  | ConfigObject ConfigObject
  | ConfigNull
  deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Configuration data container with metadata
data ConfigData = ConfigData
  { configData :: ConfigObject
  , configTimestamp :: UTCTime
  , configSource :: T.Text
  } deriving stock (Show, Eq, Generic)

-- | Configuration file format
data ConfigFormat 
  = JSON
  | YAML
  | TOML
  deriving stock (Show, Eq, Enum, Bounded)

-- | Monoid instance for ConfigData (right-biased merge)
instance Semigroup ConfigData where
  c1 <> c2 = ConfigData
    { configData = mergeObjects (configData c1) (configData c2)
    , configTimestamp = max (configTimestamp c1) (configTimestamp c2)
    , configSource = configSource c1 <> " + " <> configSource c2
    }

instance Monoid ConfigData where
  mempty = ConfigData Map.empty (read "1970-01-01 00:00:00 UTC") "empty"

-- | Create empty configuration (monoid identity)
empty :: IO (Result ConfigData)
empty = do
  timestamp <- getCurrentTime
  pure $ R.success $ ConfigData Map.empty timestamp "empty"

-- | Load configuration from file (async)
fromFile :: (MonadIO m) => FilePath -> m (Result ConfigData)
fromFile path = liftIO $ do
  timestamp <- getCurrentTime
  result <- try $ TIO.readFile path
  case result of
    Left err -> pure $ R.failure $ 
      E.create "FILESYSTEM_ERROR" ("Cannot read file: " <> T.pack (show err)) 
               FILESYSTEM (Map.singleton "file_path" (A.String $ T.pack path)) Nothing timestamp
    Right content -> do
      let format = parseByExtension path
      parseResult <- fromString content format
      case parseResult of
        R.Success configData -> 
          pure $ R.success $ configData { configSource = T.pack path }
        R.Failure err -> pure $ R.failure err

-- | Create configuration from object/map
fromObject :: ConfigObject -> IO (Result ConfigData)
fromObject obj = do
  timestamp <- getCurrentTime
  if validateObjectStructure obj
    then pure $ R.success $ ConfigData obj timestamp "object"
    else pure $ R.failure $ 
      E.create "VALIDATION_ERROR" "Invalid object structure" 
               VALIDATION Map.empty Nothing timestamp

-- | Parse configuration from string content
fromString :: T.Text -> ConfigFormat -> IO (Result ConfigValue)
fromString content format = do
  timestamp <- getCurrentTime
  case format of
    JSON -> case A.eitherDecodeStrict (T.encodeUtf8 content) of
      Left err -> pure $ R.failure $ 
        E.create "PARSING_ERROR" ("JSON parse error: " <> T.pack err) 
                 PARSING Map.empty Nothing timestamp
      Right value -> pure $ R.success value
    YAML -> case Y.decodeEither' (T.encodeUtf8 content) of
      Left err -> pure $ R.failure $ 
        E.create "PARSING_ERROR" ("YAML parse error: " <> T.pack (show err)) 
                 PARSING Map.empty Nothing timestamp  
      Right value -> pure $ R.success value
    TOML -> pure $ R.failure $ 
      E.create "NOT_IMPLEMENTED" "TOML parsing not yet implemented" 
               SYSTEM Map.empty Nothing timestamp

-- | Load configuration from environment variables
fromEnvironment :: Maybe T.Text -> IO (Result ConfigData)
fromEnvironment prefixMaybe = do
  timestamp <- getCurrentTime
  env <- Env.getEnvironment
  let filteredEnv = case prefixMaybe of
        Nothing -> env
        Just prefix -> filter (\(k, _) -> T.pack prefix `T.isPrefixOf` T.pack k) env
  let configObj = Map.fromList $ map (convertEnvVar prefixMaybe) filteredEnv
  pure $ R.success $ ConfigData configObj timestamp "environment"

-- | Retrieve value by key path
get :: ConfigKey -> ConfigData -> Result ConfigValue
get key config = 
  case lookupNested (T.splitOn "." key) (configData config) of
    Just value -> R.success value
    Nothing -> R.failure $ 
      E.create "NOT_FOUND" ("Key not found: " <> key) 
               VALIDATION (Map.singleton "key" (A.String key)) Nothing
               (configTimestamp config)

-- | Retrieve value with fallback default
getWithDefault :: ConfigKey -> ConfigValue -> ConfigData -> ConfigValue
getWithDefault key defaultValue config = 
  case get key config of
    R.Success value -> value
    R.Failure _ -> defaultValue

-- | Check if key exists in configuration
has :: ConfigKey -> ConfigData -> Bool
has key config = 
  case lookupNested (T.splitOn "." key) (configData config) of
    Just _ -> True
    Nothing -> False

-- | Get all available keys (flattened, dot-separated)
keys :: ConfigData -> [ConfigKey]
keys config = sort $ flattenKeys "" (configData config)

-- | Merge multiple configurations (monoid operation)
merge :: [ConfigData] -> IO (Result ConfigData)
merge configs = do
  timestamp <- getCurrentTime
  case configs of
    [] -> empty
    _ -> pure $ R.success $ foldl1 (<>) configs

-- | Validate configuration against schema (simplified)
validate :: [(ConfigKey, ConfigValue -> Bool)] -> ConfigData -> Result ConfigData
validate schema config = 
  if all (\(key, validator) -> validateKey key validator config) schema
    then R.success config
    else R.failure $ 
      E.create "VALIDATION_ERROR" "Configuration validation failed" 
               VALIDATION Map.empty Nothing (configTimestamp config)

-- | Validate that required keys are present
validateRequired :: [ConfigKey] -> ConfigData -> Result ConfigData
validateRequired requiredKeys config = 
  let missingKeys = filter (not . flip has config) requiredKeys
  in if null missingKeys
    then R.success config
    else R.failure $ 
      E.create "VALIDATION_ERROR" ("Missing required keys: " <> T.intercalate ", " missingKeys)
               VALIDATION (Map.singleton "missing_keys" (A.Array $ fromList $ map A.String missingKeys)) 
               Nothing (configTimestamp config)
  where
    fromList = Data.Vector.fromList

-- | Validate value types against schema (simplified)
validateTypes :: [(ConfigKey, T.Text)] -> ConfigData -> Result ConfigData
validateTypes typeSchema config = 
  let violations = filter (\(key, expectedType) -> not $ validateType key expectedType config) typeSchema
  in if null violations
    then R.success config
    else R.failure $ 
      E.create "VALIDATION_ERROR" "Type validation failed" 
               VALIDATION Map.empty Nothing (configTimestamp config)

-- Internal helper functions

parseByExtension :: FilePath -> ConfigFormat
parseByExtension path = 
  case map toLower (takeExtension path) of
    ".json" -> JSON
    ".yaml" -> YAML
    ".yml" -> YAML
    ".toml" -> TOML
    _ -> JSON

validateObjectStructure :: ConfigObject -> Bool
validateObjectStructure = const True -- Simplified validation

convertEnvVar :: Maybe T.Text -> (String, String) -> (T.Text, ConfigValue)
convertEnvVar prefixMaybe (key, value) = 
  let cleanKey = case prefixMaybe of
        Nothing -> T.pack key
        Just prefix -> T.drop (T.length prefix + 1) (T.pack key)
      nestedKey = T.replace "_" "." cleanKey
      configValue = tryParseValue (T.pack value)
  in (nestedKey, configValue)

tryParseValue :: T.Text -> ConfigValue
tryParseValue text
  | text == "true" = ConfigBool True
  | text == "false" = ConfigBool False
  | T.all (\c -> c `elem` ("0123456789.-" :: String)) text = 
      case reads (T.unpack text) of
        [(n, "")] -> ConfigNumber n
        _ -> ConfigString text
  | otherwise = ConfigString text

lookupNested :: [T.Text] -> ConfigObject -> Maybe ConfigValue
lookupNested [] _ = Nothing
lookupNested [key] obj = Map.lookup key obj
lookupNested (key:keys) obj = 
  case Map.lookup key obj of
    Just (ConfigObject subObj) -> lookupNested keys subObj
    _ -> Nothing

flattenKeys :: T.Text -> ConfigObject -> [ConfigKey]
flattenKeys prefix obj = concatMap flattenKey (Map.toList obj)
  where
    flattenKey (key, value) = 
      let fullKey = if T.null prefix then key else prefix <> "." <> key
      in case value of
        ConfigObject subObj -> flattenKeys fullKey subObj
        _ -> [fullKey]

mergeObjects :: ConfigObject -> ConfigObject -> ConfigObject
mergeObjects = Map.unionWith mergeValues

mergeValues :: ConfigValue -> ConfigValue -> ConfigValue
mergeValues (ConfigObject obj1) (ConfigObject obj2) = ConfigObject (mergeObjects obj1 obj2)
mergeValues (ConfigArray arr1) (ConfigArray arr2) = ConfigArray (arr1 ++ arr2)
mergeValues _ newValue = newValue

validateKey :: ConfigKey -> (ConfigValue -> Bool) -> ConfigData -> Bool
validateKey key validator config = 
  case get key config of
    R.Success value -> validator value
    R.Failure _ -> False

validateType :: ConfigKey -> T.Text -> ConfigData -> Bool
validateType key expectedType config = 
  case get key config of
    R.Success value -> checkType expectedType value
    R.Failure _ -> False

checkType :: T.Text -> ConfigValue -> Bool
checkType "String" (ConfigString _) = True
checkType "Number" (ConfigNumber _) = True
checkType "Boolean" (ConfigBool _) = True
checkType "Array" (ConfigArray _) = True
checkType "Object" (ConfigObject _) = True
checkType _ _ = False