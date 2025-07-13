{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Qi.Core.Cache
  ( -- * Cache Types
    Cache(..)
  , CacheConfig(..)
  , CacheKey
  , CacheValue
  , EvictionPolicy(..)
  , TTL(..)
    -- * Factory Operations
  , createMemory
  , createPersistent
    -- * Core Operations
  , get
  , set
  , has
  , remove
  , clear
  , size
    -- * Advanced Operations
  , getOrSet
  , setMany
  , getMany
  , expire
    -- * Utilities
  , isExpired
  , cleanupExpired
  ) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar, modifyTVar', atomically)
import Control.Exception (try, SomeException)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import GHC.Generics (Generic)

import Qi.Base.Result (Result)
import qualified Qi.Base.Result as R
import Qi.Base.Error (QiError, ErrorCategory(..))
import qualified Qi.Base.Error as E

-- | Cache key type
type CacheKey = T.Text

-- | Cache value type (JSON serializable)
type CacheValue = A.Value

-- | Time-to-live in seconds (Nothing = no expiration)
newtype TTL = TTL { unTTL :: Maybe Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Cache eviction policy
data EvictionPolicy
  = LRU     -- Least Recently Used
  | FIFO    -- First In, First Out
  | RANDOM  -- Random eviction
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Cache configuration
data CacheConfig = CacheConfig
  { maxSize :: Maybe Int              -- Maximum number of entries
  , defaultTTL :: TTL                -- Default time-to-live
  , evictionPolicy :: EvictionPolicy  -- Eviction strategy
  , persistent :: Bool                -- Whether to persist to disk
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Cache entry with metadata
data CacheEntry = CacheEntry
  { entryValue :: CacheValue
  , entryExpiration :: Maybe UTCTime
  , entryAccessTime :: UTCTime
  , entryInsertTime :: UTCTime
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Cache storage
data CacheStorage = CacheStorage
  { storageEntries :: Map.Map CacheKey CacheEntry
  , storageAccessOrder :: [CacheKey]  -- For LRU tracking
  , storageInsertOrder :: [CacheKey]  -- For FIFO tracking
  } deriving stock (Show, Eq, Generic)

-- | Cache instance
data Cache = Cache
  { cacheConfig :: CacheConfig
  , cacheStorage :: TVar CacheStorage
  , cachePath :: Maybe FilePath  -- For persistent cache
  } deriving stock (Generic)

-- | Create in-memory cache
createMemory :: (MonadIO m) => CacheConfig -> m (Result Cache)
createMemory config = liftIO $ do
  storage <- newTVarIO emptyStorage
  if validateConfig config
    then pure $ R.success $ Cache config storage Nothing
    else do
      timestamp <- getCurrentTime
      pure $ R.failure $ 
        E.create "VALIDATION_ERROR" "Invalid cache configuration" 
                 VALIDATION Map.empty Nothing timestamp

-- | Create cache with disk persistence
createPersistent :: (MonadIO m) => FilePath -> CacheConfig -> m (Result Cache)
createPersistent path config = liftIO $ do
  timestamp <- getCurrentTime
  
  -- Validate config and ensure directory exists
  if not (validateConfig config)
    then pure $ R.failure $ 
      E.create "VALIDATION_ERROR" "Invalid cache configuration" 
               VALIDATION Map.empty Nothing timestamp
    else do
      -- Create directory if needed
      createDirectoryIfMissing True (takeDirectory path)
      
      -- Try to load existing cache
      loadResult <- loadFromDisk path
      storage <- case loadResult of
        R.Success existingStorage -> newTVarIO existingStorage
        R.Failure _ -> newTVarIO emptyStorage
        
      pure $ R.success $ Cache config storage (Just path)

-- | Retrieve value from cache
get :: (MonadIO m) => CacheKey -> Cache -> m (Result CacheValue)
get key cache = liftIO $ do
  timestamp <- getCurrentTime
  
  atomically $ do
    storage <- readTVar (cacheStorage cache)
    case Map.lookup key (storageEntries storage) of
      Nothing -> pure $ R.failure $ 
        E.create "NOT_FOUND" ("Key not found: " <> key) 
                 VALIDATION (Map.singleton "key" (A.String key)) Nothing timestamp
      Just entry -> 
        if isEntryExpired timestamp entry
          then do
            -- Remove expired entry
            let newEntries = Map.delete key (storageEntries storage)
            let newStorage = storage { storageEntries = newEntries }
            writeTVar (cacheStorage cache) newStorage
            pure $ R.failure $ 
              E.create "NOT_FOUND" ("Key expired: " <> key) 
                       VALIDATION (Map.singleton "key" (A.String key)) Nothing timestamp
          else do
            -- Update access time and order for LRU
            let updatedEntry = entry { entryAccessTime = timestamp }
            let newEntries = Map.insert key updatedEntry (storageEntries storage)
            let newAccessOrder = key : filter (/= key) (storageAccessOrder storage)
            let newStorage = storage 
                  { storageEntries = newEntries
                  , storageAccessOrder = newAccessOrder
                  }
            writeTVar (cacheStorage cache) newStorage
            pure $ R.success (entryValue entry)

-- | Store value in cache
set :: (MonadIO m) => CacheKey -> CacheValue -> TTL -> Cache -> m (Result ())
set key value ttl cache = liftIO $ do
  timestamp <- getCurrentTime
  
  let expiration = case unTTL ttl of
        Nothing -> Nothing
        Just seconds -> Just $ addUTCTime (fromIntegral seconds) timestamp
  
  let newEntry = CacheEntry
        { entryValue = value
        , entryExpiration = expiration
        , entryAccessTime = timestamp
        , entryInsertTime = timestamp
        }
  
  atomically $ do
    storage <- readTVar (cacheStorage cache)
    let newEntries = Map.insert key newEntry (storageEntries storage)
    let newAccessOrder = key : filter (/= key) (storageAccessOrder storage)
    let newInsertOrder = if key `Map.member` storageEntries storage
          then storageInsertOrder storage  -- Don't change insert order for updates
          else storageInsertOrder storage ++ [key]
    
    let storageWithEntry = storage
          { storageEntries = newEntries
          , storageAccessOrder = newAccessOrder
          , storageInsertOrder = newInsertOrder
          }
    
    -- Check if eviction is needed
    finalStorage <- case maxSize (cacheConfig cache) of
      Nothing -> pure storageWithEntry
      Just maxSz -> 
        if Map.size newEntries > maxSz
          then evictEntry (cacheConfig cache) storageWithEntry
          else pure storageWithEntry
    
    writeTVar (cacheStorage cache) finalStorage
    
    -- Persist to disk if configured
    case cachePath cache of
      Nothing -> pure ()
      Just path -> liftIO $ void $ saveToDisk path finalStorage
    
    pure $ R.success ()

-- | Check if key exists (without retrieving value)
has :: (MonadIO m) => CacheKey -> Cache -> m Bool
has key cache = liftIO $ do
  timestamp <- getCurrentTime
  atomically $ do
    storage <- readTVar (cacheStorage cache)
    case Map.lookup key (storageEntries storage) of
      Nothing -> pure False
      Just entry -> pure $ not $ isEntryExpired timestamp entry

-- | Remove key from cache
remove :: (MonadIO m) => CacheKey -> Cache -> m Bool
remove key cache = liftIO $ atomically $ do
  storage <- readTVar (cacheStorage cache)
  let existed = key `Map.member` storageEntries storage
  if existed
    then do
      let newEntries = Map.delete key (storageEntries storage)
      let newAccessOrder = filter (/= key) (storageAccessOrder storage)
      let newInsertOrder = filter (/= key) (storageInsertOrder storage)
      let newStorage = storage
            { storageEntries = newEntries
            , storageAccessOrder = newAccessOrder
            , storageInsertOrder = newInsertOrder
            }
      writeTVar (cacheStorage cache) newStorage
      pure True
    else pure False

-- | Remove all entries from cache
clear :: (MonadIO m) => Cache -> m ()
clear cache = liftIO $ atomically $ do
  writeTVar (cacheStorage cache) emptyStorage

-- | Get number of entries in cache
size :: (MonadIO m) => Cache -> m Int
size cache = liftIO $ atomically $ do
  storage <- readTVar (cacheStorage cache)
  pure $ Map.size (storageEntries storage)

-- | Get value or compute and cache if missing
getOrSet :: (MonadIO m) => CacheKey -> IO (Result CacheValue) -> TTL -> Cache -> m (Result CacheValue)
getOrSet key factory ttl cache = liftIO $ do
  existingResult <- get key cache
  case existingResult of
    R.Success value -> pure $ R.success value
    R.Failure _ -> do
      -- Key missing or expired, call factory
      factoryResult <- factory
      case factoryResult of
        R.Success newValue -> do
          setResult <- set key newValue ttl cache
          case setResult of
            R.Success _ -> pure $ R.success newValue
            R.Failure err -> pure $ R.failure err
        R.Failure err -> pure $ R.failure err

-- | Set multiple key-value pairs
setMany :: (MonadIO m) => Map.Map CacheKey CacheValue -> TTL -> Cache -> m (Result ())
setMany pairs ttl cache = liftIO $ do
  results <- mapM (\(k, v) -> set k v ttl cache) (Map.toList pairs)
  let failures = [err | R.Failure err <- results]
  if null failures
    then pure $ R.success ()
    else do
      timestamp <- getCurrentTime
      pure $ R.failure $ 
        E.create "BATCH_OPERATION_FAILED" "Some set operations failed" 
                 SYSTEM Map.empty Nothing timestamp

-- | Get multiple values by keys
getMany :: (MonadIO m) => [CacheKey] -> Cache -> m (Result (Map.Map CacheKey CacheValue))
getMany keys cache = liftIO $ do
  results <- mapM (`get` cache) keys
  let successes = [(k, v) | (k, R.Success v) <- zip keys results]
  pure $ R.success $ Map.fromList successes

-- | Manually expire a key
expire :: (MonadIO m) => CacheKey -> Cache -> m Bool
expire key cache = remove key cache

-- Internal helper functions

emptyStorage :: CacheStorage
emptyStorage = CacheStorage Map.empty [] []

validateConfig :: CacheConfig -> Bool
validateConfig config = 
  case maxSize config of
    Nothing -> True
    Just sz -> sz > 0

isEntryExpired :: UTCTime -> CacheEntry -> Bool
isEntryExpired currentTime entry = 
  case entryExpiration entry of
    Nothing -> False
    Just expTime -> currentTime >= expTime

evictEntry :: CacheConfig -> CacheStorage -> STM CacheStorage
evictEntry config storage = 
  case evictionPolicy config of
    LRU -> evictLRU storage
    FIFO -> evictFIFO storage  
    RANDOM -> evictLRU storage  -- Fallback to LRU for now

evictLRU :: CacheStorage -> STM CacheStorage
evictLRU storage = 
  case reverse (storageAccessOrder storage) of
    [] -> pure storage
    (oldestKey:_) -> pure $ storage
      { storageEntries = Map.delete oldestKey (storageEntries storage)
      , storageAccessOrder = filter (/= oldestKey) (storageAccessOrder storage)
      , storageInsertOrder = filter (/= oldestKey) (storageInsertOrder storage)
      }

evictFIFO :: CacheStorage -> STM CacheStorage
evictFIFO storage = 
  case storageInsertOrder storage of
    [] -> pure storage
    (oldestKey:restOrder) -> pure $ storage
      { storageEntries = Map.delete oldestKey (storageEntries storage)
      , storageAccessOrder = filter (/= oldestKey) (storageAccessOrder storage)
      , storageInsertOrder = restOrder
      }

cleanupExpired :: (MonadIO m) => Cache -> m Int
cleanupExpired cache = liftIO $ do
  timestamp <- getCurrentTime
  atomically $ do
    storage <- readTVar (cacheStorage cache)
    let entries = storageEntries storage
    let expiredKeys = [k | (k, entry) <- Map.toList entries, isEntryExpired timestamp entry]
    let newEntries = foldr Map.delete entries expiredKeys
    let newStorage = storage
          { storageEntries = newEntries
          , storageAccessOrder = filter (`notElem` expiredKeys) (storageAccessOrder storage)
          , storageInsertOrder = filter (`notElem` expiredKeys) (storageInsertOrder storage)
          }
    writeTVar (cacheStorage cache) newStorage
    pure $ length expiredKeys

-- Persistence functions (simplified)

loadFromDisk :: FilePath -> IO (Result CacheStorage)
loadFromDisk path = do
  timestamp <- getCurrentTime
  exists <- doesFileExist path
  if not exists
    then pure $ R.success emptyStorage
    else do
      result <- try $ TIO.readFile path
      case result of
        Left err -> pure $ R.failure $ 
          E.create "FILESYSTEM_ERROR" ("Cannot read cache file: " <> T.pack (show err)) 
                   FILESYSTEM (Map.singleton "file_path" (A.String $ T.pack path)) Nothing timestamp
        Right content -> 
          case A.eitherDecodeStrict (T.encodeUtf8 content) of
            Left parseErr -> pure $ R.failure $ 
              E.create "PARSING_ERROR" ("Cannot parse cache file: " <> T.pack parseErr) 
                       PARSING Map.empty Nothing timestamp
            Right storage -> pure $ R.success storage

saveToDisk :: FilePath -> CacheStorage -> IO (Result ())
saveToDisk path storage = do
  timestamp <- getCurrentTime
  result <- try $ TIO.writeFile path (T.decodeUtf8 $ A.encode storage)
  case result of
    Left err -> pure $ R.failure $ 
      E.create "FILESYSTEM_ERROR" ("Cannot write cache file: " <> T.pack (show err)) 
               FILESYSTEM (Map.singleton "file_path" (A.String $ T.pack path)) Nothing timestamp
    Right _ -> pure $ R.success ()

void :: Monad m => m a -> m ()
void action = action >> pure ()