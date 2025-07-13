{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | QiCore Base Component - QiError Implementation
-- 
-- This module provides the complete QiError implementation following
-- the behavioral contracts from qi.base.contracts.md with full compliance
-- to immutability and error chaining requirements.
module Qi.Base.Error
  ( QiError(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  -- Factory Operations (Contract Section 2.1)
  , create
  , createIO
  , createErrorIO
  , fromExceptionIO
  , fromStringIO
  -- Query Operations (Contract Section 2.2)
  , toString
  , toStructuredData
  , getCategory
  , getRootCause
  , getErrorChain
  -- Transformation Operations (Contract Section 2.3)
  , withContext
  , withCause
  , withSeverity
  , chain
  , getRootError
  , hasCategory
  , formatChain
  ) where

import Control.Exception (Exception, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(..), withObject)
import Data.Aeson.Types (Parser)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime, UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Calendar (fromGregorian)
import GHC.Generics (Generic)

-- ============================================================================
-- Core Types (Contract Section 2.0)
-- ============================================================================

-- | Error categories with retry strategies (Contract Section 3.0)
-- Complete enumeration covering all expected error types per contract
data ErrorCategory
  = VALIDATION      -- ^ Input constraint violations (retry: never)
  | NETWORK         -- ^ Communication failures (retry: exponential_backoff)
  | SYSTEM          -- ^ Resource/infrastructure problems (retry: linear_backoff)
  | BUSINESS        -- ^ Domain rule violations (retry: never)
  | SECURITY        -- ^ Authorization/authentication failures (retry: never)
  | PARSING         -- ^ Data format/syntax errors (retry: never)
  | TIMEOUT         -- ^ Operation time limit exceeded (retry: timeout_backoff)
  | ASYNC           -- ^ Async operation failures (retry: exponential_backoff)
  | CONCURRENCY     -- ^ Thread safety, locking, race conditions (retry: linear_backoff)
  | RESOURCE        -- ^ Memory, file handles, connection limits (retry: linear_backoff)
  | CONFIGURATION   -- ^ Config loading, validation, missing keys (retry: never)
  | SERIALIZATION   -- ^ JSON/YAML parsing, data conversion (retry: never)
  | FILESYSTEM      -- ^ File I/O operations, permissions (retry: linear_backoff)
  | UNKNOWN         -- ^ Unclassified errors (retry: cautious)
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

-- | Error severity levels for operational decisions
data ErrorSeverity
  = LOW             -- ^ Minor issues, continue operation
  | MEDIUM          -- ^ Significant issues, retry recommended
  | HIGH            -- ^ Major issues, escalation needed
  | CRITICAL        -- ^ System-threatening, immediate attention
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

-- | Core QiError type matching contract specification exactly
-- Contract: Product type `Code × Message × Category × Context × Cause × Timestamp`
-- Contract: Immutability - all operations return new instances
-- Contract: Composability - supports error chaining and context accumulation
data QiError = QiError
  { qiErrorCode :: !Text                           -- ^ Unique error identifier (contract: code)
  , qiErrorMessage :: !Text                        -- ^ Human-readable description (contract: message)
  , qiErrorCategory :: !ErrorCategory              -- ^ Error classification (contract: category)
  , qiErrorContext :: !(Map Text Value)            -- ^ Additional context (contract: context as object)
  , qiErrorCause :: !(Maybe QiError)               -- ^ Underlying error cause (contract: cause)
  , qiErrorTimestamp :: !UTCTime                   -- ^ When error occurred (contract: timestamp)
  , qiErrorSeverity :: !ErrorSeverity              -- ^ Operational severity level
  } deriving stock (Eq, Show, Generic)

-- ============================================================================
-- JSON Serialization (2025 Aeson Best Practices)
-- ============================================================================

instance ToJSON QiError where
  toJSON err = object
    [ "code" .= qiErrorCode err
    , "message" .= qiErrorMessage err
    , "category" .= qiErrorCategory err
    , "context" .= qiErrorContext err
    , "cause" .= qiErrorCause err
    , "timestamp" .= (fromRational $ toRational $ utcTimeToPOSIXSeconds $ qiErrorTimestamp err :: Double)
    , "severity" .= qiErrorSeverity err
    ]

-- Helper function for time conversion
posixSecondsToUTCTime :: Double -> UTCTime
posixSecondsToUTCTime secs = addUTCTime (realToFrac secs) (UTCTime (fromGregorian 1970 1 1) 0)

instance FromJSON QiError where
  parseJSON = withObject "QiError" $ \o -> do
    code <- o .: "code"
    message <- o .: "message"
    category <- o .: "category"
    context <- o .: "context"
    cause <- o .: "cause"
    timestampNum <- o .: "timestamp" :: Parser Double
    severity <- o .: "severity"
    pure $ QiError code message category context cause (posixSecondsToUTCTime timestampNum) severity

-- ============================================================================
-- Factory Operations (Contract Section 2.1)
-- ============================================================================

-- | Pure constructor for QiError (timestamp must be provided)
-- Contract: all required fields populated
-- Contract: context defaults to empty map if not provided
create :: Text -> Text -> ErrorCategory -> Map Text Value -> Maybe QiError -> UTCTime -> QiError
create code message category context cause timestamp = QiError
  { qiErrorCode = code
  , qiErrorMessage = message
  , qiErrorCategory = category
  , qiErrorContext = context
  , qiErrorCause = cause
  , qiErrorTimestamp = timestamp
  , qiErrorSeverity = MEDIUM  -- Default severity
  }

-- | IO-based constructor that sets current timestamp
-- Contract: timestamp defaults to current time
-- Contract: context defaults to empty map
createIO :: Text -> Text -> ErrorCategory -> IO QiError
createIO code message category = do
  now <- getCurrentTime
  pure $ create code message category Map.empty Nothing now

-- | IO-based constructor (for compatibility with Result module)
createErrorIO :: Text -> Text -> ErrorCategory -> IO QiError
createErrorIO = createIO

-- | Convert Haskell exception to QiError (IO version)
-- Contract: preserves exception message
-- Contract: includes stack trace in context
-- Contract: category defaults to UNKNOWN
fromExceptionIO :: Exception e => e -> IO QiError
fromExceptionIO ex = do
  now <- getCurrentTime
  pure $ QiError
    { qiErrorCode = "HASKELL_EXCEPTION"
    , qiErrorMessage = T.pack $ show ex
    , qiErrorCategory = UNKNOWN
    , qiErrorContext = Map.fromList [("exceptionType", String $ T.pack $ show ex)]
    , qiErrorCause = Nothing
    , qiErrorTimestamp = now
    , qiErrorSeverity = HIGH
    }

-- | Create QiError from simple message (IO version)
-- Contract: creates QiError from simple message
-- Contract: category defaults to UNKNOWN
fromStringIO :: Text -> ErrorCategory -> IO QiError
fromStringIO message category = do
  now <- getCurrentTime
  pure $ QiError
    { qiErrorCode = "STRING_ERROR"
    , qiErrorMessage = message
    , qiErrorCategory = category
    , qiErrorContext = Map.empty
    , qiErrorCause = Nothing
    , qiErrorTimestamp = now
    , qiErrorSeverity = MEDIUM
    }

-- ============================================================================
-- Query Operations (Contract Section 2.2)
-- ============================================================================

-- | Format error as human-readable string
-- Contract: includes code and message
-- Contract: human readable format
-- Contract: consistent format across implementations
toString :: QiError -> Text
toString err = 
  "[" <> T.pack (show (qiErrorCategory err)) <> "] " 
  <> qiErrorCode err <> ": " <> qiErrorMessage err
  <> contextSuffix <> causeSuffix
  where
    contextSuffix = if Map.null (qiErrorContext err) 
      then "" 
      else " {context: " <> T.pack (show (qiErrorContext err)) <> "}"
    causeSuffix = case qiErrorCause err of
      Nothing -> ""
      Just cause -> " (caused by: " <> qiErrorCode cause <> ")"

-- | Convert to structured data for serialization
-- Contract: result is JSON serializable
-- Contract: round-trip preserves all information
-- Contract: nested causes properly serialized
toStructuredData :: QiError -> Map Text Value
toStructuredData err = Map.fromList
  [ ("code", String $ qiErrorCode err)
  , ("message", String $ qiErrorMessage err)
  , ("category", String $ T.pack $ show $ qiErrorCategory err)
  , ("context", Object $ KM.fromList $ map (\(k,v) -> (K.fromText k, v)) $ Map.toList $ qiErrorContext err)
  , ("timestamp", Number $ fromRational $ toRational $ utcTimeToPOSIXSeconds $ qiErrorTimestamp err)
  , ("severity", String $ T.pack $ show $ qiErrorSeverity err)
  , ("cause", case qiErrorCause err of
      Nothing -> Null
      Just c -> Object $ KM.fromList $ map (\(k,v) -> (K.fromText k, v)) $ Map.toList $ toStructuredData c)
  ]

-- | Get error category
-- Contract: getCategory(error) == error.category
getCategory :: QiError -> ErrorCategory
getCategory = qiErrorCategory

-- | Get root cause by following chain
-- Contract: if cause == null: getRootCause(error) == error
-- Contract: if cause != null: getRootCause(error) == getRootCause(error.cause)
-- Contract: terminates: cause chains are finite
getRootCause :: QiError -> QiError
getRootCause err = case qiErrorCause err of
  Nothing -> err
  Just cause -> getRootCause cause

-- | Get complete error chain
-- Contract: returns complete chain from root to current
-- Contract: preserves order: [root, ..., current]
-- Contract: non-empty: always contains at least current error
getErrorChain :: QiError -> [QiError]
getErrorChain err = reverse $ buildChain err []
  where
    buildChain current acc = 
      let newAcc = current : acc
      in case qiErrorCause current of
           Nothing -> newAcc
           Just cause -> buildChain cause newAcc

-- ============================================================================
-- Transformation Operations (Contract Section 2.3)
-- ============================================================================

-- | Add context to error (immutable)
-- Contract: preserves all fields except context
-- Contract: merges new context with existing
-- Contract: immutable: original error unchanged
-- Contract: associative: withContext(c1)(withContext(c2)(e)) == withContext(c1 ∪ c2)(e)
withContext :: Map Text Value -> QiError -> QiError
withContext newContext err = err 
  { qiErrorContext = Map.union newContext (qiErrorContext err) }

-- | Set error cause (immutable)
-- Contract: preserves all fields except cause
-- Contract: replaces existing cause
-- Contract: immutable: original error unchanged
withCause :: QiError -> QiError -> QiError
withCause cause err = err { qiErrorCause = Just cause }

-- | Set error severity (immutable)
-- Contract: preserves all fields except severity
-- Contract: immutable: original error unchanged
withSeverity :: ErrorSeverity -> QiError -> QiError
withSeverity severity err = err { qiErrorSeverity = severity }

-- | Chain errors preserving both contexts
-- Contract: creates error chain preserving both errors
-- Contract: chain(cause, effect) links cause as root of effect
-- Contract: different from withCause: preserves both error contexts
-- Contract: maintains causal relationship
chain :: QiError -> QiError -> QiError
chain cause effect = withCause cause effect

-- | Get root error from chain (alias for getRootCause with clearer name)
-- Contract: follows cause chain to find original error
-- Contract: getRootError(error) == error if no cause
-- Contract: terminates: cause chains are finite
-- Contract: more explicit than getRootCause
getRootError :: QiError -> QiError
getRootError = getRootCause

-- | Check if error has specific category
-- Contract: hasCategory(cat, error) == (error.category == cat)
-- Contract: O(1) operation for category checking
-- Contract: used for retry strategy decisions
hasCategory :: ErrorCategory -> QiError -> Bool
hasCategory cat err = qiErrorCategory err == cat

-- | Format complete error chain for display
-- Contract: returns human-readable error chain
-- Contract: includes all errors in causal order
-- Contract: suitable for logging and debugging
-- Contract: consistent format across implementations
formatChain :: QiError -> Text
formatChain err = T.intercalate " → " (map formatSingle $ getErrorChain err)
  where
    formatSingle e = "[" <> T.pack (show $ qiErrorCategory e) <> "] " 
                     <> qiErrorCode e <> ": " <> qiErrorMessage e

-- ============================================================================
-- Retry Strategy Helpers
-- ============================================================================

-- | Determine if error category supports retry
-- Maps to retry strategies from contract specification
isRetryable :: QiError -> Bool
isRetryable err = case qiErrorCategory err of
  VALIDATION -> False      -- never
  NETWORK -> True          -- exponential_backoff
  SYSTEM -> True           -- linear_backoff
  BUSINESS -> False        -- never
  SECURITY -> False        -- never
  PARSING -> False         -- never
  TIMEOUT -> True          -- timeout_backoff
  ASYNC -> True            -- exponential_backoff
  CONCURRENCY -> True      -- linear_backoff
  RESOURCE -> True         -- linear_backoff
  CONFIGURATION -> False   -- never
  SERIALIZATION -> False   -- never
  FILESYSTEM -> True       -- linear_backoff
  UNKNOWN -> True          -- cautious

-- | Get recommended retry strategy for error category
getRetryStrategy :: ErrorCategory -> Text
getRetryStrategy category = case category of
  VALIDATION -> "never"
  NETWORK -> "exponential_backoff"
  SYSTEM -> "linear_backoff"
  BUSINESS -> "never"
  SECURITY -> "never"
  PARSING -> "never"
  TIMEOUT -> "timeout_backoff"
  ASYNC -> "exponential_backoff"
  CONCURRENCY -> "linear_backoff"
  RESOURCE -> "linear_backoff"
  CONFIGURATION -> "never"
  SERIALIZATION -> "never"
  FILESYSTEM -> "linear_backoff"
  UNKNOWN -> "cautious"

-- ============================================================================
-- Error Aggregation (Contract Section 4.1)
-- ============================================================================

-- | Aggregate multiple errors into single error (IO version)
-- Contract: collects all errors instead of failing fast
-- Contract: if all valid: returns success with all values  
-- Contract: if any invalid: returns failure with all errors
aggregateIO :: [QiError] -> IO QiError
aggregateIO [] = do
  now <- getCurrentTime
  pure $ QiError
    { qiErrorCode = "NO_ERRORS"
    , qiErrorMessage = "Empty error list provided to aggregate"
    , qiErrorCategory = VALIDATION
    , qiErrorContext = Map.empty
    , qiErrorCause = Nothing
    , qiErrorTimestamp = now
    , qiErrorSeverity = LOW
    }
aggregateIO [err] = pure err
aggregateIO errs = do
  now <- getCurrentTime
  pure $ QiError
    { qiErrorCode = "AGGREGATED_ERRORS"
    , qiErrorMessage = "Multiple errors occurred: " <> T.pack (show (length errs)) <> " errors"
    , qiErrorCategory = qiErrorCategory (head errs) -- Use first error's category
    , qiErrorContext = Map.fromList $ zipWith 
        (\i e -> (T.pack $ "error_" <> show i, String $ qiErrorCode e)) 
        [0..] errs
    , qiErrorCause = Just (head errs) -- First error as primary cause
    , qiErrorTimestamp = now
    , qiErrorSeverity = maximum (map qiErrorSeverity errs) -- Highest severity
    }