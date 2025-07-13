{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Base.Error
  ( QiError(..)
  , ErrorCategory(..)
  -- Contract-compliant factory operations
  , createError
  , fromException
  -- Contract-compliant transformation operations  
  , withContext
  , withCause
  -- Contract-compliant query operations
  , getCategory
  , toString
  , toStructuredData
  -- Additional operations
  , aggregate
  , isRetryable
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Control.Exception (Exception, SomeException)

-- | Error categories for classification (contract-compliant)
data ErrorCategory
  = Validation      -- ^ Input validation failures  
  | Network         -- ^ HTTP/network related errors
  | Filesystem      -- ^ File/IO operation failures (contract: FILESYSTEM)
  | Configuration   -- ^ Config loading/parsing errors (contract: CONFIGURATION)  
  | Cache           -- ^ Cache operation failures (contract: CACHE)
  | Timeout         -- ^ Operation timeout errors
  | Permission      -- ^ Access/authorization failures (contract: PERMISSION)
  | Unknown         -- ^ Unclassified errors
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ErrorCategory
instance FromJSON ErrorCategory

-- | Core QiError type matching contract specification
data QiError = QiError
  { code :: Text                    -- ^ Unique error identifier (contract: code)
  , message :: Text                 -- ^ Human-readable description (contract: message)  
  , category :: ErrorCategory       -- ^ Error classification (contract: category)
  , context :: Map Text Unknown     -- ^ Additional context (contract: context as object)
  , cause :: Maybe QiError          -- ^ Underlying error cause (contract: cause)
  , timestamp :: UTCTime            -- ^ When error occurred (contract: timestamp as number)
  } deriving (Show, Eq, Generic)

-- | Type for unknown values in context (contract requirement)
data Unknown
  = UnknownText Text
  | UnknownNumber Double  
  | UnknownBool Bool
  | UnknownNull
  deriving (Show, Eq, Generic)

instance ToJSON QiError
instance FromJSON QiError

instance ToJSON Unknown
instance FromJSON Unknown

-- | Smart constructor for QiError (contract-compliant)
createError :: Text -> Text -> ErrorCategory -> QiError  
createError errorCode errorMessage errorCategory = QiError
  { code = errorCode
  , message = errorMessage  
  , category = errorCategory
  , context = Map.empty
  , cause = Nothing
  , timestamp = undefined -- Will be set by IO in practice
  }

-- | Convert Haskell exception to QiError (contract-compliant)
fromException :: Exception e => e -> QiError
fromException ex = QiError
  { code = "HASKELL_EXCEPTION"
  , message = T.pack $ show ex
  , category = Filesystem  -- Most exceptions are system/filesystem related
  , context = Map.fromList [("exceptionType", UnknownText $ T.pack $ show ex)]
  , cause = Nothing
  , timestamp = undefined -- Will be set by IO
  }

-- | Contract-compliant transformation operations
-- | Add context to error (contract: withContext operation)
withContext :: QiError -> Map Text Unknown -> QiError
withContext err additionalContext = err 
  { context = Map.union additionalContext (context err) }

-- | Add cause to error (contract: withCause operation)  
withCause :: QiError -> QiError -> QiError
withCause err causeError = err { cause = Just causeError }

-- | Contract-compliant query operations
-- | Get error category (contract: getCategory operation)
getCategory :: QiError -> ErrorCategory
getCategory = category

-- | Format error as string (contract: toString operation)
toString :: QiError -> Text
toString err = 
  "[" <> T.pack (show (category err)) <> "] " <> code err <> ": " <> message err <>
  (if Map.null (context err) then "" else " " <> T.pack (show (context err))) <>
  (case cause err of
     Nothing -> ""
     Just c -> " (caused by: " <> code c <> ")")

-- | Convert to structured data (contract: toStructuredData operation)
toStructuredData :: QiError -> Map Text Unknown
toStructuredData err = Map.fromList
  [ ("code", UnknownText $ code err)
  , ("message", UnknownText $ message err)  
  , ("category", UnknownText $ T.pack $ show $ category err)
  , ("context", UnknownText $ T.pack $ show $ context err) -- TODO: Better serialization
  , ("timestamp", UnknownNumber $ fromRational $ toRational $ utcTimeToPOSIXSeconds $ timestamp err)
  , ("cause", case cause err of
      Nothing -> UnknownNull
      Just c -> UnknownText $ T.pack $ show $ toStructuredData c)
  ]

-- | Aggregate multiple errors (contract-compliant)
aggregate :: [QiError] -> QiError
aggregate [] = createError "NO_ERRORS" "Empty error list" Validation
aggregate [err] = err
aggregate errs = QiError
  { code = "AGGREGATED_ERRORS"
  , message = "Multiple errors occurred: " <> T.pack (show (length errs))
  , category = category (head errs) -- Use first error's category
  , context = Map.fromList $ zipWith (\i e -> (T.pack $ show i, UnknownText $ code e)) [0..] errs
  , cause = Just (head errs)
  , timestamp = undefined -- Will be set by IO
  }

-- | Check if error category is retryable (contract-compliant)
isRetryable :: QiError -> Bool
isRetryable err = case category err of
  Network -> True
  Timeout -> True
  Filesystem -> True  -- Renamed from System to match contract
  _ -> False