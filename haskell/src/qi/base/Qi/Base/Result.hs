{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Base.Result
  ( Result(..)
  -- Contract-compliant factory operations
  , success
  , failure
  , fromTryCatch
  , fromAsyncTryCatch
  , fromMaybe
  , fromEither
  -- Contract-compliant query properties  
  , isSuccess
  , isFailure
  , getData
  , getError
  -- Contract-compliant transformation operations
  , mapResult
  , mapError
  , flatMap
  , unwrap
  , unwrapOr
  , match
  , orElse
  -- Collection operations
  , sequence
  , traverse
  ) where

import Prelude hiding (sequence, traverse)
import qualified Prelude as P
import Control.Exception (Exception, try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Qi.Base.Error (QiError, createError, fromException)

-- | Result type - clean algebraic data type
data Result a
  = Success a
  | Failure QiError
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Result a)
instance FromJSON a => FromJSON (Result a)

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure e = Failure e
  Failure e <*> _ = Failure e

instance Monad Result where
  Success a >>= f = f a
  Failure e >>= _ = Failure e

-- | Smart constructors
success :: a -> Result a
success = Success

failure :: QiError -> Result a
failure = Failure

-- | Convert from Either
fromEither :: Either QiError a -> Result a
fromEither (Left e) = Failure e
fromEither (Right a) = Success a

-- | Convert from Maybe with default error
fromMaybe :: QiError -> Maybe a -> Result a
fromMaybe err Nothing = Failure err
fromMaybe _ (Just a) = Success a

-- | Safe exception handling
fromTryCatch :: Exception e => IO a -> IO (Result a)
fromTryCatch action = do
  result <- try action
  case result of
    Right a -> return $ Success a
    Left e -> return $ Failure (fromException e)

-- | Safe async exception handling
fromAsyncTryCatch :: Exception e => IO a -> IO (Result a)
fromAsyncTryCatch = fromTryCatch

-- | Contract-compliant query operations
-- | Check if result represents success (contract: isSuccess property)
isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

-- | Check if result represents failure (contract: isFailure property) 
isFailure :: Result a -> Bool
isFailure = not . isSuccess

-- | Get success data or null (contract: data property)
getData :: Result a -> Maybe a
getData (Success a) = Just a
getData _ = Nothing

-- | Get error or null (contract: error property)
getError :: Result a -> Maybe QiError
getError (Failure e) = Just e
getError _ = Nothing

-- | Transformation operations
mapResult :: (a -> b) -> Result a -> Result b
mapResult = fmap

mapError :: (QiError -> QiError) -> Result a -> Result a
mapError f (Failure e) = Failure (f e)
mapError _ (Success a) = Success a

flatMap :: (a -> Result b) -> Result a -> Result b
flatMap = (=<<)

-- | Contract-required unwrap operations
-- | Extract value or throw error (contract: unwrap operation)
unwrap :: Result a -> a
unwrap (Success a) = a
unwrap (Failure e) = error $ "Result unwrap failed: " <> show e

-- | Extract value or return default (contract: unwrapOr operation)
unwrapOr :: a -> Result a -> a
unwrapOr defaultValue (Success a) = a
unwrapOr defaultValue (Failure _) = defaultValue

-- | Pattern matching (contract: match operation)
match :: (a -> b) -> (QiError -> b) -> Result a -> b
match onSuccess _ (Success a) = onSuccess a
match _ onFailure (Failure e) = onFailure e

-- | Error recovery (contract: orElse operation)
orElse :: (QiError -> Result a) -> Result a -> Result a
orElse _ (Success a) = Success a
orElse alternative (Failure e) = alternative e

-- | Collection operations
sequence :: [Result a] -> Result [a]
sequence [] = Success []
sequence (Success a : rest) = case sequence rest of
  Success as -> Success (a : as)
  Failure e -> Failure e
sequence (Failure e : _) = Failure e

traverse :: (a -> Result b) -> [a] -> Result [b]
traverse f = sequence . map f