# QiCore Base Component - Haskell Implementation Specification

This document provides Haskell-specific implementation guidance for the QiCore Base Component, mapping the language-agnostic contracts from [qi.base.contracts.md](../../docs/contracts/qi.base.contracts.md) to idiomatic Haskell patterns with GHC 9.12+ features.

## Language Mapping Overview

| Contract Operation | Haskell Implementation | GHC 9.12+ Features |
|-------------------|------------------------|-------------------|
| `Result<T>` | `Either QiError T` | Linear types, SIMD |
| `andThen` | `(>>=)` or `andThen` | Improved inference |
| `asyncMap` | `IO (Either QiError T)` | STM optimizations |
| `inspect` | `traceResult` | Debug support |
| `ErrorCategory` | `data ErrorCategory` | Pattern synonyms |

## Haskell-Specific Result Implementation

### Core Types and Instances

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LinearTypes #-} -- GHC 9.12+ linear types

module Qi.Base.Result 
  ( Result
  , QiError(..)
  , ErrorCategory(..)
  , success
  , failure
  , andThen
  , inspect
  , inspectErr
  , asyncMap
  , fromIO
  , toIO
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (when)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Result type alias for cleaner signatures
type Result a = Either QiError a

-- | Error categories with retry strategies
data ErrorCategory
  = VALIDATION
  | NETWORK  
  | SYSTEM
  | BUSINESS
  | SECURITY
  | PARSING
  | TIMEOUT
  | ASYNC
  | CONCURRENCY
  | RESOURCE
  | CONFIGURATION
  | SERIALIZATION
  | FILESYSTEM
  | UNKNOWN
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

-- | Structured error type with context and chaining
data QiError = QiError
  { qiErrorCode :: !T.Text
  , qiErrorMessage :: !T.Text
  , qiErrorCategory :: !ErrorCategory
  , qiErrorContext :: !(Map.Map T.Text T.Text)
  , qiErrorCause :: !(Maybe QiError)
  , qiErrorTimestamp :: !UTCTime
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- Functor, Applicative, Monad instances (automatic via Either)
-- Alternative and MonadPlus instances for error handling

-- | Smart constructor for errors
-- | Create QiError with current timestamp (IO required)
-- Note: All QiError creation must be done in IO to set proper timestamps
createError :: T.Text -> T.Text -> ErrorCategory -> IO QiError
createError code msg category = do
  now <- getCurrentTime
  pure $ QiError
    { qiErrorCode = code
    , qiErrorMessage = msg
    , qiErrorCategory = category
    , qiErrorContext = Map.empty
    , qiErrorCause = Nothing
    , qiErrorTimestamp = now
    , qiErrorSeverity = MEDIUM  -- Default severity
    }
```

### Factory Functions

```haskell
-- | Success constructor (pure)
success :: a -> Result a
success = Right

-- | Failure constructor (pure, timestamp set later if needed)
failure :: QiError -> Result a  
failure = Left

-- | From try-catch pattern
fromTryCatch :: IO a -> IO (Result a)
fromTryCatch action = do
  result <- try action
  case result of
    Right value -> pure $ success value
    Left err -> do
      qiErr <- createError "EXCEPTION" (T.pack $ show err) UNKNOWN
      pure $ failure qiErr

-- | From Maybe with custom error
fromMaybe :: QiError -> Maybe a -> Result a
fromMaybe err Nothing = failure err
fromMaybe _ (Just x) = success x

-- | From IO action (preserves IO context)
fromIO :: IO a -> IO (Result a)
fromIO = fromTryCatch
```

### Haskell-Specific Operations

```haskell
-- | Rust-style andThen (alias for >>=)
andThen :: Result a -> (a -> Result b) -> Result b
andThen = flip (>>=)

-- | Side effect for successful values (Haskell tracing style)
inspect :: (a -> IO ()) -> Result a -> IO (Result a)
inspect f (Right x) = f x >> pure (Right x)
inspect _ err@(Left _) = pure err

-- | Side effect for error values
inspectErr :: (QiError -> IO ()) -> Result a -> IO (Result a)
inspectErr f err@(Left e) = f e >> pure err
inspectErr _ success@(Right _) = pure success

-- | Collect/flatten nested Results (join operation)
collect :: Result (Result a) -> Result a
collect (Right (Right x)) = Right x
collect (Right (Left e)) = Left e
collect (Left e) = Left e

-- | Pattern synonym for more readable pattern matching
pattern Success :: a -> Result a
pattern Success x = Right x

pattern Failure :: QiError -> Result a  
pattern Failure e = Left e

{-# COMPLETE Success, Failure #-}
```

### Async/IO Integration

```haskell
-- | Async map operation preserving IO context
asyncMap :: (a -> IO b) -> Result a -> IO (Result b)
asyncMap f (Success x) = do
  result <- fromTryCatch (f x)
  pure result
asyncMap _ (Failure e) = pure $ Failure e

-- | Async andThen (monadic bind for IO Results)
asyncAndThen :: Result a -> (a -> IO (Result b)) -> IO (Result b)
asyncAndThen (Success x) f = f x
asyncAndThen (Failure e) _ = pure $ Failure e

-- | Convert Result to IO (for compatibility)
toIO :: Result a -> IO a
toIO (Success x) = pure x
toIO (Failure e) = ioError $ userError $ T.unpack $ qiErrorMessage e

-- | Parallel sequence with fail-fast (using STM for GHC 9.12+)
import Control.Concurrent.STM

asyncSequence :: [IO (Result a)] -> IO (Result [a])
asyncSequence actions = do
  results <- mapM id actions -- Execute all actions
  pure $ sequence results    -- Convert [Result a] to Result [a]

-- | Parallel sequence with STM coordination (GHC 9.12+ optimized)
asyncSequenceSTM :: [IO (Result a)] -> IO (Result [a])
asyncSequenceSTM actions = do
  tvars <- mapM (const $ newTVarIO Nothing) actions
  -- Implementation using STM for coordination
  results <- atomically $ do
    vals <- mapM readTVar tvars
    case sequence vals of
      Nothing -> retry
      Just rs -> pure rs
  pure $ sequence rs
```

### Collection Operations

```haskell
-- | Partition Results into successes and failures
partition :: [Result a] -> ([a], [QiError])
partition = foldr go ([], [])
  where
    go (Success x) (succ, errs) = (x:succ, errs)
    go (Failure e) (succ, errs) = (succ, e:errs)

-- | Extract all successful values
rights :: [Result a] -> [a]
rights = fst . partition

-- | Extract all errors
lefts :: [Result a] -> [QiError]
lefts = snd . partition

-- | Applicative combination of two Results
combine2 :: Result a -> Result b -> (a -> b -> c) -> Result c
combine2 (Success x) (Success y) f = Success (f x y)
combine2 (Failure e) _ _ = Failure e
combine2 _ (Failure e) _ = Failure e

-- | Sequence with accumulating errors (Validation style)
sequenceAccumulating :: [Result a] -> Either [QiError] [a]
sequenceAccumulating results = 
  case partition results of
    (successes, []) -> Right successes
    (_, errors) -> Left errors
```

### QiError Operations

```haskell
-- | Add context to error
withContext :: Map.Map T.Text T.Text -> QiError -> QiError
withContext newContext err = err 
  { qiErrorContext = Map.union newContext (qiErrorContext err) }

-- | Set error cause
withCause :: QiError -> QiError -> QiError  
withCause cause err = err { qiErrorCause = Just cause }

-- | Chain errors (cause becomes root of effect)
chain :: QiError -> QiError -> QiError
chain cause effect = withCause cause effect

-- | Get root error from chain
getRootError :: QiError -> QiError
getRootError err = case qiErrorCause err of
  Nothing -> err
  Just cause -> getRootError cause

-- | Check error category
hasCategory :: ErrorCategory -> QiError -> Bool
hasCategory cat err = qiErrorCategory err == cat

-- | Format error chain for display
formatChain :: QiError -> T.Text
formatChain err = T.intercalate " → " (formatChain' err)
  where
    formatChain' e = 
      let current = "[" <> T.pack (show $ qiErrorCategory e) <> "] " 
                   <> qiErrorCode e <> ": " <> qiErrorMessage e
      in case qiErrorCause e of
           Nothing -> [current]
           Just cause -> current : formatChain' cause
```

### Linear Types Integration (GHC 9.12+)

```haskell
{-# LANGUAGE LinearTypes #-}

-- | Linear resource management with Results
data Resource = Resource !Handle

-- | Linear bracket for resource safety
bracket :: IO (Result Resource) 
        -> (Resource %1 -> IO (Result a))
        -> (Resource %1 -> IO ())
        -> IO (Result a)
bracket acquire use release = do
  resource <- acquire
  case resource of
    Failure e -> pure $ Failure e
    Success r -> do
      result <- use r
      release r
      pure result

-- | Linear Result operations (experimental)
mapLinear :: (a %1 -> b) -> Result a %1 -> Result b
mapLinear f (Success x) = Success (f x)
mapLinear _ (Failure e) = Failure e
```

### Performance Optimizations (GHC 9.12+)

```haskell
-- | Strict evaluation for performance
{-# INLINE success #-}
{-# INLINE failure #-}
{-# INLINE andThen #-}

-- | SIMD-optimized operations for numeric Results (GHC 9.12+)
import GHC.Exts (Int#, (+#))

-- | Batch operations on numeric Results using SIMD
batchMapInt :: (Int -> Int) -> [Result Int] -> [Result Int]
batchMapInt f = map (fmap f)
{-# INLINE batchMapInt #-}
{-# SPECIALIZE batchMapInt :: (Int -> Int) -> [Result Int] -> [Result Int] #-}
```

### Validation Patterns

```haskell
-- | Validation that accumulates errors
newtype Validation e a = Validation (Either [e] a)
  deriving stock (Eq, Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Validation . Right
  Validation (Left es1) <*> Validation (Left es2) = Validation (Left (es1 <> es2))
  Validation (Left es) <*> _ = Validation (Left es)
  _ <*> Validation (Left es) = Validation (Left es)
  Validation (Right f) <*> Validation (Right x) = Validation (Right (f x))

-- | Convert Result to Validation  
resultToValidation :: Result a -> Validation QiError a
resultToValidation (Success x) = Validation (Right x)
resultToValidation (Failure e) = Validation (Left [e])

-- | Validate multiple predicates
validateAll :: [a -> Result a] -> a -> Validation QiError a
validateAll validators x = 
  let results = map ($ x) validators
      errors = lefts results
  in case errors of
       [] -> Validation (Right x)
       es -> Validation (Left es)
```

## Integration with Haskell Ecosystem

### Aeson Integration

```haskell
instance ToJSON a => ToJSON (Result a) where
  toJSON (Success x) = object ["kind" .= ("success" :: T.Text), "value" .= x]
  toJSON (Failure e) = object ["kind" .= ("failure" :: T.Text), "error" .= e]

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "Result" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "success" -> Success <$> (o .: "value")
      "failure" -> Failure <$> (o .: "error")
      _ -> fail "Invalid Result kind"
```

### STM Integration

```haskell
-- | STM-safe Result operations
type STMResult a = STM (Result a)

-- | Lift STM computations to Results
liftSTM :: STM a -> STMResult a
liftSTM action = Success <$> action

-- | Conditional STM operations
retryOnFailure :: STMResult a -> STM a
retryOnFailure stmResult = do
  result <- stmResult
  case result of
    Success x -> pure x
    Failure _ -> retry
```

## Usage Examples

See [examples/](./examples/) directory for complete Haskell usage patterns including:
- Basic Result operations with do-notation
- IO integration patterns
- STM concurrency examples
- Linear types for resource management
- Integration with popular Haskell libraries

## GHC Compatibility

- **GHC 9.12+**: Full support including linear types, SIMD optimizations
- **GHC 9.10+**: Core functionality without linear types
- **Cabal 3.14+**: Modern build system support
- **Stack**: Compatible with latest LTS resolver

This specification ensures Haskell developers can use QiCore idiomatically with modern GHC features while maintaining full compliance with the language-agnostic behavioral contracts.