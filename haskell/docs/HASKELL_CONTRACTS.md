# QiCore Haskell Contract Specifications

This document specifies the Haskell-specific implementation details for QiCore Foundation behavioral contracts, based on the language-agnostic specifications in `docs/contracts/qi.base.contracts.md`.

## Overview

The Haskell implementation of QiCore Foundation leverages category theory and modern GHC features to provide mathematically rigorous implementations of Result<T> and QiError types.

## Core Type Definitions

### Result<T> Implementation

```haskell
-- Core type alias using Either for automatic mathematical laws
type Result a = Either QiError a

-- Pattern synonyms for ergonomic API
pattern Success :: a -> Result a
pattern Success a = Right a

pattern Failure :: QiError -> Result a  
pattern Failure e = Left e
```

**Mathematical Foundation:**
- Built on `Either QiError a` to automatically satisfy Functor, Applicative, and Monad laws
- No custom implementations needed - laws are guaranteed by GHC's Either instance
- Type safety ensures all operations preserve mathematical properties

### QiError Implementation

```haskell
data QiError = QiError
  { qiErrorCode      :: !Text
  , qiErrorMessage   :: !Text  
  , qiErrorCategory  :: !ErrorCategory
  , qiErrorContext   :: !(Map Text Value)
  , qiErrorCause     :: !(Maybe QiError)
  , qiErrorTimestamp :: !UTCTime
  , qiErrorSeverity  :: !ErrorSeverity
  } deriving (Eq, Show, Generic)
```

**Key Properties:**
- Strict fields (`!`) for performance and predictable memory usage
- Recursive error chaining through `qiErrorCause`
- JSON serialization via aeson with KeyMap compatibility
- Complete enumeration of ErrorCategory (14 values)

## Haskell-Specific Contract Mappings

### Factory Operations

| Contract | Haskell Implementation | Type Signature |
|----------|----------------------|----------------|
| `success(value)` | `R.success` | `a -> Result a` |
| `failure(error)` | `R.failure` | `QiError -> Result a` |
| `fromMaybe(value, error)` | `R.fromMaybe` | `Maybe a -> QiError -> Result a` |
| `fromEither(either)` | `R.fromEither` | `Either QiError a -> Result a` |
| `fromTryCatch(action)` | `R.fromTryCatch` | `IO a -> IO (Result a)` |

### Query Operations

| Contract | Haskell Implementation | Type Signature |
|----------|----------------------|----------------|
| `isSuccess()` | `R.isSuccess` | `Result a -> Bool` |
| `isFailure()` | `R.isFailure` | `Result a -> Bool` |
| `getValue()` | `R.getValue` | `Result a -> Maybe a` |
| `getError()` | `R.getError` | `Result a -> Maybe QiError` |

### Transformation Operations

| Contract | Haskell Implementation | Type Signature |
|----------|----------------------|----------------|
| `map(function)` | `R.map` or `fmap` | `(a -> b) -> Result a -> Result b` |
| `flatMap(function)` | `R.flatMap` or `>>=` | `Result a -> (a -> Result b) -> Result b` |
| `andThen(function)` | `R.andThen` | `(a -> Result b) -> Result a -> Result b` |
| `filter(predicate)` | `R.filter` | `(a -> Bool) -> QiError -> Result a -> Result a` |

## Mathematical Law Compliance

### Functor Laws (Automatically Satisfied)

```haskell
-- Identity Law: fmap id == id
prop_functorIdentity :: Result Int -> Bool
prop_functorIdentity r = fmap id r == r

-- Composition Law: fmap (f . g) == fmap f . fmap g  
prop_functorComposition :: Fun Int String -> Fun String Bool -> Result Int -> Bool
prop_functorComposition (Fun _ f) (Fun _ g) r = 
  fmap (g . f) r == (fmap g . fmap f) r
```

### Applicative Laws (Automatically Satisfied)

```haskell
-- Identity Law: pure id <*> v == v
prop_applicativeIdentity :: Result Int -> Bool
prop_applicativeIdentity v = (pure id <*> v) == v

-- Homomorphism Law: pure f <*> pure x == pure (f x)
prop_applicativeHomomorphism :: Fun Int String -> Int -> Bool  
prop_applicativeHomomorphism (Fun _ f) x = 
  (pure f <*> pure x) == (pure (f x) :: Result String)
```

### Monad Laws (Automatically Satisfied)

```haskell
-- Left Identity: return a >>= k == k a
prop_monadLeftIdentity :: Int -> Fun Int (Result String) -> Bool
prop_monadLeftIdentity a (Fun _ k) = (return a >>= k) == k a

-- Right Identity: m >>= return == m  
prop_monadRightIdentity :: Result Int -> Bool
prop_monadRightIdentity m = (m >>= return) == m

-- Associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)
prop_monadAssociativity :: Result Int -> Fun Int (Result String) -> Fun String (Result Bool) -> Bool
prop_monadAssociativity m (Fun _ f) (Fun _ g) = 
  ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
```

## Error Handling Patterns

### Error Construction

```haskell
-- Create basic error
basicError :: QiError
basicError = QE.fromCode "INVALID_INPUT" "Invalid input provided"

-- Add context to error
withContextError :: QiError  
withContextError = QE.withContext "userId" (toJSON "12345") basicError

-- Chain errors
chainedError :: QiError -> QiError
chainedError cause = QE.withCause cause $ 
  QE.fromCode "PROCESSING_ERROR" "Failed to process user input"
```

### Error Operations (IO-based for safety)

```haskell
-- Convert exceptions to Result (IO-based to avoid undefined)
fromExceptionIO :: IO a -> IO (Result a)
fromStringIO :: String -> IO QiError  
aggregateIO :: [QiError] -> IO QiError
```

**Key Design Decision:** Error creation operations that might fail are IO-based to avoid `undefined` values, maintaining the zero-fake-code policy.

## Collection Operations

### Sequence and Traverse

```haskell
-- Convert [Result a] to Result [a] (fail-fast)
sequence :: [Result a] -> Result [a]

-- Apply function and sequence in one step  
traverse :: (a -> Result b) -> [a] -> Result [b]

-- Partition results into successes and failures
partition :: [Result a] -> ([a], [QiError])
```

### Extraction Operations

```haskell
-- Extract all success values
rights :: [Result a] -> [a]

-- Extract all error values  
lefts :: [Result a] -> [QiError]

-- Unwrap with default value
unwrapOr :: a -> Result a -> a
```

## Performance Contracts

### Complexity Guarantees

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| `success`, `failure` | O(1) | O(1) |
| `isSuccess`, `isFailure` | O(1) | O(1) |
| `map`, `flatMap` | O(1) + f | O(1) + f |
| `getValue`, `getError` | O(1) | O(1) |
| `sequence` (n items) | O(n) | O(n) |

**Note:** 'f' represents the complexity of the applied function.

### Memory Usage

- Strict evaluation prevents space leaks
- Error chaining has bounded depth to prevent stack overflow
- Context maps use efficient Map implementation
- No lazy thunks in core operations

## JSON Serialization (Aeson 2.2)

### ToJSON Implementation

```haskell
instance ToJSON QiError where
  toJSON qe = object
    [ "code" .= qiErrorCode qe
    , "message" .= qiErrorMessage qe  
    , "category" .= qiErrorCategory qe
    , "context" .= qiErrorContext qe
    , "cause" .= qiErrorCause qe
    , "timestamp" .= qiErrorTimestamp qe
    , "severity" .= qiErrorSeverity qe
    ]
    
instance ToJSON (Result a) where
  toJSON (Success a) = object ["success" .= True, "data" .= a]
  toJSON (Failure e) = object ["success" .= False, "error" .= e]
```

### FromJSON Implementation

```haskell
instance FromJSON QiError where
  parseJSON = withObject "QiError" $ \o -> do
    code <- o .: "code"
    message <- o .: "message"
    category <- o .: "category" 
    context <- o .:? "context" .!= Map.empty
    cause <- o .:? "cause"
    timestamp <- o .: "timestamp"
    severity <- o .: "severity"
    pure $ QiError code message category context cause timestamp severity
```

## Concurrency and STM

### Thread Safety

All Result operations are pure and therefore thread-safe. For stateful operations:

```haskell
-- Example: Thread-safe result accumulation
accumulateResults :: [IO (Result a)] -> IO (Result [a])
accumulateResults actions = do
  results <- mapM id actions
  pure $ sequence results
```

### STM Integration

```haskell
-- STM-based result processing
processInSTM :: STM (Result a) -> IO (Result a)  
processInSTM stmAction = atomically stmAction
```

## GHC 9.12 Feature Integration

### GHC2024 Language Edition

```haskell
{-# LANGUAGE GHC2024 #-}
-- Automatically enables:
-- - DataKinds, TypeFamilies, LambdaCase
-- - BlockArguments, RoleAnnotations
-- - And other modern extensions
```

### MultilineStrings (New in GHC 9.12)

```haskell
errorDocumentation :: Text
errorDocumentation = \"\"\"
  Error handling in QiCore follows mathematical principles:
  - All operations preserve category theory laws
  - Error chaining maintains causal relationships
  - Context accumulation aids debugging
  \"\"\"
```

## Testing Integration

### Property-Based Testing

```haskell
-- Automatic law verification through QuickCheck
prop_functorLaws :: Result Int -> Bool
prop_monadLaws :: Result Int -> Fun Int (Result String) -> Bool  
prop_applicativeLaws :: Result Int -> Bool
```

### Contract Compliance Testing

Every contract operation has corresponding QuickCheck properties that verify:
- Correct behavior on valid inputs
- Proper error handling on invalid inputs
- Performance characteristics within bounds
- JSON serialization round-trip compatibility

## Migration and Compatibility

### From Older Versions

```haskell
-- Legacy compatibility (if needed)
type LegacyResult a = Either String a

migrateLegacy :: LegacyResult a -> Result a
migrateLegacy (Left msg) = Failure $ QE.fromCode "LEGACY_ERROR" (T.pack msg)
migrateLegacy (Right val) = Success val
```

### Cross-Language Consistency

The Haskell implementation serves as the mathematical reference for other language implementations:
- TypeScript implementation must pass identical contract tests
- Python implementation must exhibit same mathematical properties  
- C++ implementation must maintain performance characteristics
- All implementations must produce compatible JSON serialization

## Conclusion

The Haskell implementation of QiCore Foundation provides:

1. **Mathematical Rigor**: Category theory laws automatically satisfied
2. **Type Safety**: Compile-time verification of correctness
3. **Performance**: O(1) operations with predictable memory usage
4. **Modern Features**: Leveraging GHC 9.12 and GHC2024 
5. **Cross-Language Foundation**: Reference implementation for other languages

This specification ensures the Haskell implementation maintains mathematical integrity while providing the foundation for QiCore's multi-language ecosystem.