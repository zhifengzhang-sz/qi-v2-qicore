# QiCore Base Module API Documentation

## Overview

The QiCore Base module provides mathematical foundation types for error handling with Result<T> and QiError. This implementation follows the Haskell reference implementation exactly, using standalone functions rather than fluent API patterns.

## Import Structure

```typescript
// For development (using path aliases)
import { Ok, Err, map, flatMap, type Result, type QiError } from '@qi/base'

// For published package consumers
import { Ok, Err, map, flatMap, type Result, type QiError } from '@qi/qicore-foundation/base'
```

## Core Types

### Result<T, E>

A discriminated union representing success or failure:

```typescript
type Result<T, E = QiError> =
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }
```

### QiError

Structured error information with categorization:

```typescript
interface QiError {
  readonly code: ErrorCode
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Record<string, unknown>
  readonly cause?: QiError
  readonly timestamp: Date
  readonly severity?: 'low' | 'medium' | 'high'
}
```

### ErrorCategory

```typescript
type ErrorCategory =
  | 'VALIDATION'     // Input validation failures - no retry
  | 'NETWORK'        // Network connectivity issues - retry with backoff
  | 'SYSTEM'         // System/infrastructure failures - limited retry
  | 'BUSINESS'       // Business logic violations - no retry
  | 'AUTHENTICATION' // Authentication failures - no retry
  | 'AUTHORIZATION'  // Authorization/permission failures - no retry
  | 'CONFIGURATION'  // Configuration/setup errors - no retry
  | 'TIMEOUT'        // Timeout errors - retry with backoff
  | 'RESOURCE'       // Resource exhaustion/unavailable - retry with backoff
  | 'CONCURRENCY'    // Concurrency conflicts - retry with jitter
  | 'LOGGER'         // Logger-related errors - no retry
```

## Factory Functions

### Success Constructor

```typescript
const Ok = <T>(value: T): Result<T, never>
```

**Example:**
```typescript
const result = Ok(42)
// result.tag === 'success', result.value === 42
```

### Failure Constructor

```typescript
const Err = <E>(error: E): Result<never, E>
```

**Example:**
```typescript
const result = Err(validationError('Invalid input'))
// result.tag === 'failure', result.error contains QiError
```

### From Nullable

```typescript
const fromNullable = <T>(
  value: T | null | undefined,
  error: QiError
): Result<T, QiError>
```

**Example:**
```typescript
const result = fromNullable(user, validationError('User not found'))
```

### Safe Execution

```typescript
const tryCatch = <T>(fn: () => T): Result<T, Error>
const asyncTryCatch = <T>(fn: () => Promise<T>): Promise<Result<T, Error>>
```

## Type Guards

### Check Success State

```typescript
const isSuccess = <T, E>(result: Result<T, E>): result is Success<T, E>
const isFailure = <T, E>(result: Result<T, E>): result is Failure<E, T>
```

### Extract Values

```typescript
const getValue = <T, E>(result: Result<T, E>): T | undefined
const getError = <T, E>(result: Result<T, E>): E | undefined
```

## Transformation Functions

### Functor Operations

```typescript
const map = <T, U, E>(fn: (value: T) => U, result: Result<T, E>): Result<U, E>
const mapError = <T, E1, E2>(fn: (error: E1) => E2, result: Result<T, E1>): Result<T, E2>
```

**Example:**
```typescript
const doubled = map((x: number) => x * 2, Ok(21))
// doubled === Ok(42)
```

### Monadic Operations

```typescript
const flatMap = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  result: Result<T, E>
): Result<U, E>

const andThen = <T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Result<U, E>
): Result<U, E>
```

**Example:**
```typescript
const parseNumber = (s: string): Result<number> =>
  Number.isNaN(Number(s)) ? Err(validationError('Not a number')) : Ok(Number(s))

const result = flatMap(parseNumber, Ok('42'))
// result === Ok(42)
```

### Filtering

```typescript
const filter = <T, E>(
  predicate: (value: T) => boolean,
  error: E,
  result: Result<T, E>
): Result<T, E>
```

### Error Recovery

```typescript
const orElse = <T, E>(
  fn: (error: E) => Result<T, E>,
  result: Result<T, E>
): Result<T, E>
```

## Extraction Functions

### Safe Extraction

```typescript
const unwrapOr = <T, E>(defaultValue: T, result: Result<T, E>): T
```

### Unsafe Extraction

```typescript
const unwrap = <T, E>(result: Result<T, E>): T  // Throws on failure
```

### Pattern Matching

```typescript
const match = <T, E, R>(
  onSuccess: (value: T) => R,
  onError: (error: E) => R,
  result: Result<T, E>
): R
```

**Example:**
```typescript
const message = match(
  (value: number) => `Success: ${value}`,
  (error: QiError) => `Error: ${error.message}`,
  result
)
```

## Collection Operations

### Sequence (Fail-Fast)

```typescript
const sequence = <T, E>(results: Result<T, E>[]): Result<T[], E>
```

### Traverse

```typescript
const traverse = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  values: T[]
): Result<U[], E>
```

### Partition

```typescript
const partition = <T, E>(
  results: Result<T, E>[]
): { successes: T[]; failures: E[] }
```

### Combine Two Results

```typescript
const combine2 = <T, U, V, E>(
  result1: Result<T, E>,
  result2: Result<U, E>,
  fn: (value1: T, value2: U) => V
): Result<V, E>
```

## Applicative Operations

### Apply Function

```typescript
const apply = <T, U, E>(
  fnResult: Result<(value: T) => U, E>,
  result: Result<T, E>
): Result<U, E>
```

### Pure (Unit)

```typescript
const pure = <T>(value: T): Result<T, never>
```

## Async Operations

### Async Transformation

```typescript
const asyncMap = <T, U, E>(
  fn: (value: T) => Promise<U>,
  result: Result<T, E>
): Promise<Result<U, E | Error>>
```

### Async Monadic Bind

```typescript
const asyncAndThen = <T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Promise<Result<U, E>>
): Promise<Result<U, E>>
```

### Async Sequence

```typescript
const asyncSequence = <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T[], E>>
```

### Promise Conversion

```typescript
const fromPromise = <T>(promise: Promise<T>): Promise<Result<T, Error>>
const toPromise = <T, E>(result: Result<T, E>): Promise<T>
```

## QiError Factory Functions

### Validation Error

```typescript
const validationError = (message: string, context?: Record<string, unknown>): QiError
```

### Network Error

```typescript
const networkError = (message: string, context?: Record<string, unknown>): QiError
```

### System Error

```typescript
const systemError = (message: string, context?: Record<string, unknown>): QiError
```

### Business Error

```typescript
const businessError = (message: string, context?: Record<string, unknown>): QiError
```

### Authentication Error

```typescript
const authenticationError = (message: string, context?: Record<string, unknown>): QiError
```

### Authorization Error

```typescript
const authorizationError = (message: string, context?: Record<string, unknown>): QiError
```

### Configuration Error

```typescript
const configurationError = (message: string, context?: Record<string, unknown>): QiError
```

### Timeout Error

```typescript
const timeoutError = (message: string, context?: Record<string, unknown>): QiError
```

### Resource Error

```typescript
const resourceError = (message: string, context?: Record<string, unknown>): QiError
```

### Concurrency Error

```typescript
const concurrencyError = (message: string, context?: Record<string, unknown>): QiError
```

## Error Utilities

### Retry Strategy

```typescript
const getRetryStrategy = (error: QiError): RetryStrategy
```

### Category Checking

```typescript
const hasCategory = (error: QiError, category: ErrorCategory): boolean
```

### Error Chaining

```typescript
const chainError = (
  error: QiError,
  newError: Omit<QiError, 'cause' | 'timestamp'>
): QiError
```

### Error Formatting

```typescript
const formatErrorChain = (error: QiError, maxDepth?: number): string[]
const getRootCause = (error: QiError): QiError
```

### Context Management

```typescript
const withContext = (error: QiError, context: Record<string, unknown>): QiError
const getContext = <T>(error: QiError, key: string): T | undefined
```

### Serialization

```typescript
const serializeError = (error: QiError): Record<string, unknown>
const deserializeError = (data: Record<string, unknown>): QiError
```

## Mathematical Laws

The implementation satisfies all required mathematical laws:

### Functor Laws
- Identity: `map(id, result) === result`
- Composition: `map(f, map(g, result)) === map(compose(f, g), result)`

### Monad Laws
- Left Identity: `flatMap(f, pure(x)) === f(x)`
- Right Identity: `flatMap(pure, result) === result`
- Associativity: `flatMap(g, flatMap(f, result)) === flatMap(x => flatMap(g, f(x)), result)`

### Applicative Laws
- Identity: `apply(pure(id), result) === result`
- Composition: `apply(apply(apply(pure(compose), f), g), x) === apply(f, apply(g, x))`
- Homomorphism: `apply(pure(f), pure(x)) === pure(f(x))`
- Interchange: `apply(f, pure(x)) === apply(pure(f => f(x)), f)`

## Usage Examples

### Basic Error Handling

```typescript
import { Ok, Err, map, flatMap, match, validationError } from '@qi/base'

const divide = (a: number, b: number): Result<number, QiError> =>
  b === 0 ? Err(validationError('Division by zero')) : Ok(a / b)

const result = flatMap(
  (x: number) => divide(x, 2),
  map((s: string) => parseInt(s), Ok('42'))
)

const message = match(
  (value: number) => `Result: ${value}`,
  (error: QiError) => `Error: ${error.message}`,
  result
)
```

### Chain of Operations

```typescript
const processData = (input: string): Result<number, QiError> => {
  return flatMap(
    (validated: string) => 
      validated.length > 0 
        ? Ok(validated) 
        : Err(validationError('Empty input')),
    flatMap(
      (parsed: number) => 
        parsed > 0 
          ? Ok(parsed) 
          : Err(validationError('Must be positive')),
      map(
        (s: string) => parseInt(s), 
        Ok(input)
      )
    )
  )
}
```

### Collection Processing

```typescript
const parseNumbers = (strings: string[]): Result<number[], QiError> => {
  const parseNumber = (s: string): Result<number, QiError> =>
    Number.isNaN(Number(s)) ? Err(validationError('Not a number')) : Ok(Number(s))
  
  return traverse(parseNumber, strings)
}

const result = parseNumbers(['1', '2', '3'])
// result === Ok([1, 2, 3])
```

This API documentation reflects the actual implementation using standalone functions, not fluent API patterns as incorrectly specified in the contracts for the base module.