# Framework: qi/base

> **Prerequisites:** Make sure you've completed [Project Setup](./setup.md) first to configure your package.json and tsconfig.json. Without proper setup, the imports shown below won't work.

## What It Does

qi/base provides a complete error handling framework with three main concepts:
- **Result<T>**: Explicit success/failure types instead of exceptions
- **Error Categories**: Structured error classification for retry strategies  
- **Combinators**: Functions to transform and compose Results safely

**See [Complete API Documentation](../api/base.md) for all available functions.**

## Why You Need This

Every function that can fail should return Result<T> instead of throwing exceptions. This makes errors visible in function signatures and prevents unexpected crashes.

## Core Concepts

### 1. Result<T>
Explicit success/failure values:

```typescript
// Instead of exceptions (invisible errors)
function divide(a: number, b: number): number {
  if (b === 0) throw new Error('Division by zero')  // Invisible!
  return a / b
}

// Use Result<T> (explicit errors)
function divide(a: number, b: number): Result<number, QiError> {
  if (b === 0) return failure(validationError('Division by zero'))
  return success(a / b)
}
```

### 2. Error Categories
Structured error classification determines how to handle errors:

```typescript
const networkErr = networkError('Connection failed')    // NETWORK - retry with backoff
const validationErr = validationError('Invalid email')  // VALIDATION - never retry, fix input
const businessErr = businessError('Account suspended')  // BUSINESS - never retry, business logic
```

### 3. Combinators
Transform and compose Results safely:

```typescript
// Transform success values
const doubled = map(x => x * 2, success(21))  // success(42)

// Chain operations that might fail  
const result = flatMap(
  x => x > 0 ? success(Math.sqrt(x)) : failure(validationError('Must be positive')),
  success(16)
)  // success(4)

// Handle both cases
match(
  value => console.log(`Success: ${value}`),
  error => console.log(`Error: ${error.message}`),
  result
)
```

```typescript
// Instead of this (invisible errors):
function divide(a: number, b: number): number {
  if (b === 0) throw new Error('Division by zero')  // Invisible!
  return a / b
}

// Use this (explicit errors):
function divide(a: number, b: number): Result<number, QiError> {
  if (b === 0) return failure(validationError('Division by zero'))
  return success(a / b)
}
```

## Basic Usage

### Import What You Need

QiCore Foundation provides flexible import patterns to match your coding style:

**Standard Imports:**
```typescript
import { 
  success, failure, match, map, flatMap,
  type Result, type QiError,
  validationError, networkError 
} from '@qi/base'
```

**With Convenient Aliases:**
```typescript
import { 
  Ok, Err,           // Aliases for success/failure
  match, map, flatMap,
  type Result, type QiError,
  validationError, networkError 
} from '@qi/base'
```

**All Available Imports:**
```typescript
// Core Result types
import { 
  success, failure,     // Create Results
  Ok, Err,             // Aliases for success/failure
  type Result,         // Result<T, E> type
  type Success,        // Success variant type  
  type Failure         // Failure variant type
} from '@qi/base'

// Error handling
import {
  type QiError,        // Error interface
  type ErrorCategory,  // Error category enum
  type ErrorOptions,   // Error creation options
  ErrorCategories,     // All available categories
  isErrorCategory      // Type guard
} from '@qi/base'

// Error factory functions
import {
  create, createError, fromException, fromString,
  validationError, networkError, systemError,
  businessError, authenticationError, authorizationError,
  configurationError, timeoutError, resourceError,
  concurrencyError, loggerError
} from '@qi/base'

// Result combinators
import {
  map, flatMap, andThen, collect,
  filter, orElse, match,
  inspect, inspectErr,
  getValue, getError,
  unwrap, unwrapOr,
  isSuccess, isFailure
} from '@qi/base'

// Async helpers (Promise<Result<T>> composition)
import {
  flatMapAsync, mapAsync, matchAsync,
  flatMapPromise, mapPromise, matchPromise,
  sequenceAsync, collectAsync,
  isPromiseResult
} from '@qi/base'

// Result creation utilities
import {
  fromTryCatch, tryCatch,     // Aliases for same function
  fromAsyncTryCatch,
  fromMaybe, fromEither
} from '@qi/base'

// Error utilities  
import {
  errorToString, getCategory, toStructuredData,
  withContext, getRetryStrategy
} from '@qi/base'
```

**Import Style Recommendations:**

1. **For beginners**: Use full names for clarity
```typescript
import { success, failure, match, flatMap } from '@qi/base'
```

2. **For Rust/functional style**: Use short aliases
```typescript
import { Ok, Err, match, flatMap } from '@qi/base'
```

3. **For async code**: Include Promise helpers
```typescript
import { Ok, Err, matchAsync, flatMapAsync } from '@qi/base'
```

### Create Results

```typescript
// Success
const goodResult = success(42)

// Failure  
const badResult = failure(validationError('Invalid input'))
```

### Handle Results

```typescript
const result = divide(10, 2)

match(
  value => console.log(`Result: ${value}`),   // Success case
  error => console.log(`Error: ${error.message}`), // Failure case
  result
)
```

### Transform Results

```typescript
// Transform success values
const doubled = map(x => x * 2, success(21))  // success(42)

// Chain operations that might fail
const result = flatMap(
  x => x > 0 ? success(Math.sqrt(x)) : failure(validationError('Must be positive')),
  success(16)
)  // success(4)
```

## Error Categories

QiError includes categories that determine how to handle the error:

```typescript
const error = networkError('Connection failed')
// error.category === 'NETWORK' - should retry with backoff

const error = validationError('Invalid email')  
// error.category === 'VALIDATION' - never retry, fix input
```

**Available categories:**
- `VALIDATION` - Input validation failures (never retry)
- `NETWORK` - Network issues (retry with backoff)
- `BUSINESS` - Business rule violations (never retry)  
- `AUTHENTICATION` - Login failures (never retry)
- `AUTHORIZATION` - Permission denied (never retry)
- `SYSTEM` - Infrastructure failures (limited retry)

## Real Example

```typescript
import { success, failure, match, flatMap, validationError, networkError } from '@qi/base'

// Functions that return Result<T>
async function fetchUser(id: string): Promise<Result<User, QiError>> {
  if (!id) return failure(validationError('User ID required'))
  
  try {
    const user = await api.get(`/users/${id}`)
    return success(user)
  } catch (err) {
    return failure(networkError('Failed to fetch user'))
  }
}

async function updateUser(user: User): Promise<Result<User, QiError>> {
  if (!user.email) return failure(validationError('Email required'))
  
  try {
    const updated = await api.put(`/users/${user.id}`, user)
    return success(updated)
  } catch (err) {
    return failure(networkError('Failed to update user'))
  }
}

// Compose operations - errors propagate automatically
async function updateUserProfile(id: string, data: Partial<User>): Promise<Result<User, QiError>> {
  const userResult = await fetchUser(id)
  
  return flatMap(
    user => updateUser({ ...user, ...data }),
    userResult
  )
}

// Use the composed function
const result = await updateUserProfile('123', { name: 'John' })

match(
  user => console.log('Updated:', user.name),
  error => {
    switch (error.category) {
      case 'VALIDATION':
        console.log('Fix input:', error.message)
        break
      case 'NETWORK':
        console.log('Will retry:', error.message)
        break
    }
  },
  result
)
```

## Key Benefits

1. **No unexpected crashes** - Errors are explicit in function signatures
2. **Composable** - Chain operations with flatMap, errors propagate automatically  
3. **Structured errors** - Categories tell you how to handle each error
4. **Type safe** - TypeScript ensures you handle both success and failure cases

## Working Example

Try the complete working example:

```bash
cd typescript/app/basic-result
bun run dev
```

This shows Result<T> patterns with divide(), map(), and flatMap() operations.

## Next Steps

Once you understand Result<T> patterns, learn how the tools use this framework:

- [Config Tool](./qi-core-config.md) - Configuration loading returns Result<T>
- [Logger Tool](./qi-core-logger.md) - Log Result<T> outcomes consistently  
- [Cache Tool](./qi-core-cache.md) - Cache operations return Result<T>

All tools use the same Result<T> patterns you learned here.