# Async Result Helpers API Reference

The async helpers provide cleaner composition patterns for Promise<Result<T>> operations, eliminating manual unwrapping anti-patterns.

## Overview

These functions enable functional composition with async operations while maintaining the mathematical laws of Result<T>:

- **Async Transformation**: `flatMapAsync`, `mapAsync`, `matchAsync`
- **Promise Composition**: `flatMapPromise`, `mapPromise`, `matchPromise`  
- **Async Collections**: `sequenceAsync`, `collectAsync`
- **Type Guards**: `isPromiseResult`

## Async Transformation Operations

### flatMapAsync<T, U, E>(fn: (value: T) => Promise<Result<U, E>>, result: Result<T, E>): Promise<Result<U, E>>

Async version of flatMap for operations returning Promise<Result<T>>.

**Contract:**
- `flatMapAsync(f)(failure(e)) == Promise.resolve(failure(e))`
- `flatMapAsync(f)(success(x)) == f(x)`

```typescript
import { flatMapAsync, success } from '@qi/base'

const result = await flatMapAsync(
  async config => {
    const userData = await fetchUserData(config.apiUrl)
    return userData ? success(userData) : failure(networkError('Fetch failed'))
  },
  configResult // Result<Config, QiError>
)
```

### mapAsync<T, U, E>(fn: (value: T) => Promise<U>, result: Result<T, E>): Promise<Result<U, E | QiError>>

Async version of map with automatic error wrapping.

**Contract:**
- `mapAsync(f)(failure(e)) == Promise.resolve(failure(e))`  
- `mapAsync(f)(success(x)) == Promise.resolve(success(await f(x)))` (on success)
- `mapAsync(f)(success(x)) == Promise.resolve(failure(error))` (on thrown error)

```typescript
import { mapAsync, success } from '@qi/base'

const result = await mapAsync(
  async data => {
    // This async function can throw - errors are automatically wrapped
    const processed = await processData(data)
    return processed
  },
  dataResult // Result<Data, QiError>
)
```

### matchAsync<T, E, R>(onSuccess: (value: T) => Promise<R>, onError: (error: E) => Promise<R>, result: Result<T, E>): Promise<R>

Async version of match for async result handling.

**Contract:**
- `matchAsync(onSuccess, onError)(failure(e)) == onError(e)`
- `matchAsync(onSuccess, onError)(success(x)) == onSuccess(x)`

```typescript
import { matchAsync } from '@qi/base'

await matchAsync(
  async user => {
    console.log('Processing user:', user.name)
    await saveUserToDatabase(user)
  },
  async error => {
    console.error('Operation failed:', error.message)
    await logError(error)
  },
  userResult // Result<User, QiError>
)
```

## Promise<Result<T>> Composition Operations

### flatMapPromise<T, U, E>(fn: (value: T) => Result<U, E> | Promise<Result<U, E>>, promiseResult: Promise<Result<T, E>>): Promise<Result<U, E>>

Compose operations on Promise<Result<T>> without manual awaiting.

**Contract:**
- `flatMapPromise(f)(Promise.resolve(failure(e))) == Promise.resolve(failure(e))`
- `flatMapPromise(f)(Promise.resolve(success(x))) == f(x)` (if f returns Result)
- `flatMapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(f(x))` (if f returns Promise<Result>)

```typescript
import { flatMapPromise } from '@qi/base'

// Clean composition without manual awaiting
const result = await flatMapPromise(
  config => validateAndLoadData(config), // Can return Result<T> or Promise<Result<T>>
  loadConfigAsync() // Promise<Result<Config, QiError>>
)
```

### mapPromise<T, U, E>(fn: (value: T) => U | Promise<U>, promiseResult: Promise<Result<T, E>>): Promise<Result<U, E | QiError>>

Map over Promise<Result<T>> with automatic error handling.

**Contract:**
- `mapPromise(f)(Promise.resolve(failure(e))) == Promise.resolve(failure(e))`
- `mapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(success(f(x)))` (sync f)
- `mapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(success(await f(x)))` (async f)

```typescript
import { mapPromise } from '@qi/base'

const result = await mapPromise(
  data => processData(data), // Can be sync or async
  loadDataAsync() // Promise<Result<Data, QiError>>
)
```

### matchPromise<T, E, R>(onSuccess: (value: T) => Promise<R>, onError: (error: E) => Promise<R>, promiseResult: Promise<Result<T, E>>): Promise<R>

Match over Promise<Result<T>> with async handlers.

```typescript
import { matchPromise } from '@qi/base'

await matchPromise(
  async user => console.log('Loaded user:', user.name),
  async error => console.error('Load failed:', error.message),
  loadUserAsync() // Promise<Result<User, QiError>>
)
```

## Async Collection Operations

### sequenceAsync<T, E>(promises: Promise<Result<T, E>>[]): Promise<Result<T[], E>>

Sequence async operations, stopping at first failure.

**Contract:**
- `sequenceAsync([]) == Promise.resolve(success([]))`
- `sequenceAsync([success(x)]) == Promise.resolve(success([x]))`  
- `sequenceAsync([failure(e), ...]) == Promise.resolve(failure(e))`

```typescript
import { sequenceAsync } from '@qi/base'

const results = await sequenceAsync([
  loadUser('1'),    // Promise<Result<User, QiError>>
  loadUser('2'), 
  loadUser('3')
])

// Result<User[], QiError> - all users or first error
```

### collectAsync<T, E>(promises: Promise<Result<T, E>>[]): Promise<{ successes: T[]; failures: E[] }>

Collect all async operations, preserving both successes and failures.

**Contract:**
- `collectAsync([]) == Promise.resolve({ successes: [], failures: [] })`
- `collectAsync(promises)` preserves order and partitions results

```typescript
import { collectAsync } from '@qi/base'

const { successes, failures } = await collectAsync([
  loadUser('1'),
  loadUser('invalid-id'),
  loadUser('3')
])

console.log(`Loaded ${successes.length} users, ${failures.length} failures`)
```

## Type Guards and Utilities

### isPromiseResult<T, E>(value: unknown): value is Promise<Result<T, E>>

Type guard for Promise<Result<T>>.

```typescript
import { isPromiseResult } from '@qi/base'

if (isPromiseResult(someValue)) {
  // TypeScript knows someValue is Promise<Result<T, E>>
  const result = await someValue
}
```

## Usage Patterns

### Before: Manual Unwrapping Anti-Pattern

```typescript
// ❌ ANTI-PATTERN: Manual Result unwrapping
const configResult = await loadConfig()
if (configResult.tag === 'success') {
  const userResult = await loadUser(configResult.value)
  if (userResult.tag === 'success') {
    console.log('User:', userResult.value.name)
  } else {
    console.error('User load failed:', userResult.error.message)
  }
} else {
  console.error('Config load failed:', configResult.error.message)
}
```

### After: Clean Async Composition

```typescript
// ✅ CLEAN: Functional async composition
await matchAsync(
  async config => {
    await matchAsync(
      user => console.log('User:', user.name),
      error => console.error('User load failed:', error.message),
      await loadUser(config)
    )
  },
  error => console.error('Config load failed:', error.message),
  await loadConfig()
)

// Or with flatMapAsync
const result = await flatMapAsync(
  config => loadUser(config),
  await loadConfig()
)

match(
  user => console.log('User:', user.name),
  error => console.error('Failed:', error.message),
  result
)
```

### Promise Composition Patterns

```typescript
// ✅ Clean Promise<Result<T>> composition
const processedData = await flatMapPromise(
  async config => {
    const data = await loadData(config.source)
    return success(await processData(data))
  },
  loadConfigAsync()
)

// ✅ Collection processing
const allUsers = await sequenceAsync([
  loadUser('1'),
  loadUser('2'), 
  loadUser('3')
])

const { successes: users, failures: errors } = await collectAsync([
  loadUser('1'),
  loadUser('invalid'),
  loadUser('3')
])
```

## Mathematical Properties

All async helpers preserve the mathematical laws of Result<T>:

### Functor Laws for mapAsync
- **Identity**: `mapAsync(async x => x, result) ≡ result`
- **Composition**: `mapAsync(f, mapAsync(g, result)) ≡ mapAsync(x => f(await g(x)), result)`

### Monad Laws for flatMapAsync  
- **Left Identity**: `flatMapAsync(f, success(x)) ≡ f(x)`
- **Right Identity**: `flatMapAsync(success, result) ≡ result`
- **Associativity**: `flatMapAsync(g, flatMapAsync(f, result)) ≡ flatMapAsync(x => flatMapAsync(g, f(x)), result)`

These properties ensure that async operations compose correctly and maintain the guarantees of functional error handling.