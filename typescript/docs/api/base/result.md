# Result<T> API Reference

The Result<T> type is the core abstraction for functional error handling in QiCore Foundation.

## Type Definition

```typescript
type Result<T, E = QiError> = 
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

type Success<T, E = QiError> = { readonly tag: 'success'; readonly value: T }
type Failure<E, T = unknown> = { readonly tag: 'failure'; readonly error: E }
```

## Factory Operations

### Ok<T>(value: T): Result<T, never>

Creates a successful Result.

```typescript
const number = Ok(42)
const user = Ok({ id: 1, name: 'Alice' })
const array = Ok([1, 2, 3])
```

### Err<E>(error: E): Result<never, E>

Creates a failed Result.

```typescript
const error = Err(new Error('Something went wrong'))
const customError = Err({ code: 'CUSTOM_ERROR', message: 'Custom error' })
```

### fromNullable<T>(value: T | null | undefined, error: QiError): Result<T>

Converts nullable values to Result.

```typescript
const user = getUser() // Returns User | null
const result = fromNullable(user, createError('USER_NOT_FOUND', 'User not found'))
```

### tryCatch<T>(fn: () => T): Result<T>

Wraps a function that might throw into a Result.

```typescript
const parseResult = tryCatch(() => JSON.parse(jsonString))
```

### asyncTryCatch<T>(fn: () => Promise<T>): Promise<Result<T>>

Wraps an async function that might throw into a Result.

```typescript
const fetchResult = await asyncTryCatch(() => fetch('/api/data'))
```

## Query Operations

### isSuccess<T, E>(result: Result<T, E>): result is Success<T, E>

Type guard for successful Results.

```typescript
if (isSuccess(result)) {
  console.log(result.value) // TypeScript knows this is safe
}
```

### isFailure<T, E>(result: Result<T, E>): result is Failure<E, T>

Type guard for failed Results.

```typescript
if (isFailure(result)) {
  console.log(result.error) // TypeScript knows this is safe
}
```

### getValue<T, E>(result: Result<T, E>): T | undefined

Safely extracts the value from a Result.

```typescript
const value = getValue(result) // T | undefined
```

### getError<T, E>(result: Result<T, E>): E | undefined

Safely extracts the error from a Result.

```typescript
const error = getError(result) // E | undefined
```

## Transformation Operations

### map<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E>

Transforms the success value of a Result.

```typescript
const doubled = map(Ok(21), x => x * 2) // Ok(42)
const error = map(Err('failed'), x => x * 2) // Err('failed')
```

**Laws:**
- Identity: `map(result, x => x) === result`
- Composition: `map(map(result, f), g) === map(result, x => g(f(x)))`

### mapError<T, E, F>(result: Result<T, E>, fn: (error: E) => F): Result<T, F>

Transforms the error of a Result.

```typescript
const enhanced = mapError(result, err => ({ ...err, timestamp: Date.now() }))
```

### flatMap<T, U, E>(result: Result<T, E>, fn: (value: T) => Result<U, E>): Result<U, E>

Monadic bind operation for chaining Results.

```typescript
const result = flatMap(getUser(123), user => 
  flatMap(getProfile(user.id), profile => 
    Ok({ user, profile })
  )
)
```

**Laws:**
- Left Identity: `flatMap(Ok(a), f) === f(a)`
- Right Identity: `flatMap(m, Ok) === m`
- Associativity: `flatMap(flatMap(m, f), g) === flatMap(m, x => flatMap(f(x), g))`

### andThen<T, U, E>(result: Result<T, E>, fn: (value: T) => Result<U, E>): Result<U, E>

Alias for flatMap.

```typescript
const result = andThen(step1(), step2)
```

### filter<T, E>(result: Result<T, E>, predicate: (value: T) => boolean, error: () => E): Result<T, E>

Filters a Result based on a predicate.

```typescript
const evenOnly = filter(
  Ok(42),
  x => x % 2 === 0,
  () => new Error('Not even')
)
```

### orElse<T, E>(result: Result<T, E>, fallback: () => Result<T, E>): Result<T, E>

Provides a fallback Result if the original failed.

```typescript
const result = orElse(primary, () => secondary)
```

## Extraction Operations

### unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T

Extracts the value or returns a default.

```typescript
const value = unwrapOr(result, 'default') // Always returns T
```

### unwrap<T, E>(result: Result<T, E>): T

Extracts the value, throwing if the Result is an error.

```typescript
const value = unwrap(Ok(42)) // 42
const error = unwrap(Err('failed')) // Throws Error
```

**⚠️ Warning**: Only use when you're certain the Result is successful.

### match<T, E, U>(result: Result<T, E>, handlers: { success: (value: T) => U; failure: (error: E) => U }): U

Pattern matching for Results.

```typescript
const message = match(result, {
  success: value => `Success: ${value}`,
  failure: error => `Error: ${error.message}`
})
```

## Collection Operations

### sequence<T, E>(results: Result<T, E>[]): Result<T[], E>

Converts an array of Results to a Result of an array.

```typescript
const results = [Ok(1), Ok(2), Ok(3)]
const sequenced = sequence(results) // Ok([1, 2, 3])

const mixed = [Ok(1), Err('failed'), Ok(3)]
const failed = sequence(mixed) // Err('failed')
```

### traverse<T, U, E>(array: T[], fn: (item: T) => Result<U, E>): Result<U[], E>

Maps a function over an array and sequences the results.

```typescript
const strings = ['1', '2', '3']
const numbers = traverse(strings, str => {
  const num = parseInt(str)
  return isNaN(num) ? Err('Invalid number') : Ok(num)
})
```

### partition<T, E>(results: Result<T, E>[]): { successes: T[]; failures: E[] }

Separates an array of Results into successes and failures.

```typescript
const results = [Ok(1), Err('error1'), Ok(3), Err('error2')]
const { successes, failures } = partition(results)
// successes: [1, 3]
// failures: ['error1', 'error2']
```

### combine2<T, U, E>(result1: Result<T, E>, result2: Result<U, E>): Result<[T, U], E>

Combines two Results into a single Result with a tuple.

```typescript
const combined = combine2(Ok(42), Ok('hello'))
// Ok([42, 'hello'])
```

## Applicative Operations

### apply<T, U, E>(fn: Result<(value: T) => U, E>, value: Result<T, E>): Result<U, E>

Applies a function in a Result to a value in a Result.

```typescript
const addFn = Ok((x: number) => x + 10)
const value = Ok(5)
const result = apply(addFn, value) // Ok(15)
```

### pure<T>(value: T): Result<T, never>

Lifts a value into the Result context (alias for Ok).

```typescript
const result = pure(42) // Ok(42)
```

## Async Operations

### asyncMap<T, U, E>(result: Result<T, E>, fn: (value: T) => Promise<U>): Promise<Result<U, E>>

Async version of map.

```typescript
const result = await asyncMap(Ok(42), async x => {
  const response = await fetch(`/api/data/${x}`)
  return await response.json()
})
```

### asyncAndThen<T, U, E>(result: Result<T, E>, fn: (value: T) => Promise<Result<U, E>>): Promise<Result<U, E>>

Async version of andThen/flatMap.

```typescript
const result = await asyncAndThen(userResult, async user => {
  const profile = await fetchProfile(user.id)
  return Ok({ user, profile })
})
```

### asyncSequence<T, E>(results: Promise<Result<T, E>>[]): Promise<Result<T[], E>>

Async version of sequence.

```typescript
const promises = [
  fetchUser(1),
  fetchUser(2),
  fetchUser(3)
]
const users = await asyncSequence(promises)
```

### fromPromise<T>(promise: Promise<T>): Promise<Result<T, Error>>

Converts a Promise to a Result.

```typescript
const result = await fromPromise(fetch('/api/data'))
```

### toPromise<T, E>(result: Result<T, E>): Promise<T>

Converts a Result to a Promise (rejects on error).

```typescript
const promise = toPromise(Ok(42)) // Promise<42>
const rejected = toPromise(Err('failed')) // Promise.reject('failed')
```

## Usage Examples

### Basic Usage

```typescript
import { Ok, Err, map, flatMap, match } from '@qi/qicore-foundation/base'

// Create Results
const success = Ok(42)
const failure = Err(new Error('Something went wrong'))

// Transform
const doubled = map(success, x => x * 2)

// Chain operations
const result = flatMap(success, x => 
  x > 0 ? Ok(Math.sqrt(x)) : Err(new Error('Negative number'))
)

// Pattern match
const message = match(result, {
  success: value => `Result: ${value}`,
  failure: error => `Error: ${error.message}`
})
```

### Async Operations

```typescript
import { asyncMap, asyncAndThen, fromPromise } from '@qi/qicore-foundation/base'

// Convert Promise to Result
const userResult = await fromPromise(fetch('/api/user/123'))

// Transform async
const enrichedUser = await asyncMap(userResult, async user => {
  const profile = await fetchProfile(user.id)
  return { ...user, profile }
})

// Chain async operations
const fullUser = await asyncAndThen(userResult, async user => {
  const profile = await fetchProfile(user.id)
  return asyncAndThen(profile, async profileData => {
    const settings = await fetchSettings(user.id)
    return Ok({ user, profile: profileData, settings })
  })
})
```

### Collection Processing

```typescript
import { traverse, sequence, partition } from '@qi/qicore-foundation/base'

// Process array of items
const userIds = [1, 2, 3, 4, 5]
const users = await traverse(userIds, id => fetchUser(id))

// Combine multiple Results
const results = [Ok(1), Ok(2), Err('failed'), Ok(4)]
const combined = sequence(results) // Err('failed')

// Separate successes and failures
const { successes, failures } = partition(results)
```

## Type Inference

Result<T> provides excellent TypeScript inference:

```typescript
// TypeScript infers Result<number>
const result = Ok(42)

// TypeScript infers Result<string>
const mapped = map(result, x => x.toString())

// TypeScript infers Result<User>
const user = flatMap(result, id => fetchUser(id))
```

## Error Handling Best Practices

1. **Always handle both cases**: Use pattern matching or type guards
2. **Avoid unwrap()**: Use unwrapOr() or pattern matching instead
3. **Chain operations**: Use flatMap for operations that return Results
4. **Provide defaults**: Use orElse() for fallback strategies
5. **Use type guards**: Leverage TypeScript's type narrowing

## Performance Considerations

- Results are plain objects with minimal overhead
- Operations are optimized for common cases
- Async operations use native Promise integration
- Collection operations use efficient array methods

## Mathematical Properties

Result<T> satisfies the mathematical laws for:

### Functor
- `map(result, id) === result`
- `map(map(result, f), g) === map(result, x => g(f(x)))`

### Applicative
- `apply(pure(id), result) === result`
- `apply(pure(f), pure(x)) === pure(f(x))`

### Monad
- `flatMap(pure(a), f) === f(a)`
- `flatMap(m, pure) === m`
- `flatMap(flatMap(m, f), g) === flatMap(m, x => flatMap(f(x), g))`

These laws ensure predictable and composable behavior.