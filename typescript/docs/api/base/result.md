# Result<T> API Reference

The Result<T> type is the core abstraction for functional error handling in QiCore Foundation.

## Type Definition

```typescript
type Result<T, E = QiError> = 
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

type Success<T> = { readonly tag: 'success'; readonly value: T }
type Failure<E> = { readonly tag: 'failure'; readonly error: E }
```

## Factory Operations

### success<T>(value: T): Result<T, never>

Creates a successful Result.

```typescript
const number = success(42)
const user = success({ id: 1, name: 'Alice' })
const array = success([1, 2, 3])
```

### failure<E>(error: E): Result<never, E>

Creates a failed Result.

```typescript
const error = failure(new Error('Something went wrong'))
const customError = failure({ code: 'CUSTOM_ERROR', message: 'Custom error' })
```

### fromMaybe<T>(value: T | null | undefined, errorIfNull: QiError): Result<T, QiError>

Converts nullable values to Result.

```typescript
const user = getUser() // Returns User | null
const result = fromMaybe(user, createError('USER_NOT_FOUND', 'User not found'))
```

### fromTryCatch<T>(operation: () => T, errorHandler?: (error: unknown) => QiError): Result<T, QiError>

Wraps a function that might throw into a Result.

```typescript
const parseResult = fromTryCatch(() => JSON.parse(jsonString))
```

### fromAsyncTryCatch<T>(operation: () => Promise<T>, errorHandler?: (error: unknown) => QiError): Promise<Result<T, QiError>>

Wraps an async function that might throw into a Result.

```typescript
const fetchResult = await fromAsyncTryCatch(() => fetch('/api/data'))
```

### fromEither<L, R>(either: { tag: 'left'; value: L } | { tag: 'right'; value: R }): Result<R, L>

Converts from Either-like type.

```typescript
const either = { tag: 'right', value: 42 }
const result = fromEither(either) // success(42)
```

## Query Operations

### isSuccess<T, E>(result: Result<T, E>): result is Success<T>

Type guard for successful Results.

```typescript
if (isSuccess(result)) {
  console.log(result.value) // TypeScript knows this is safe
}
```

### isFailure<T, E>(result: Result<T, E>): result is Failure<E>

Type guard for failed Results.

```typescript
if (isFailure(result)) {
  console.log(result.error) // TypeScript knows this is safe
}
```

### getValue<T, E>(result: Result<T, E>): T | null

Safely extracts the value from a Result.

```typescript
const value = getValue(result) // T | null
```

### getError<T, E>(result: Result<T, E>): E | null

Safely extracts the error from a Result.

```typescript
const error = getError(result) // E | null
```

## Transformation Operations

### map<T, U, E>(fn: (value: T) => U, result: Result<T, E>): Result<U, E>

Transforms the success value of a Result.

```typescript
const doubled = map(x => x * 2, success(21)) // success(42)
const error = map(x => x * 2, failure('failed')) // failure('failed')
```

**Laws:**
- Identity: `map(x => x, result) === result`
- Composition: `map(g, map(f, result)) === map(x => g(f(x)), result)`

### mapError<T, E, F>(fn: (error: E) => F, result: Result<T, E>): Result<T, F>

Transforms the error of a Result.

```typescript
const enhanced = mapError(err => ({ ...err, timestamp: Date.now() }), result)
```

### flatMap<T, U, E>(fn: (value: T) => Result<U, E>, result: Result<T, E>): Result<U, E>

Monadic bind operation for chaining Results.

```typescript
const result = flatMap(user => 
  flatMap(profile => 
    success({ user, profile }), getProfile(user.id)
  ), getUser(123)
)
```

**Laws:**
- Left Identity: `flatMap(f, success(a)) === f(a)`
- Right Identity: `flatMap(success, m) === m`
- Associativity: `flatMap(g, flatMap(f, m)) === flatMap(x => flatMap(g, f(x)), m)`

### andThen<T, U, E>(fn: (value: T) => Result<U, E>, result: Result<T, E>): Result<U, E>

Alias for flatMap.

```typescript
const result = andThen(step2, step1())
```

### filter<T, E>(predicate: (value: T) => boolean, errorIfFalse: E, result: Result<T, E>): Result<T, E>

Filters a Result based on a predicate.

```typescript
const evenOnly = filter(
  x => x % 2 === 0,
  new Error('Not even'),
  success(42)
)
```

### orElse<T, E, F>(alternative: (error: E) => Result<T, F>, result: Result<T, E>): Result<T, F>

Provides a fallback Result if the original failed.

```typescript
const result = orElse(() => secondary, primary)
```

## Extraction Operations

### unwrapOr<T, E>(defaultValue: T, result: Result<T, E>): T

Extracts the value or returns a default.

```typescript
const value = unwrapOr('default', result) // Always returns T
```

### unwrap<T, E>(result: Result<T, E>): T

Extracts the value, throwing if the Result is an error.

```typescript
const value = unwrap(success(42)) // 42
const error = unwrap(failure('failed')) // Throws Error
```

**⚠️ Warning**: Only use when you're certain the Result is successful.

### inspect<T, E>(fn: (value: T) => void, result: Result<T, E>): Result<T, E>

Inspects the success value without changing the Result.

```typescript
const logged = inspect(value => console.log('Value:', value), result)
```

### inspectErr<T, E>(fn: (error: E) => void, result: Result<T, E>): Result<T, E>

Inspects the error value without changing the Result.

```typescript
const logged = inspectErr(error => console.error('Error:', error), result)
```

### collect<T, E>(result: Result<Result<T, E>, E>): Result<T, E>

Flattens nested Results.

```typescript
const nested = success(success(42))
const flattened = collect(nested) // success(42)
```

### match<T, E, R>(onSuccess: (value: T) => R, onError: (error: E) => R, result: Result<T, E>): R

Pattern matching for Results.

```typescript
const message = match(
  value => `Success: ${value}`,
  error => `Error: ${error.message}`,
  result
)
```


## Usage Examples

### Basic Usage

```typescript
import { success, failure, map, flatMap, match } from '@qi/qicore-foundation/base'

// Create Results
const successResult = success(42)
const failureResult = failure(new Error('Something went wrong'))

// Transform
const doubled = map(x => x * 2, successResult)

// Chain operations
const result = flatMap(x => 
  x > 0 ? success(Math.sqrt(x)) : failure(new Error('Negative number')),
  successResult
)

// Pattern match
const message = match(
  value => `Result: ${value}`,
  error => `Error: ${error.message}`,
  result
)
```


## Type Inference

Result<T> provides excellent TypeScript inference:

```typescript
// TypeScript infers Result<number>
const result = success(42)

// TypeScript infers Result<string>
const mapped = map(x => x.toString(), result)

// TypeScript infers Result<User>
const user = flatMap(id => fetchUser(id), result)
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
- `map(id, result) === result`
- `map(g, map(f, result)) === map(x => g(f(x)), result)`

### Applicative
- `apply(pure(id), result) === result`
- `apply(pure(f), pure(x)) === pure(f(x))`

### Monad
- `flatMap(f, success(a)) === f(a)`
- `flatMap(success, m) === m`
- `flatMap(g, flatMap(f, m)) === flatMap(x => flatMap(g, f(x)), m)`

These laws ensure predictable and composable behavior.