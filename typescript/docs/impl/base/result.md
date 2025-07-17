Here's a contract-compliant implementation of `result.ts`:

```typescript
/**
 * QiCore Foundation Base - Result<T> Implementation
 *
 * Pure discriminated union implementation with standalone functions
 * following the behavioral contracts exactly.
 */

import type { QiError } from './error.js'

/**
 * Result<T> - Pure discriminated union
 * Contract: Result<T> = Success<T> | Failure<QiError>
 */
export type Result<T, E = QiError> =
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

/**
 * Success variant type for cleaner type guards
 */
export type Success<T> = { readonly tag: 'success'; readonly value: T }

/**
 * Failure variant type for cleaner type guards
 */
export type Failure<E> = { readonly tag: 'failure'; readonly error: E }

// ============================================================================
// Factory Operations (Contract Section 1.1)
// ============================================================================

/**
 * Create successful Result
 * Contract: success(x).isSuccess() == true
 */
export const success = <T>(value: T): Result<T, never> => ({ 
  tag: 'success', 
  value 
})

/**
 * Create failed Result
 * Contract: failure(e).isFailure() == true
 */
export const failure = <E>(error: E): Result<never, E> => ({ 
  tag: 'failure', 
  error 
})

/**
 * Create Result from fallible operation
 * Contract: if operation succeeds: result.isSuccess() == true
 * Contract: if operation throws: result.isFailure() == true
 */
export const fromTryCatch = <T>(
  operation: () => T,
  errorHandler?: (error: unknown) => QiError
): Result<T, QiError> => {
  try {
    return success(operation())
  } catch (error) {
    if (errorHandler) {
      return failure(errorHandler(error))
    }
    // Default error handling - you'll need to import createError from error.ts
    return failure({
      code: 'EXCEPTION',
      message: error instanceof Error ? error.message : String(error),
      category: 'SYSTEM',
      context: { originalError: error }
    } as QiError)
  }
}

/**
 * Create Result from async operation
 * Contract: async variant of fromTryCatch
 */
export const fromAsyncTryCatch = async <T>(
  operation: () => Promise<T>,
  errorHandler?: (error: unknown) => QiError
): Promise<Result<T, QiError>> => {
  try {
    const value = await operation()
    return success(value)
  } catch (error) {
    if (errorHandler) {
      return failure(errorHandler(error))
    }
    return failure({
      code: 'ASYNC_EXCEPTION',
      message: error instanceof Error ? error.message : String(error),
      category: 'SYSTEM',
      context: { originalError: error }
    } as QiError)
  }
}

/**
 * Create Result from nullable value
 * Contract: if value != null: result.isSuccess() == true
 * Contract: if value == null: result.isFailure() == true
 */
export const fromMaybe = <T>(
  value: T | null | undefined,
  errorIfNull: QiError
): Result<T, QiError> => {
  return value != null ? success(value) : failure(errorIfNull)
}

/**
 * Convert from Either-like type
 * Contract: preserves left/right semantics
 */
export const fromEither = <L, R>(
  either: { tag: 'left'; value: L } | { tag: 'right'; value: R }
): Result<R, L> => {
  return either.tag === 'right' 
    ? success(either.value)
    : failure(either.value)
}

// ============================================================================
// Query Properties (Contract Section 1.2)
// ============================================================================

/**
 * Check if Result is success
 * Contract: isSuccess(success(x)) == true
 * Contract: isSuccess(failure(e)) == false
 */
export const isSuccess = <T, E>(result: Result<T, E>): result is Success<T> =>
  result.tag === 'success'

/**
 * Check if Result is failure
 * Contract: isFailure(failure(e)) == true
 * Contract: isFailure(success(x)) == false
 */
export const isFailure = <T, E>(result: Result<T, E>): result is Failure<E> =>
  result.tag === 'failure'

/**
 * Get value or null
 * Contract: getValue(success(x)) == x
 * Contract: getValue(failure(e)) == null
 */
export const getValue = <T, E>(result: Result<T, E>): T | null =>
  result.tag === 'success' ? result.value : null

/**
 * Get error or null
 * Contract: getError(failure(e)) == e
 * Contract: getError(success(x)) == null
 */
export const getError = <T, E>(result: Result<T, E>): E | null =>
  result.tag === 'failure' ? result.error : null

// ============================================================================
// Transformation Operations (Contract Section 1.3)
// ============================================================================

/**
 * Map function over success value
 * Contract: map(id) == id
 * Contract: map(f ∘ g) == map(f) ∘ map(g)
 * Contract: map(f)(success(x)) == success(f(x))
 * Contract: map(f)(failure(e)) == failure(e)
 */
export const map = <T, U, E>(
  fn: (value: T) => U,
  result: Result<T, E>
): Result<U, E> =>
  result.tag === 'success' ? success(fn(result.value)) : result

/**
 * Map function over error value
 * Contract: mapError(f)(success(x)) == success(x)
 * Contract: mapError(f)(failure(e)) == failure(f(e))
 */
export const mapError = <T, E, F>(
  fn: (error: E) => F,
  result: Result<T, E>
): Result<T, F> =>
  result.tag === 'failure' ? failure(fn(result.error)) : result

/**
 * Monadic bind operation
 * Contract: flatMap(f)(success(x)) == f(x)
 * Contract: result.flatMap(success) == result
 * Contract: result.flatMap(f).flatMap(g) == result.flatMap(x => f(x).flatMap(g))
 * Contract: flatMap(f)(failure(e)) == failure(e)
 */
export const flatMap = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  result: Result<T, E>
): Result<U, E> =>
  result.tag === 'success' ? fn(result.value) : result

/**
 * Alias for flatMap with clearer semantics
 * Contract: andThen(f) == flatMap(f)
 */
export const andThen = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  result: Result<T, E>
): Result<U, E> => flatMap(fn, result)

/**
 * Inspect success value without changing it
 * Contract: inspect(f)(success(x)) == success(x) after calling f(x)
 * Contract: inspect(f)(failure(e)) == failure(e) without calling f
 */
export const inspect = <T, E>(
  fn: (value: T) => void,
  result: Result<T, E>
): Result<T, E> => {
  if (result.tag === 'success') {
    fn(result.value)
  }
  return result
}

/**
 * Inspect error value without changing it
 * Contract: inspectErr(f)(failure(e)) == failure(e) after calling f(e)
 * Contract: inspectErr(f)(success(x)) == success(x) without calling f
 */
export const inspectErr = <T, E>(
  fn: (error: E) => void,
  result: Result<T, E>
): Result<T, E> => {
  if (result.tag === 'failure') {
    fn(result.error)
  }
  return result
}

/**
 * Flatten nested Results
 * Contract: collect(success(success(x))) == success(x)
 * Contract: collect(success(failure(e))) == failure(e)
 * Contract: collect(failure(e)) == failure(e)
 */
export const collect = <T, E>(
  result: Result<Result<T, E>, E>
): Result<T, E> =>
  result.tag === 'success' ? result.value : result

/**
 * Filter success values
 * Contract: filter(pred)(success(x)) == success(x) if pred(x) is true
 * Contract: filter(pred)(success(x)) == failure(FILTERED_ERROR) if pred(x) is false
 * Contract: filter(pred)(failure(e)) == failure(e)
 */
export const filter = <T, E>(
  predicate: (value: T) => boolean,
  errorIfFalse: E,
  result: Result<T, E>
): Result<T, E> => {
  if (result.tag === 'success') {
    return predicate(result.value) ? result : failure(errorIfFalse)
  }
  return result
}

/**
 * Provide alternative for failure case
 * Contract: orElse(alt)(success(x)) == success(x)
 * Contract: orElse(alt)(failure(e)) == alt(e)
 */
export const orElse = <T, E, F>(
  alternative: (error: E) => Result<T, F>,
  result: Result<T, E>
): Result<T, F> =>
  result.tag === 'failure' ? alternative(result.error) : result

// ============================================================================
// Extraction Operations (Contract Section 1.4)
// ============================================================================

/**
 * Extract value unsafely
 * Contract: unwrap(success(x)) == x
 * Contract: unwrap(failure(e)) throws exception
 */
export const unwrap = <T, E>(result: Result<T, E>): T => {
  if (result.tag === 'success') {
    return result.value
  }
  throw new Error(`Called unwrap on failure: ${JSON.stringify(result.error)}`)
}

/**
 * Extract value with default
 * Contract: unwrapOr(default)(success(x)) == x
 * Contract: unwrapOr(default)(failure(e)) == default
 */
export const unwrapOr = <T, E>(
  defaultValue: T,
  result: Result<T, E>
): T =>
  result.tag === 'success' ? result.value : defaultValue

/**
 * Pattern matching
 * Contract: match(onSuccess, onError)(success(x)) == onSuccess(x)
 * Contract: match(onSuccess, onError)(failure(e)) == onError(e)
 */
export const match = <T, E, R>(
  onSuccess: (value: T) => R,
  onError: (error: E) => R,
  result: Result<T, E>
): R => {
  switch (result.tag) {
    case 'success':
      return onSuccess(result.value)
    case 'failure':
      return onError(result.error)
  }
}
```

This implementation:

1. **Follows contracts exactly** - only includes operations specified in the behavioral contracts
2. **Pure functional** - no methods on types, only standalone functions
3. **Simple and clear** - ~250 lines vs ~500 in the original
4. **Type-safe** - leverages TypeScript's discriminated unions
5. **No dependencies** - except for QiError type import

Key differences from the current implementation:
- Removed fluent API builder
- Removed async operations (except fromAsyncTryCatch)
- Removed collection operations (sequence, traverse, partition)
- Removed applicative operations (apply, pure)
- Removed convenience methods on Result type
- Simplified to pure functions matching contract signatures

The contracts specify a more "Rust-like" API with functions taking the Result as the last parameter, making it suitable for piping or partial application.