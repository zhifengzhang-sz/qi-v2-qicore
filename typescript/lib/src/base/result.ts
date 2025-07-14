/**
 * QiCore Foundation Base - Result<T> Implementation
 *
 * Pure discriminated union implementation with standalone functions
 * following the Haskell reference implementation exactly.
 */

import type { QiError } from './error.js'

/**
 * Result<T> - Pure discriminated union (mirrors Haskell's Either QiError a)
 */
export type Result<T, E = QiError> =
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

/**
 * Success variant type for cleaner type guards
 */
export type Success<T, _E = QiError> = { readonly tag: 'success'; readonly value: T }

/**
 * Failure variant type for cleaner type guards
 */
export type Failure<E, _T = unknown> = { readonly tag: 'failure'; readonly error: E }

// ============================================================================
// Factory Operations (Contract Section 1.1)
// ============================================================================

/**
 * Success constructor (pure)
 * Contract: success(x).isSuccess() == true
 */
export const Ok = <T>(value: T): Result<T, never> => ({ tag: 'success', value })

/**
 * Failure constructor (pure)
 * Contract: failure(e).isFailure() == true
 */
export const Err = <E>(error: E): Result<never, E> => ({ tag: 'failure', error })

/**
 * Convert from nullable value with custom error
 * Contract: if value != null: result.isSuccess() == true
 * Contract: if value == null: result.isFailure() == true
 */
export const fromNullable = <T>(
  value: T | null | undefined,
  error: QiError
): Result<T, QiError> => {
  return value != null ? Ok(value) : Err(error)
}

/**
 * Safe execution utilities
 */
export const tryCatch = <T>(fn: () => T): Result<T, Error> => {
  try {
    return Ok(fn())
  } catch (error) {
    return Err(error as Error)
  }
}

export const asyncTryCatch = async <T>(fn: () => Promise<T>): Promise<Result<T, Error>> => {
  try {
    const value = await fn()
    return Ok(value)
  } catch (error) {
    return Err(error as Error)
  }
}

// ============================================================================
// Query Properties (Contract Section 1.2)
// ============================================================================

/**
 * Type guard to check if Result is in success state
 * Contract: isSuccess(success(x)) == true
 * Contract: isSuccess(failure(e)) == false
 */
export const isSuccess = <T, E>(result: Result<T, E>): result is Success<T, E> =>
  result.tag === 'success'

/**
 * Type guard to check if Result is in failure state
 * Contract: isFailure(failure(e)) == true
 * Contract: isFailure(success(x)) == false
 */
export const isFailure = <T, E>(result: Result<T, E>): result is Failure<E, T> =>
  result.tag === 'failure'

/**
 * Get success data or null
 * Contract: getValue(success(x)) == x
 * Contract: getValue(failure(e)) == null
 */
export const getValue = <T, E>(result: Result<T, E>): T | undefined => {
  return result.tag === 'success' ? result.value : undefined
}

/**
 * Get error or null
 * Contract: getError(failure(e)) == e
 * Contract: getError(success(x)) == null
 */
export const getError = <T, E>(result: Result<T, E>): E | undefined => {
  return result.tag === 'failure' ? result.error : undefined
}

// ============================================================================
// Transformation Operations (Contract Section 1.3)
// ============================================================================

/**
 * Functor map operation
 * Contract: Functor Laws
 * Contract: identity: map(id, result) == result
 * Contract: composition: map(f, map(g, result)) == map(compose(f, g), result)
 * Contract: map(f, success(x)) == success(f(x))
 * Contract: map(f, failure(e)) == failure(e)
 */
export const map = <T, U, E>(fn: (value: T) => U, result: Result<T, E>): Result<U, E> => {
  return result.tag === 'success' ? Ok(fn(result.value)) : result
}

/**
 * Map over error value
 * Contract: mapError(f, success(x)) == success(x)
 * Contract: mapError(f, failure(e)) == failure(f(e))
 */
export const mapError = <T, E1, E2>(
  fn: (error: E1) => E2,
  result: Result<T, E1>
): Result<T, E2> => {
  return result.tag === 'failure' ? Err(fn(result.error)) : result
}

/**
 * Monadic bind (flatMap)
 * Contract: Monad Laws
 * Contract: left identity: flatMap(f, success(x)) == f(x)
 * Contract: right identity: flatMap(success, result) == result
 * Contract: associativity: flatMap(g, flatMap(f, result)) == flatMap(x => flatMap(g, f(x)), result)
 * Contract: flatMap(f, failure(e)) == failure(e)
 */
export const flatMap = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  result: Result<T, E>
): Result<U, E> => {
  return result.tag === 'success' ? fn(result.value) : result
}

/**
 * Rust-style andThen (alias for flatMap with arguments flipped)
 * Contract: andThen(result, f) == flatMap(f, result)
 */
export const andThen = <T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Result<U, E>
): Result<U, E> => {
  return flatMap(fn, result)
}

/**
 * Filter with predicate
 * Contract: filter(pred, error, success(x)) == success(x) if pred(x) is true
 * Contract: filter(pred, error, success(x)) == failure(error) if pred(x) is false
 * Contract: filter(pred, error, failure(e)) == failure(e)
 */
export const filter = <T, E>(
  predicate: (value: T) => boolean,
  error: E,
  result: Result<T, E>
): Result<T, E> => {
  if (result.tag === 'success') {
    return predicate(result.value) ? result : Err(error)
  }
  return result
}

/**
 * Error recovery mechanism
 * Contract: orElse(alt, success(x)) == success(x)
 * Contract: orElse(alt, failure(e)) == alt(e)
 */
export const orElse = <T, E>(
  fn: (error: E) => Result<T, E>,
  result: Result<T, E>
): Result<T, E> => {
  return result.tag === 'success' ? result : fn(result.error)
}

// ============================================================================
// Extraction Operations (Contract Section 1.4)
// ============================================================================

/**
 * Extract value or return default
 * Contract: unwrapOr(default, success(x)) == x
 * Contract: unwrapOr(default, failure(e)) == default
 */
export const unwrapOr = <T, E>(defaultValue: T, result: Result<T, E>): T => {
  return result.tag === 'success' ? result.value : defaultValue
}

/**
 * Extract value or throw error (partial function)
 * Contract: unwrap(success(x)) == x
 * Contract: unwrap(failure(e)) throws exception
 */
export const unwrap = <T, E>(result: Result<T, E>): T => {
  if (result.tag === 'success') {
    return result.value
  }
  throw new Error(`Result unwrap failed: ${result.error}`)
}

/**
 * Pattern matching with continuation functions
 * Contract: match(onSuccess, onError, success(x)) == onSuccess(x)
 * Contract: match(onSuccess, onError, failure(e)) == onError(e)
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

// ============================================================================
// Collection Operations (Contract Section 1.5)
// ============================================================================

/**
 * Sequence list of Results (fail-fast)
 * Contract: if all success: returns success with all values
 * Contract: if any failure: returns first failure (fail-fast)
 * Contract: preserves order: success values maintain input order
 */
export const sequence = <T, E>(results: Result<T, E>[]): Result<T[], E> => {
  const values: T[] = []
  for (const result of results) {
    if (result.tag === 'failure') {
      return result as Result<T[], E>
    }
    values.push(result.value)
  }
  return Ok(values)
}

/**
 * Map and sequence in one operation
 * Contract: traverse(f, values) == sequence(map(f, values))
 */
export const traverse = <T, U, E>(fn: (value: T) => Result<U, E>, values: T[]): Result<U[], E> => {
  return sequence(values.map(fn))
}

/**
 * Split Results into separate success and error lists
 * Contract: splits Results into separate success and error lists
 * Contract: preserves order within each list
 * Contract: partition(results) == { successes, failures }
 */
export const partition = <T, E>(results: Result<T, E>[]): { successes: T[]; failures: E[] } => {
  const successes: T[] = []
  const failures: E[] = []

  for (const result of results) {
    if (result.tag === 'success') {
      successes.push(result.value)
    } else {
      failures.push(result.error)
    }
  }

  return { successes, failures }
}

/**
 * Applicative combination of two Results
 * Contract: combine2(success(x), success(y), f) == success(f(x, y))
 * Contract: if either Result is failure, return first failure
 */
export const combine2 = <T, U, V, E>(
  result1: Result<T, E>,
  result2: Result<U, E>,
  fn: (value1: T, value2: U) => V
): Result<V, E> => {
  if (result1.tag === 'success' && result2.tag === 'success') {
    return Ok(fn(result1.value, result2.value))
  }
  if (result1.tag === 'failure') {
    return result1 as unknown as Result<V, E>
  }
  return result2 as unknown as Result<V, E>
}

// ============================================================================
// Applicative Operations (Contract Section 1.6)
// ============================================================================

/**
 * Applicative function application
 * Contract: Applicative Laws
 * Contract: identity: apply(success(id), result) == result
 * Contract: composition: apply(apply(apply(success(compose), f), g), x) == apply(f, apply(g, x))
 * Contract: homomorphism: apply(success(f), success(x)) == success(f(x))
 * Contract: interchange: apply(f, success(x)) == apply(success(f => f(x)), f)
 */
export const apply = <T, U, E>(
  fnResult: Result<(value: T) => U, E>,
  result: Result<T, E>
): Result<U, E> => {
  if (fnResult.tag === 'success' && result.tag === 'success') {
    return Ok(fnResult.value(result.value))
  }
  if (fnResult.tag === 'failure') {
    return fnResult as unknown as Result<U, E>
  }
  return result as unknown as Result<U, E>
}

/**
 * Applicative unit operation
 * Contract: pure(x) == success(x)
 */
export const pure = <T>(value: T): Result<T, never> => Ok(value)

// ============================================================================
// Async Operations (Contract Section 1.7)
// ============================================================================

/**
 * Async map operation
 * Contract: asyncMap(f, success(x)) == Promise.resolve(success(await f(x)))
 * Contract: asyncMap(f, failure(e)) == Promise.resolve(failure(e))
 */
export const asyncMap = async <T, U, E>(
  fn: (value: T) => Promise<U>,
  result: Result<T, E>
): Promise<Result<U, E | Error>> => {
  if (result.tag === 'success') {
    try {
      const value = await fn(result.value)
      return Ok(value)
    } catch (error) {
      return Err(error as Error) as Result<U, E | Error>
    }
  }
  return result as unknown as Result<U, E | Error>
}

/**
 * Async andThen (monadic bind for async Results)
 * Contract: asyncAndThen(result, f) == f(result.value) if success
 * Contract: asyncAndThen(result, f) == Promise.resolve(failure(e)) if failure
 */
export const asyncAndThen = async <T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Promise<Result<U, E>>
): Promise<Result<U, E>> => {
  return result.tag === 'success' ? fn(result.value) : result
}

/**
 * Parallel sequence with fail-fast
 * Contract: resolves all promises, then applies sequence logic
 * Contract: if all Results are success: returns success with all values
 * Contract: if any Result is failure: returns first failure
 */
export const asyncSequence = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T[], E>> => {
  const results = await Promise.all(promises)
  return sequence(results)
}

/**
 * Convert Promise<T> to Promise<Result<T>>
 * Contract: Promise resolution becomes success
 * Contract: Promise rejection becomes failure with Error
 */
export const fromPromise = async <T>(promise: Promise<T>): Promise<Result<T, Error>> => {
  try {
    const value = await promise
    return Ok(value)
  } catch (error) {
    return Err(error as Error)
  }
}

/**
 * Convert Result<T> to Promise<T>
 * Contract: success(x) becomes Promise.resolve(x)
 * Contract: failure(e) becomes Promise.reject(e)
 */
export const toPromise = async <T, E>(result: Result<T, E>): Promise<T> => {
  if (result.tag === 'success') {
    return result.value
  }
  throw result.error
}
