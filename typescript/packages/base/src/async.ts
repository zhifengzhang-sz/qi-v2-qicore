/**
 * QiCore Foundation Base - Async Result Helpers
 *
 * Async composition helpers for cleaner Promise<Result<T>> handling.
 * These extensions eliminate manual Promise/Result unwrapping anti-patterns.
 */

import type { QiError } from './error'
import { create } from './error'
import { success, failure } from './result'
import type { Result } from './result'

// ============================================================================
// Async Transformation Operations
// ============================================================================

/**
 * Async version of flatMap for operations returning Promise<Result<T>>
 * Contract: flatMapAsync(f)(failure(e)) == Promise.resolve(failure(e))
 * Contract: flatMapAsync(f)(success(x)) == f(x)
 *
 * @example
 * ```typescript
 * const result = await flatMapAsync(
 *   async config => loadUserData(config), // Returns Promise<Result<User>>
 *   configResult // Result<Config>
 * );
 * ```
 */
export const flatMapAsync = async <T, U, E>(
  fn: (value: T) => Promise<Result<U, E>>,
  result: Result<T, E>
): Promise<Result<U, E>> => {
  if (result.tag === 'failure') return result
  return await fn(result.value)
}

/**
 * Async version of map with automatic error wrapping
 * Contract: mapAsync(f)(failure(e)) == Promise.resolve(failure(e))
 * Contract: mapAsync(f)(success(x)) == Promise.resolve(success(await f(x))) (on success)
 * Contract: mapAsync(f)(success(x)) == Promise.resolve(failure(error)) (on thrown error)
 *
 * @example
 * ```typescript
 * const result = await mapAsync(
 *   async data => processData(data), // Returns Promise<ProcessedData>
 *   dataResult // Result<Data>
 * );
 * ```
 */
export const mapAsync = async <T, U, E>(
  fn: (value: T) => Promise<U>,
  result: Result<T, E>
): Promise<Result<U, E | QiError>> => {
  if (result.tag === 'failure') return result

  try {
    const value = await fn(result.value)
    return success(value)
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    const qiError = create(
      'ASYNC_MAP_ERROR',
      `Async map operation failed: ${errorMessage}`,
      'SYSTEM',
      { originalError: error }
    )
    return failure(qiError)
  }
}

/**
 * Async version of match for async result handling
 * Contract: matchAsync(onSuccess, onError)(failure(e)) == onError(e)
 * Contract: matchAsync(onSuccess, onError)(success(x)) == onSuccess(x)
 *
 * @example
 * ```typescript
 * await matchAsync(
 *   async user => console.log('User:', user.name),
 *   async error => console.error('Failed:', error.message),
 *   userResult
 * );
 * ```
 */
export const matchAsync = async <T, E, R>(
  onSuccess: (value: T) => Promise<R>,
  onError: (error: E) => Promise<R>,
  result: Result<T, E>
): Promise<R> => {
  return result.tag === 'success' ? await onSuccess(result.value) : await onError(result.error)
}

// ============================================================================
// Promise<Result<T>> Composition Operations
// ============================================================================

/**
 * Compose operations on Promise<Result<T>> without manual awaiting
 * Contract: flatMapPromise(f)(Promise.resolve(failure(e))) == Promise.resolve(failure(e))
 * Contract: flatMapPromise(f)(Promise.resolve(success(x))) == f(x) (if f returns Result)
 * Contract: flatMapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(f(x)) (if f returns Promise<Result>)
 *
 * @example
 * ```typescript
 * const result = await flatMapPromise(
 *   config => validateAndLoad(config), // Returns Result<Data> or Promise<Result<Data>>
 *   loadConfigAsync() // Promise<Result<Config>>
 * );
 * ```
 */
export const flatMapPromise = async <T, U, E>(
  fn: (value: T) => Result<U, E> | Promise<Result<U, E>>,
  promiseResult: Promise<Result<T, E>>
): Promise<Result<U, E>> => {
  const result = await promiseResult
  if (result.tag === 'failure') return result

  const mapped = fn(result.value)
  return mapped instanceof Promise ? await mapped : mapped
}

/**
 * Map over Promise<Result<T>> with automatic error handling
 * Contract: mapPromise(f)(Promise.resolve(failure(e))) == Promise.resolve(failure(e))
 * Contract: mapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(success(f(x))) (sync f)
 * Contract: mapPromise(f)(Promise.resolve(success(x))) == Promise.resolve(success(await f(x))) (async f)
 *
 * @example
 * ```typescript
 * const result = await mapPromise(
 *   data => processData(data), // Returns ProcessedData or Promise<ProcessedData>
 *   loadDataAsync() // Promise<Result<Data>>
 * );
 * ```
 */
export const mapPromise = async <T, U, E>(
  fn: (value: T) => U | Promise<U>,
  promiseResult: Promise<Result<T, E>>
): Promise<Result<U, E | QiError>> => {
  const result = await promiseResult
  if (result.tag === 'failure') return result

  try {
    const mapped = fn(result.value)
    const value = mapped instanceof Promise ? await mapped : mapped
    return success(value)
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    const qiError = create(
      'ASYNC_MAP_PROMISE_ERROR',
      `Async promise map operation failed: ${errorMessage}`,
      'SYSTEM',
      { originalError: error }
    )
    return failure(qiError)
  }
}

/**
 * Match over Promise<Result<T>> with async handlers
 * Contract: matchPromise(onSuccess, onError)(Promise.resolve(failure(e))) == onError(e)
 * Contract: matchPromise(onSuccess, onError)(Promise.resolve(success(x))) == onSuccess(x)
 *
 * @example
 * ```typescript
 * await matchPromise(
 *   async user => console.log('Loaded user:', user.name),
 *   async error => console.error('Load failed:', error.message),
 *   loadUserAsync() // Promise<Result<User>>
 * );
 * ```
 */
export const matchPromise = async <T, E, R>(
  onSuccess: (value: T) => Promise<R>,
  onError: (error: E) => Promise<R>,
  promiseResult: Promise<Result<T, E>>
): Promise<R> => {
  const result = await promiseResult
  return result.tag === 'success' ? await onSuccess(result.value) : await onError(result.error)
}

// ============================================================================
// Async Collection Operations
// ============================================================================

/**
 * Sequence async operations, stopping at first failure
 * Contract: sequenceAsync([]) == Promise.resolve(success([]))
 * Contract: sequenceAsync([success(x)]) == Promise.resolve(success([x]))
 * Contract: sequenceAsync([failure(e), ...]) == Promise.resolve(failure(e))
 *
 * @example
 * ```typescript
 * const results = await sequenceAsync([
 *   loadUser('1'),
 *   loadUser('2'),
 *   loadUser('3')
 * ]);
 * // Result<User[], QiError>
 * ```
 */
export const sequenceAsync = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T[], E>> => {
  const values: T[] = []

  for (const promise of promises) {
    const result = await promise
    if (result.tag === 'failure') return result
    values.push(result.value)
  }

  return success(values)
}

/**
 * Collect all async operations, preserving both successes and failures
 * Contract: collectAsync([]) == Promise.resolve({ successes: [], failures: [] })
 * Contract: collectAsync(promises) preserves order and partitions results
 *
 * @example
 * ```typescript
 * const { successes, failures } = await collectAsync([
 *   loadUser('1'),
 *   loadUser('invalid'),
 *   loadUser('3')
 * ]);
 * // successes: User[], failures: QiError[]
 * ```
 */
export const collectAsync = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<{ successes: T[]; failures: E[] }> => {
  const results = await Promise.all(promises)
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

// ============================================================================
// Type Guards and Utilities
// ============================================================================

/**
 * Type guard for Promise<Result<T>>
 * Useful for conditional async result handling
 */
export const isPromiseResult = <T, E>(value: unknown): value is Promise<Result<T, E>> => {
  return value instanceof Promise
}
