/**
 * QiCore Foundation Base - Result<T> Implementation
 *
 * TypeScript-native implementation of Result<T> using discriminated unions
 * and fluent API patterns while preserving mathematical laws from Haskell reference.
 */

import type { QiError } from './error.js'

/**
 * Result<T> represents a computation that either succeeds with value T or fails with error E.
 *
 * Uses TypeScript discriminated unions for compile-time safety and runtime type checking.
 * The 'tag' field enables exhaustive pattern matching and type narrowing.
 */
export type Result<T, E = QiError> =
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

/**
 * Type guard to check if Result is in success state
 */
export const isSuccess = <T, E>(
  result: Result<T, E>
): result is { readonly tag: 'success'; readonly value: T } => result.tag === 'success'

/**
 * Type guard to check if Result is in failure state
 */
export const isFailure = <T, E>(
  result: Result<T, E>
): result is { readonly tag: 'failure'; readonly error: E } => result.tag === 'failure'

/**
 * Pattern matching for Result<T> with exhaustive checking
 *
 * @param result - The Result to match against
 * @param cases - Object with success and failure handlers
 * @returns The result of the appropriate handler
 */
export const match = <T, E, R>(
  result: Result<T, E>,
  cases: {
    success: (value: T) => R
    failure: (error: E) => R
  }
): R => {
  switch (result.tag) {
    case 'success':
      return cases.success(result.value)
    case 'failure':
      return cases.failure(result.error)
  }
}

/**
 * Factory function to create successful Result
 */
export const Ok = <T, E = QiError>(value: T): Result<T, E> => ({
  tag: 'success',
  value,
})

/**
 * Factory function to create failed Result
 */
export const Err = <E = QiError, T = unknown>(error: E): Result<T, E> => ({
  tag: 'failure',
  error,
})

/**
 * Fluent API Builder for Result<T> operations
 *
 * Provides method chaining while preserving mathematical laws.
 * All operations are immutable and return new builder instances.
 */
export class ResultBuilder<T, E = QiError> {
  constructor(private readonly result: Result<T, E>) {}

  /**
   * Functor map operation - transforms success value, preserves failures
   *
   * Mathematical law: Functor identity and composition laws
   * - map(id) === id
   * - map(f ∘ g) === map(f) ∘ map(g)
   */
  map<U>(fn: (value: T) => U): ResultBuilder<U, E> {
    return new ResultBuilder(
      this.result.tag === 'success' ? { tag: 'success', value: fn(this.result.value) } : this.result
    )
  }

  /**
   * Monad flatMap operation - enables chaining operations that return Results
   *
   * Mathematical laws: Monad left identity, right identity, associativity
   * - flatMap(x => Ok(x)) === id
   * - Ok(x).flatMap(f) === f(x)
   * - m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))
   */
  flatMap<U>(fn: (value: T) => Result<U, E>): ResultBuilder<U, E> {
    return new ResultBuilder(this.result.tag === 'success' ? fn(this.result.value) : this.result)
  }

  /**
   * Filter operation with custom error generation
   *
   * Converts success to failure if predicate fails
   */
  filter(predicate: (value: T) => boolean, errorFn: () => E): ResultBuilder<T, E> {
    return new ResultBuilder(
      this.result.tag === 'success' && predicate(this.result.value)
        ? this.result
        : this.result.tag === 'success'
          ? { tag: 'failure', error: errorFn() }
          : this.result
    )
  }

  /**
   * Applicative apply operation for combining Results
   *
   * Mathematical laws: Applicative identity, composition, homomorphism, interchange
   */
  apply<U>(fnResult: Result<(value: T) => U, E>): ResultBuilder<U, E> {
    if (this.result.tag === 'success' && fnResult.tag === 'success') {
      return new ResultBuilder({ tag: 'success', value: fnResult.value(this.result.value) })
    }
    if (this.result.tag === 'failure') {
      return new ResultBuilder(this.result)
    }
    return new ResultBuilder(fnResult as Result<U, E>)
  }

  /**
   * Async map operation for Promise-returning functions
   *
   * Integrates with TypeScript's async/await patterns
   */
  async mapAsync<U>(fn: (value: T) => Promise<U>): Promise<ResultBuilder<U, E | Error>> {
    if (this.result.tag === 'success') {
      try {
        const value = await fn(this.result.value)
        return new ResultBuilder({ tag: 'success', value })
      } catch (error) {
        return new ResultBuilder({
          tag: 'failure',
          error: error as E | Error,
        })
      }
    }
    return new ResultBuilder(this.result as Result<U, E>)
  }

  /**
   * Async flatMap operation
   */
  async flatMapAsync<U>(fn: (value: T) => Promise<Result<U, E>>): Promise<ResultBuilder<U, E>> {
    if (this.result.tag === 'success') {
      try {
        const result = await fn(this.result.value)
        return new ResultBuilder(result)
      } catch (error) {
        return new ResultBuilder({
          tag: 'failure',
          error: error as E,
        })
      }
    }
    return new ResultBuilder(this.result as Result<U, E>)
  }

  /**
   * Unwrap success value or throw error
   *
   * Use sparingly - prefer pattern matching for safety
   */
  unwrap(): T {
    if (this.result.tag === 'success') {
      return this.result.value
    }
    throw new Error(`Cannot unwrap failed Result: ${JSON.stringify(this.result.error)}`)
  }

  /**
   * Unwrap success value or return default
   */
  unwrapOr(defaultValue: T): T {
    return this.result.tag === 'success' ? this.result.value : defaultValue
  }

  /**
   * Unwrap success value or compute default from error
   */
  unwrapOrElse(fn: (error: E) => T): T {
    return this.result.tag === 'success' ? this.result.value : fn(this.result.error)
  }

  /**
   * Build final Result - terminates fluent chain
   */
  build(): Result<T, E> {
    return this.result
  }

  /**
   * Tap operation for side effects without changing the Result
   */
  tap(fn: (value: T) => void): ResultBuilder<T, E> {
    if (this.result.tag === 'success') {
      fn(this.result.value)
    }
    return this
  }

  /**
   * TapError operation for side effects on errors
   */
  tapError(fn: (error: E) => void): ResultBuilder<T, E> {
    if (this.result.tag === 'failure') {
      fn(this.result.error)
    }
    return this
  }

  /**
   * Error recovery operation - provides alternative Result on failure
   */
  orElse(fn: (error: E) => Result<T, E>): ResultBuilder<T, E> {
    if (this.result.tag === 'failure') {
      return new ResultBuilder(fn(this.result.error))
    }
    return this
  }

  /**
   * Convert Result to Promise (throws on failure)
   */
  toPromise(): Promise<T> {
    if (this.result.tag === 'success') {
      return Promise.resolve(this.result.value)
    }
    return Promise.reject(this.result.error)
  }
}

/**
 * Entry point to fluent API - wraps Result in builder
 */
export const from = <T, E = QiError>(result: Result<T, E>): ResultBuilder<T, E> =>
  new ResultBuilder(result)

/**
 * Try-catch wrapper that returns Result instead of throwing
 */
export const tryCatch = <T>(fn: () => T): Result<T, Error> => {
  try {
    return Ok(fn())
  } catch (error) {
    return Err(error instanceof Error ? error : new Error(String(error)))
  }
}

/**
 * Async try-catch wrapper
 */
export const asyncTryCatch = async <T>(fn: () => Promise<T>): Promise<Result<T, Error>> => {
  try {
    const value = await fn()
    return Ok(value)
  } catch (error) {
    return Err(error instanceof Error ? error : new Error(String(error)))
  }
}

/**
 * Convert nullable value to Result
 */
export const fromNullable = <T>(value: T | null | undefined, error: QiError): Result<T, QiError> =>
  value != null ? Ok(value) : Err(error)

/**
 * Sequence array of Results into Result of array
 * Fails fast on first error
 */
export const sequence = <T, E>(results: Result<T, E>[]): Result<T[], E> => {
  const values: T[] = []
  for (const result of results) {
    if (result.tag === 'failure') {
      return result
    }
    values.push(result.value)
  }
  return Ok(values)
}

/**
 * Traverse array applying function that returns Result
 */
export const traverse = <T, U, E>(items: T[], fn: (item: T) => Result<U, E>): Result<U[], E> => {
  const results = items.map(fn)
  return sequence(results)
}

/**
 * Async traverse
 */
export const traverseAsync = async <T, U, E>(
  items: T[],
  fn: (item: T) => Promise<Result<U, E>>
): Promise<Result<U[], E>> => {
  const results = await Promise.all(items.map(fn))
  return sequence(results)
}

/**
 * Combine two Results using a binary function
 */
export const combine2 = <T1, T2, U, E>(
  result1: Result<T1, E>,
  result2: Result<T2, E>,
  fn: (value1: T1, value2: T2) => U
): Result<U, E> => {
  if (result1.tag === 'success' && result2.tag === 'success') {
    return Ok(fn(result1.value, result2.value))
  }
  if (result1.tag === 'failure') {
    return result1 as Result<U, E>
  }
  return result2 as Result<U, E>
}

/**
 * Partition array of Results into successes and failures
 */
export const partition = <T, E>(
  results: Result<T, E>[]
): {
  successes: T[]
  failures: E[]
} => {
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
 * Extract all successful values from Result array
 */
export const rights = <T, E>(results: Result<T, E>[]): T[] => {
  const values: T[] = []
  for (const result of results) {
    if (result.tag === 'success') {
      values.push(result.value)
    }
  }
  return values
}

/**
 * Extract all error values from Result array
 */
export const lefts = <T, E>(results: Result<T, E>[]): E[] => {
  const errors: E[] = []
  for (const result of results) {
    if (result.tag === 'failure') {
      errors.push(result.error)
    }
  }
  return errors
}

/**
 * Convert Promise to Result (catches rejections as failures)
 */
export const fromPromise = async <T>(promise: Promise<T>): Promise<Result<T, Error>> => {
  try {
    const value = await promise
    return Ok(value)
  } catch (error) {
    return Err(error instanceof Error ? error : new Error(String(error)))
  }
}

/**
 * Process array of Promise<Result<T>> and return Result of array
 */
export const asyncSequence = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T[], E>> => {
  const results = await Promise.all(promises)
  return sequence(results)
}
