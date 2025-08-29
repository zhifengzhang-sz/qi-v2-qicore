/**
 * QiCore Foundation Base Module
 *
 * Mathematical foundation types for TypeScript implementation.
 * Exports Result<T> and QiError with pure functional patterns.
 */

// ============================================================================
// Result<T> Exports
// ============================================================================

// Type exports
export type { Result, Success, Failure } from './result'

// Factory operations
export {
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  fromMaybe,
  fromEither,
} from './result'

// Aliases for compatibility
export { fromTryCatch as tryCatch } from './result'

// Query operations
export {
  isSuccess,
  isFailure,
  getValue,
  getError,
} from './result'

// Transformation operations
export {
  map,
  mapError,
  flatMap,
  andThen,
  inspect,
  inspectErr,
  collect,
  filter,
  orElse,
} from './result'

// Extraction operations
export {
  unwrap,
  unwrapOr,
  match,
} from './result'

// ============================================================================
// QiError Exports
// ============================================================================

// Type exports
export type {
  QiError,
  ErrorCategory,
  ErrorOptions,
  RetryStrategy,
} from './error'

// Constants
export {
  ErrorCategories,
  isErrorCategory,
} from './error'

// Factory operations
export {
  create,
  createError,
  fromException,
  fromString,
} from './error'

// Query operations
export {
  errorToString,
  getCategory,
  toStructuredData,
} from './error'

// Transformation operations
export { withContext } from './error'

// Utility operations
export { getRetryStrategy } from './error'

// Convenience factories
export {
  validationError,
  networkError,
  systemError,
  businessError,
  authenticationError,
  authorizationError,
  configurationError,
  timeoutError,
  resourceError,
  concurrencyError,
  loggerError,
} from './error'

// ============================================================================
// Async Result Helpers
// ============================================================================

// Async transformation operations
export {
  flatMapAsync,
  mapAsync,
  matchAsync,
} from './async'

// Promise<Result<T>> composition operations
export {
  flatMapPromise,
  mapPromise,
  matchPromise,
} from './async'

// Async collection operations
export {
  sequenceAsync,
  collectAsync,
} from './async'

// Type guards and utilities
export { isPromiseResult } from './async'

// ============================================================================
// Common Aliases for Ergonomics
// ============================================================================

// Rust-style naming aliases
export { success as Ok, failure as Err } from './result'
