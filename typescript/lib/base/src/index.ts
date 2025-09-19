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
export type { Result, Success, Failure } from "./result.js";

// Factory operations
export {
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  fromMaybe,
  fromEither,
} from "./result.js";

// Aliases for compatibility
export { fromTryCatch as tryCatch } from "./result.js";

// Query operations
export {
  isSuccess,
  isFailure,
  getValue,
  getError,
} from "./result.js";

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
} from "./result.js";

// Extraction operations
export {
  unwrap,
  unwrapOr,
  match,
} from "./result.js";

// ============================================================================
// QiError Exports
// ============================================================================

// Type exports
export type {
  QiError,
  ErrorCategory,
  ErrorOptions,
  RetryStrategy,
} from "./error.js";

// Constants
export {
  ErrorCategories,
  isErrorCategory,
} from "./error.js";

// Factory operations
export {
  create,
  createError,
  fromException,
  fromString,
} from "./error.js";

// Query operations
export {
  errorToString,
  getCategory,
  toStructuredData,
} from "./error.js";

// Transformation operations
export { withContext } from "./error.js";

// Utility operations
export { getRetryStrategy } from "./error.js";

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
} from "./error.js";

// ============================================================================
// Async Result Helpers
// ============================================================================

// Async transformation operations
export {
  flatMapAsync,
  mapAsync,
  matchAsync,
} from "./async.js";

// Promise<Result<T>> composition operations
export {
  flatMapPromise,
  mapPromise,
  matchPromise,
} from "./async.js";

// Async collection operations
export {
  sequenceAsync,
  collectAsync,
} from "./async.js";

// Type guards and utilities
export { isPromiseResult } from "./async.js";

// ============================================================================
// Common Aliases for Ergonomics
// ============================================================================

// Rust-style naming aliases
export { success as Ok, failure as Err } from "./result.js";
