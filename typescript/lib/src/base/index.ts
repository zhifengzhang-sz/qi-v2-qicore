/**
 * QiCore Foundation Base Module
 *
 * Mathematical foundation types for TypeScript implementation.
 * Exports Result<T> and QiError with fluent API patterns.
 */

// Result<T> exports
export type { Result, Success, Failure } from './result.js'
export {
  // Factory operations
  Ok,
  Err,
  fromNullable,
  tryCatch,
  asyncTryCatch,
  // Query operations
  isSuccess,
  isFailure,
  getValue,
  getError,
  // Transformation operations
  map,
  mapError,
  flatMap,
  andThen,
  filter,
  orElse,
  // Extraction operations
  unwrapOr,
  unwrap,
  match,
  // Collection operations
  sequence,
  traverse,
  partition,
  combine2,
  // Applicative operations
  apply,
  pure,
  // Async operations
  asyncMap,
  asyncAndThen,
  asyncSequence,
  fromPromise,
  toPromise,
} from './result.js'

// QiError exports
export type {
  QiError,
  ErrorCode,
  ErrorCategory,
  RetryStrategy,
} from './error.js'
export {
  ErrorCategories,
  isErrorCategory,
  createErrorCode,
  createError,
  QiErrorBuilder,
  errorBuilder,
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
  getRetryStrategy,
  hasCategory,
  chainError,
  formatErrorChain,
  getRootCause,
  serializeError,
  deserializeError,
  createAggregateError,
  getContext,
  withContext,
} from './error.js'
