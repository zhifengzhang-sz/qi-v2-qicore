/**
 * QiCore Foundation Base Module
 *
 * Mathematical foundation types for TypeScript implementation.
 * Exports Result<T> and QiError with fluent API patterns.
 */

// Result<T> exports
export type { Result } from './result.js'
export {
  isSuccess,
  isFailure,
  match,
  Ok,
  Err,
  ResultBuilder,
  from,
  tryCatch,
  asyncTryCatch,
  fromNullable,
  sequence,
  traverse,
  traverseAsync,
  combine2,
  partition,
  rights,
  lefts,
  fromPromise,
  asyncSequence,
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
