Here's the contract-compliant `index.ts` for the base module:

```typescript
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
export type { Result, Success, Failure } from './result.js'

// Factory operations
export { 
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  fromMaybe,
  fromEither
} from './result.js'

// Query operations  
export {
  isSuccess,
  isFailure,
  getValue,
  getError
} from './result.js'

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
  orElse
} from './result.js'

// Extraction operations
export {
  unwrap,
  unwrapOr,
  match
} from './result.js'

// ============================================================================
// QiError Exports
// ============================================================================

// Type exports
export type { 
  QiError,
  ErrorCategory,
  ErrorOptions,
  RetryStrategy
} from './error.js'

// Constants
export {
  ErrorCategories,
  isErrorCategory
} from './error.js'

// Factory operations
export {
  create,
  createError,
  fromException,
  fromString,
  loggerError
} from './error.js'

// Query operations
export {
  toString,
  getCategory,
  toStructuredData
} from './error.js'

// Transformation operations
export {
  withContext
} from './error.js'

// Utility operations
export {
  getRetryStrategy
} from './error.js'

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
  concurrencyError
} from './error.js'

// ============================================================================
// Common Aliases for Ergonomics
// ============================================================================

// Rust-style naming aliases
export { success as Ok, failure as Err } from './result.js'
```

This index.ts:

1. **Exports only contract-specified operations** - no extras
2. **Clear organization** - separated by type (Result vs QiError) and operation category
3. **Includes type exports** - all necessary types for consumers
4. **Provides common aliases** - Ok/Err for those familiar with Rust
5. **Uses .js extensions** - required for ESM compatibility

Key differences from the current implementation:
- Removed FluentResultBuilder and related exports
- Removed async operations (asyncMap, asyncAndThen, asyncSequence, fromPromise, toPromise)
- Removed collection operations (sequence, traverse, partition, combine2)
- Removed applicative operations (apply, pure)
- Removed extra error operations (chainError, formatErrorChain, getRootCause, serializeError, etc.)
- Removed QiErrorBuilder and errorBuilder
- Much simpler and focused on contract-specified operations only

The module now exports exactly what the contracts specify, nothing more, nothing less.