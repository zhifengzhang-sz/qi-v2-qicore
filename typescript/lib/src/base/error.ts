/**
 * QiCore Foundation Base - QiError Implementation
 *
 * TypeScript-native error handling with branded types and structured error information.
 * Follows the mathematical logic revealed by Haskell while leveraging TypeScript strengths.
 */

/**
 * Branded type for error codes to prevent string confusion at compile time
 */
export type ErrorCode = string & { readonly __brand: unique symbol }

/**
 * Error categories for systematic error handling and retry logic
 *
 * Maps directly to Haskell ErrorCategory for cross-language consistency
 */
export type ErrorCategory =
  | 'VALIDATION' // Input validation failures - no retry
  | 'NETWORK' // Network connectivity issues - retry with backoff
  | 'SYSTEM' // System/infrastructure failures - limited retry
  | 'BUSINESS' // Business logic violations - no retry
  | 'AUTHENTICATION' // Authentication failures - no retry
  | 'AUTHORIZATION' // Authorization/permission failures - no retry
  | 'CONFIGURATION' // Configuration/setup errors - no retry
  | 'TIMEOUT' // Timeout errors - retry with backoff
  | 'RESOURCE' // Resource exhaustion/unavailable - retry with backoff
  | 'CONCURRENCY' // Concurrency conflicts - retry with jitter
  | 'LOGGER' // Logger-related errors - no retry

/**
 * Comprehensive error information structure
 *
 * Captures all necessary context for debugging, logging, and error handling
 */
export interface QiError {
  readonly code: ErrorCode
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Record<string, unknown>
  readonly cause?: QiError
  readonly timestamp: Date
  readonly severity?: 'low' | 'medium' | 'high'
}

/**
 * Retry strategy configuration based on error category
 */
export interface RetryStrategy {
  readonly shouldRetry: boolean
  readonly maxRetries: number
  readonly backoffMs?: number
  readonly maxBackoffMs?: number
}

/**
 * Error category constants for easy access
 */
export const ErrorCategories: ReadonlyArray<ErrorCategory> = [
  'VALIDATION',
  'NETWORK',
  'SYSTEM',
  'BUSINESS',
  'AUTHENTICATION',
  'AUTHORIZATION',
  'CONFIGURATION',
  'TIMEOUT',
  'RESOURCE',
  'CONCURRENCY',
  'LOGGER',
] as const

/**
 * Type guard for error categories
 */
export const isErrorCategory = (value: string): value is ErrorCategory =>
  ErrorCategories.includes(value as ErrorCategory)

/**
 * Create branded error code from string
 */
export const createErrorCode = (code: string): ErrorCode => code as ErrorCode

/**
 * Factory function for creating QiError instances
 */
export const createError = (params: {
  code: string | ErrorCode
  message: string
  category: ErrorCategory
  context?: Record<string, unknown>
  cause?: QiError
  severity?: 'low' | 'medium' | 'high'
}): QiError => {
  const error: QiError = {
    code: typeof params.code === 'string' ? createErrorCode(params.code) : params.code,
    message: params.message,
    category: params.category,
    context: params.context ?? {},
    timestamp: new Date(),
  }

  if (params.cause) {
    ;(error as QiError & { cause: QiError }).cause = params.cause
  }

  if (params.severity) {
    ;(error as QiError & { severity: 'low' | 'medium' | 'high' }).severity = params.severity
  }

  return error
}

/**
 * Fluent builder for complex error construction
 */
export class QiErrorBuilder {
  private code?: ErrorCode
  private message?: string
  private category?: ErrorCategory
  private context: Record<string, unknown> = {}
  private cause?: QiError
  private severity?: 'low' | 'medium' | 'high'

  withCode(code: string | ErrorCode): QiErrorBuilder {
    this.code = typeof code === 'string' ? createErrorCode(code) : code
    return this
  }

  withMessage(message: string): QiErrorBuilder {
    this.message = message
    return this
  }

  withCategory(category: ErrorCategory): QiErrorBuilder {
    this.category = category
    return this
  }

  withContext(context: Record<string, unknown>): QiErrorBuilder {
    this.context = { ...this.context, ...context }
    return this
  }

  withCause(cause: QiError): QiErrorBuilder {
    this.cause = cause
    return this
  }

  withSeverity(severity: 'low' | 'medium' | 'high'): QiErrorBuilder {
    this.severity = severity
    return this
  }

  build(): QiError {
    if (!this.code || !this.message || !this.category) {
      throw new Error('QiError requires code, message, and category')
    }

    const error: QiError = {
      code: this.code,
      message: this.message,
      category: this.category,
      context: this.context,
      timestamp: new Date(),
    }

    if (this.cause) {
      ;(error as QiError & { cause: QiError }).cause = this.cause
    }

    if (this.severity) {
      ;(error as QiError & { severity: 'low' | 'medium' | 'high' }).severity = this.severity
    }

    return error
  }
}

/**
 * Create QiError builder for fluent construction
 */
export const errorBuilder = (): QiErrorBuilder => new QiErrorBuilder()

/**
 * Predefined error factory functions for common cases
 */
export const validationError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'VALIDATION_FAILED',
    message,
    category: 'VALIDATION',
    ...(context && { context }),
    severity: 'medium',
  })

export const networkError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'NETWORK_ERROR',
    message,
    category: 'NETWORK',
    ...(context && { context }),
    severity: 'high',
  })

export const systemError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'SYSTEM_ERROR',
    message,
    category: 'SYSTEM',
    ...(context && { context }),
    severity: 'high',
  })

export const businessError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'BUSINESS_LOGIC_ERROR',
    message,
    category: 'BUSINESS',
    ...(context && { context }),
    severity: 'medium',
  })

export const authenticationError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'AUTHENTICATION_FAILED',
    message,
    category: 'AUTHENTICATION',
    ...(context && { context }),
    severity: 'high',
  })

export const authorizationError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'AUTHORIZATION_FAILED',
    message,
    category: 'AUTHORIZATION',
    ...(context && { context }),
    severity: 'high',
  })

export const configurationError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'CONFIGURATION_ERROR',
    message,
    category: 'CONFIGURATION',
    ...(context && { context }),
    severity: 'high',
  })

export const timeoutError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'TIMEOUT_ERROR',
    message,
    category: 'TIMEOUT',
    ...(context && { context }),
    severity: 'medium',
  })

export const resourceError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'RESOURCE_ERROR',
    message,
    category: 'RESOURCE',
    ...(context && { context }),
    severity: 'high',
  })

export const concurrencyError = (message: string, context?: Record<string, unknown>): QiError =>
  createError({
    code: 'CONCURRENCY_ERROR',
    message,
    category: 'CONCURRENCY',
    ...(context && { context }),
    severity: 'medium',
  })

/**
 * Get retry strategy based on error category
 * Maintains consistency with Haskell implementation
 */
export const getRetryStrategy = (error: QiError): RetryStrategy => {
  switch (error.category) {
    case 'VALIDATION':
      return { shouldRetry: false, maxRetries: 0 }
    case 'NETWORK':
      return {
        shouldRetry: true,
        maxRetries: 3,
        backoffMs: 1000,
        maxBackoffMs: 30000,
      }
    case 'SYSTEM':
      return {
        shouldRetry: true,
        maxRetries: 2,
        backoffMs: 5000,
        maxBackoffMs: 60000,
      }
    case 'BUSINESS':
      return { shouldRetry: false, maxRetries: 0 }
    case 'AUTHENTICATION':
      return { shouldRetry: false, maxRetries: 0 }
    case 'AUTHORIZATION':
      return { shouldRetry: false, maxRetries: 0 }
    case 'CONFIGURATION':
      return { shouldRetry: false, maxRetries: 0 }
    case 'TIMEOUT':
      return {
        shouldRetry: true,
        maxRetries: 2,
        backoffMs: 2000,
        maxBackoffMs: 30000,
      }
    case 'RESOURCE':
      return {
        shouldRetry: true,
        maxRetries: 3,
        backoffMs: 1000,
        maxBackoffMs: 60000,
      }
    case 'CONCURRENCY':
      return {
        shouldRetry: true,
        maxRetries: 5,
        backoffMs: 100,
        maxBackoffMs: 5000,
      }
    case 'LOGGER':
      return { shouldRetry: false, maxRetries: 0 }
    default:
      return { shouldRetry: false, maxRetries: 0 }
  }
}

/**
 * Check if error has specific category
 */
export const hasCategory = (error: QiError, category: ErrorCategory): boolean =>
  error.category === category

/**
 * Chain errors - add current error as cause of new error
 */
export const chainError = (
  error: QiError,
  newError: Omit<QiError, 'cause' | 'timestamp'>
): QiError => {
  const chained: QiError = {
    ...newError,
    timestamp: new Date(),
  }
  ;(chained as QiError & { cause: QiError }).cause = error
  return chained
}

/**
 * Format error chain for display
 */
export const formatErrorChain = (error: QiError, maxDepth = 10): string[] => {
  const chain: string[] = []
  let current: QiError | undefined = error
  let depth = 0

  while (current && depth < maxDepth) {
    chain.push(`${current.code}: ${current.message}`)
    current = current.cause
    depth++
  }

  if (current && depth >= maxDepth) {
    chain.push('... (chain truncated)')
  }

  return chain
}

/**
 * Get root cause of error chain
 */
export const getRootCause = (error: QiError): QiError => {
  let current = error
  while (current.cause) {
    current = current.cause
  }
  return current
}

/**
 * Serialize QiError to JSON-compatible format
 * Maintains cross-language compatibility
 */
export const serializeError = (error: QiError): Record<string, unknown> => ({
  code: error.code,
  message: error.message,
  category: error.category,
  context: error.context,
  cause: error.cause ? serializeError(error.cause) : undefined,
  timestamp: error.timestamp.getTime(), // Unix timestamp
  severity: error.severity,
})

/**
 * Deserialize QiError from JSON-compatible format
 */
export const deserializeError = (data: Record<string, unknown>): QiError => {
  if (
    typeof data.code !== 'string' ||
    typeof data.message !== 'string' ||
    !isErrorCategory(data.category as string)
  ) {
    throw new Error('Invalid error data format')
  }

  return {
    code: createErrorCode(data.code),
    message: data.message,
    category: data.category as ErrorCategory,
    context: (data.context as Record<string, unknown>) ?? {},
    cause: data.cause ? deserializeError(data.cause as Record<string, unknown>) : undefined,
    timestamp: new Date(data.timestamp as number),
    severity: data.severity as 'low' | 'medium' | 'high' | undefined,
  }
}

/**
 * Create aggregate error from multiple errors
 */
export const createAggregateError = (
  message: string,
  errors: QiError[],
  category: ErrorCategory = 'SYSTEM'
): QiError =>
  createError({
    code: 'AGGREGATE_ERROR',
    message,
    category,
    context: {
      errorCount: errors.length,
      errors: errors.map(serializeError),
    },
    severity: 'high',
  })

/**
 * Extract specific context value with type safety
 */
export const getContext = <T>(error: QiError, key: string): T | undefined =>
  error.context[key] as T | undefined

/**
 * Add context to existing error (creates new error)
 */
export const withContext = (error: QiError, context: Record<string, unknown>): QiError => ({
  ...error,
  context: { ...error.context, ...context },
  timestamp: new Date(),
})
