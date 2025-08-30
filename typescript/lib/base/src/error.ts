/**
 * QiCore Foundation Base - QiError Implementation
 *
 * Pure implementation following behavioral contracts exactly.
 * Provides structured error information with categorization.
 */

/**
 * Error categories for systematic error handling
 * Contract: complete enumeration for cross-language consistency
 */
export type ErrorCategory =
  | 'VALIDATION' // Input validation failures - never retry
  | 'NETWORK' // Network communication failures - exponential backoff
  | 'SYSTEM' // System resource problems - linear backoff
  | 'BUSINESS' // Business logic violations - never retry
  | 'AUTHENTICATION' // Authentication failures - never retry
  | 'AUTHORIZATION' // Permission failures - never retry
  | 'CONFIGURATION' // Configuration errors - never retry
  | 'TIMEOUT' // Timeout errors - exponential backoff
  | 'RESOURCE' // Resource exhaustion - linear backoff
  | 'CONCURRENCY' // Concurrency conflicts - linear backoff
  | 'LOGGER' // Logger-related errors - never retry

/**
 * Core QiError structure
 * Contract: Product type Code × Message × Category × Context?
 */
export interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Record<string, unknown>
}

/**
 * Retry strategy for each error category
 */
export interface RetryStrategy {
  readonly strategy: 'never' | 'exponential_backoff' | 'linear_backoff'
  readonly description: string
}

/**
 * Error category enumeration for validation
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

// ============================================================================
// Factory Operations (Contract Section 2.2)
// ============================================================================

/**
 * Create QiError with all required fields
 * Contract: all required fields populated
 * Contract: context defaults to empty map
 */
export const create = (
  code: string,
  message: string,
  category: ErrorCategory,
  context: Record<string, unknown> = {}
): QiError => ({
  code,
  message,
  category,
  context,
})

/**
 * Error options for createError factory
 */
export interface ErrorOptions {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
}

/**
 * Create QiError from options object
 * Contract: all required fields populated from options
 * Contract: context defaults to empty map if not provided
 */
export const createError = (options: ErrorOptions): QiError => ({
  code: options.code,
  message: options.message,
  category: options.category,
  context: options.context ?? {},
})

/**
 * Create QiError from exception
 * Contract: preserves exception message
 * Contract: category defaults to UNKNOWN (using SYSTEM as closest match)
 */
export const fromException = (exception: unknown, category: ErrorCategory = 'SYSTEM'): QiError => {
  if (exception instanceof Error) {
    return create('EXCEPTION', exception.message, category, {
      name: exception.name,
      stack: exception.stack,
    })
  }

  return create('UNKNOWN_EXCEPTION', String(exception), category, { originalValue: exception })
}

/**
 * Create QiError from string message
 * Contract: creates QiError from simple message
 * Contract: category defaults to UNKNOWN (using SYSTEM as closest match)
 */
export const fromString = (message: string, category: ErrorCategory = 'SYSTEM'): QiError =>
  create('ERROR', message, category)

// ============================================================================
// Query Operations (Contract Section 2.3)
// ============================================================================

/**
 * Convert error to string
 * Contract: includes code and message
 * Contract: human readable format
 * Contract: consistent format across implementations
 */
export const errorToString = (error: QiError): string => `[${error.code}] ${error.message}`

/**
 * Get error category
 * Contract: getCategory(error) == error.category
 */
export const getCategory = (error: QiError): ErrorCategory => error.category

/**
 * Convert to serializable structure
 * Contract: result is JSON serializable
 * Contract: preserves all error information
 */
export const toStructuredData = (error: QiError): Record<string, unknown> => ({
  code: error.code,
  message: error.message,
  category: error.category,
  context: error.context,
})

// ============================================================================
// Transformation Operations (Contract Section 2.4)
// ============================================================================

/**
 * Add or merge context to error
 * Contract: preserves all fields except context
 * Contract: merges new context with existing
 * Contract: immutable: original error unchanged
 */
export const withContext = (context: Record<string, unknown>, error: QiError): QiError => ({
  ...error,
  context: { ...error.context, ...context },
})

// ============================================================================
// Error Category Contract Implementation (Contract Section 3)
// ============================================================================

/**
 * Get retry strategy for error category
 * Contract: category determines retry behavior
 */
export const getRetryStrategy = (category: ErrorCategory): RetryStrategy => {
  const strategies: Record<ErrorCategory, RetryStrategy> = {
    VALIDATION: {
      strategy: 'never',
      description: 'Input validation and constraint violations',
    },
    NETWORK: {
      strategy: 'exponential_backoff',
      description: 'Network communication failures',
    },
    SYSTEM: {
      strategy: 'linear_backoff',
      description: 'System resource and infrastructure problems',
    },
    BUSINESS: {
      strategy: 'never',
      description: 'Business logic and domain rule violations',
    },
    AUTHENTICATION: {
      strategy: 'never',
      description: 'Authentication failures',
    },
    AUTHORIZATION: {
      strategy: 'never',
      description: 'Authorization/permission failures',
    },
    CONFIGURATION: {
      strategy: 'never',
      description: 'Configuration/setup errors',
    },
    TIMEOUT: {
      strategy: 'exponential_backoff',
      description: 'Timeout errors',
    },
    RESOURCE: {
      strategy: 'linear_backoff',
      description: 'Resource exhaustion/unavailable',
    },
    CONCURRENCY: {
      strategy: 'linear_backoff',
      description: 'Concurrency conflicts',
    },
    LOGGER: {
      strategy: 'never',
      description: 'Logger-related errors',
    },
  }

  return strategies[category]
}

// ============================================================================
// Convenience Factory Functions (Common Patterns)
// ============================================================================

/**
 * Create validation error
 */
export const validationError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('VALIDATION_ERROR', message, 'VALIDATION', context)

/**
 * Create network error
 */
export const networkError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('NETWORK_ERROR', message, 'NETWORK', context)

/**
 * Create system error
 */
export const systemError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('SYSTEM_ERROR', message, 'SYSTEM', context)

/**
 * Create business error
 */
export const businessError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('BUSINESS_ERROR', message, 'BUSINESS', context)

/**
 * Create authentication error
 */
export const authenticationError = (
  message: string,
  context: Record<string, unknown> = {}
): QiError => create('AUTH_ERROR', message, 'AUTHENTICATION', context)

/**
 * Create authorization error
 */
export const authorizationError = (
  message: string,
  context: Record<string, unknown> = {}
): QiError => create('AUTHZ_ERROR', message, 'AUTHORIZATION', context)

/**
 * Create configuration error
 */
export const configurationError = (
  message: string,
  context: Record<string, unknown> = {}
): QiError => create('CONFIG_ERROR', message, 'CONFIGURATION', context)

/**
 * Create timeout error
 */
export const timeoutError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('TIMEOUT_ERROR', message, 'TIMEOUT', context)

/**
 * Create resource error
 */
export const resourceError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('RESOURCE_ERROR', message, 'RESOURCE', context)

/**
 * Create concurrency error
 */
export const concurrencyError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('CONCURRENCY_ERROR', message, 'CONCURRENCY', context)

/**
 * Create logger error
 * Contract: code automatically set to LOGGER_ERROR
 * Contract: context defaults to empty map
 */
export const loggerError = (message: string, context: Record<string, unknown> = {}): QiError =>
  create('LOGGER_ERROR', message, 'LOGGER', context)
