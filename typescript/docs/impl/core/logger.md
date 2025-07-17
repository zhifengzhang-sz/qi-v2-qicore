Based on the implementation guide and our earlier work on qi/base, here's the logger.ts implementation:

```typescript
/**
 * QiCore Foundation Core - Logger Implementation
 *
 * Simple, pure functional logger with level filtering and structured output.
 * Uses console backend for zero dependencies.
 */

import { 
  type Result, 
  type QiError,
  success, 
  failure, 
  create as createError 
} from '@qi/base'

// ============================================================================
// Types
// ============================================================================

/**
 * Log levels in severity order
 */
export type LogLevel = 'DEBUG' | 'INFO' | 'WARN' | 'ERROR' | 'FATAL'

/**
 * Logger configuration
 */
export interface LoggerConfig {
  readonly level: LogLevel
}

/**
 * Immutable logger instance
 */
export interface Logger {
  readonly level: LogLevel
  readonly context: Record<string, unknown>
}

/**
 * Log entry structure for output
 */
interface LogEntry {
  readonly timestamp: string
  readonly level: LogLevel
  readonly message: string
  readonly context?: Record<string, unknown>
  readonly error?: unknown
}

// ============================================================================
// Constants
// ============================================================================

/**
 * Log level numeric values for comparison
 */
const LOG_LEVEL_VALUES: Record<LogLevel, number> = {
  DEBUG: 10,
  INFO: 20,
  WARN: 30,
  ERROR: 40,
  FATAL: 50
} as const

/**
 * Valid log levels for validation
 */
const VALID_LOG_LEVELS: ReadonlyArray<LogLevel> = [
  'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'
] as const

// ============================================================================
// Factory Operations
// ============================================================================

/**
 * Create logger instance with configuration
 * Contract: validates configuration before creation
 * Contract: returns VALIDATION error for invalid config
 */
export const create = (config: LoggerConfig): Result<Logger> => {
  if (!isValidLogLevel(config.level)) {
    return failure(
      createError(
        'INVALID_LOG_LEVEL',
        `Invalid log level: ${config.level}`,
        'CONFIGURATION',
        { level: config.level }
      )
    )
  }

  return success({
    level: config.level,
    context: {}
  })
}

// ============================================================================
// Logging Operations (Effects)
// ============================================================================

/**
 * Log debug-level message
 * Contract: only logs if logger level <= DEBUG
 */
export const debug = (
  message: string,
  context: Record<string, unknown> | undefined,
  logger: Logger
): void => {
  log('DEBUG', message, context, undefined, logger)
}

/**
 * Log info-level message
 * Contract: only logs if logger level <= INFO
 */
export const info = (
  message: string,
  context: Record<string, unknown> | undefined,
  logger: Logger
): void => {
  log('INFO', message, context, undefined, logger)
}

/**
 * Log warning-level message
 * Contract: only logs if logger level <= WARN
 */
export const warn = (
  message: string,
  context: Record<string, unknown> | undefined,
  logger: Logger
): void => {
  log('WARN', message, context, undefined, logger)
}

/**
 * Log error-level message
 * Contract: only logs if logger level <= ERROR
 * Contract: includes error details if provided
 */
export const error = (
  message: string,
  errorOrContext: unknown | Record<string, unknown> | undefined,
  context: Record<string, unknown> | undefined,
  logger: Logger
): void => {
  // Handle overloaded parameters
  let actualError: unknown | undefined
  let actualContext: Record<string, unknown> | undefined

  if (isError(errorOrContext)) {
    actualError = errorOrContext
    actualContext = context
  } else if (errorOrContext && typeof errorOrContext === 'object') {
    actualContext = errorOrContext as Record<string, unknown>
  }

  log('ERROR', message, actualContext, actualError, logger)
}

/**
 * Log fatal-level message
 * Contract: only logs if logger level <= FATAL
 * Contract: indicates critical system failures
 */
export const fatal = (
  message: string,
  errorOrContext: unknown | Record<string, unknown> | undefined,
  context: Record<string, unknown> | undefined,
  logger: Logger
): void => {
  // Handle overloaded parameters (same as error)
  let actualError: unknown | undefined
  let actualContext: Record<string, unknown> | undefined

  if (isError(errorOrContext)) {
    actualError = errorOrContext
    actualContext = context
  } else if (errorOrContext && typeof errorOrContext === 'object') {
    actualContext = errorOrContext as Record<string, unknown>
  }

  log('FATAL', message, actualContext, actualError, logger)
}

// ============================================================================
// Utility Operations
// ============================================================================

/**
 * Check if level would be logged (performance optimization)
 * Contract: returns true if level >= logger.level
 * Contract: constant time operation
 */
export const isLevelEnabled = (level: LogLevel, logger: Logger): boolean => {
  return LOG_LEVEL_VALUES[level] >= LOG_LEVEL_VALUES[logger.level]
}

/**
 * Create logger with additional context
 * Contract: all subsequent log calls include context
 * Contract: context merged with per-call context
 * Contract: original logger unchanged (immutable)
 */
export const withContext = (
  context: Record<string, unknown>,
  logger: Logger
): Logger => ({
  ...logger,
  context: { ...logger.context, ...context }
})

// ============================================================================
// Internal Functions
// ============================================================================

/**
 * Core logging function (internal)
 */
const log = (
  level: LogLevel,
  message: string,
  context: Record<string, unknown> | undefined,
  error: unknown | undefined,
  logger: Logger
): void => {
  // Performance: check level first
  if (!isLevelEnabled(level, logger)) {
    return
  }

  const entry: LogEntry = {
    timestamp: new Date().toISOString(),
    level,
    message,
    ...(context || Object.keys(logger.context).length > 0 
      ? { context: { ...logger.context, ...context } }
      : {}),
    ...(error ? { error: formatError(error) } : {})
  }

  // Output to console (using appropriate console method)
  const output = formatLogEntry(entry)
  
  switch (level) {
    case 'DEBUG':
      console.debug(output)
      break
    case 'INFO':
      console.info(output)
      break
    case 'WARN':
      console.warn(output)
      break
    case 'ERROR':
      console.error(output)
      break
    case 'FATAL':
      console.error(output) // No console.fatal
      break
  }
}

/**
 * Format log entry for output
 */
const formatLogEntry = (entry: LogEntry): string => {
  // Simple JSON output for structured logging
  return JSON.stringify(entry)
}

/**
 * Format error for logging
 */
const formatError = (error: unknown): unknown => {
  if (isError(error)) {
    return {
      message: error.message,
      stack: error.stack,
      ...(error.name ? { name: error.name } : {})
    }
  }
  
  if (isQiError(error)) {
    return {
      code: error.code,
      message: error.message,
      category: error.category,
      context: error.context
    }
  }
  
  return error
}

/**
 * Type guard for Error
 */
const isError = (value: unknown): value is Error => {
  return value instanceof Error
}

/**
 * Type guard for QiError
 */
const isQiError = (value: unknown): value is QiError => {
  return (
    value !== null &&
    typeof value === 'object' &&
    'code' in value &&
    'message' in value &&
    'category' in value
  )
}

/**
 * Validate log level
 */
const isValidLogLevel = (level: string): level is LogLevel => {
  return VALID_LOG_LEVELS.includes(level as LogLevel)
}

// ============================================================================
// Re-exports for convenience
// ============================================================================

export type { LoggerConfig, Logger }
```

This implementation:

1. **Follows the contract specifications** - simple logger with level filtering
2. **Uses pure functions** - logger is just immutable data
3. **Integrates with Result<T>** - factory returns Result for error handling
4. **Zero dependencies** - uses console backend
5. **Performance conscious** - level check before formatting
6. **Immutable** - withContext returns new logger
7. **Structured output** - JSON format for easy parsing
8. **Type safe** - proper TypeScript types throughout

Key design decisions:
- Logger is just data (level + context)
- Logging functions are effects (return void)
- Simple console backend for zero dependencies
- JSON output for structured logging
- Overloaded error/fatal for flexibility
- Context accumulation through immutability

Usage example:
```typescript
const loggerResult = create({ level: 'INFO' })
if (isSuccess(loggerResult)) {
  const logger = loggerResult.value
  const appLogger = withContext({ app: 'myapp', version: '1.0' }, logger)
  
  info('Application started', undefined, appLogger)
  error('Failed to connect', new Error('Connection refused'), { retry: 3 }, appLogger)
}
```