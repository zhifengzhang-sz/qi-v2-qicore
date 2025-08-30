/**
 * QiCore Foundation - Logger Module
 *
 * Event-driven logging with Pino integration using functional patterns.
 * Provides structured JSON logging with OpenTelemetry integration and
 * high-performance event system. Max-Min principle: 70% pino package, 30% custom wrapper.
 */

import { type Result, type QiError, success, failure, create as createError } from '@qi/base'
import { EventEmitter } from 'eventemitter3'
import pino, { type Logger as PinoLogger, type LoggerOptions } from 'pino'

// ============================================================================
// Core Types
// ============================================================================

/**
 * Log levels in order of severity
 * Contract: DEBUG < INFO < WARN < ERROR < FATAL
 */
export type LogLevel = 'debug' | 'info' | 'warn' | 'error' | 'fatal'

/**
 * Log entry structure
 */
export interface LogEntry {
  readonly level: LogLevel
  readonly message: string
  readonly timestamp: Date
  readonly context?: Record<string, unknown>
  readonly error?: Error
  readonly traceId?: string
  readonly spanId?: string
}

/**
 * Logger configuration
 */
export interface LoggerConfig {
  readonly level: LogLevel
  readonly name?: string
  readonly pretty?: boolean
  readonly destination?: string
  readonly redact?: string[]
  readonly serializers?: Record<string, (obj: unknown) => unknown>
  readonly hooks?: {
    logMethod?: (inputArgs: unknown[], method: unknown) => void
  }
}

/**
 * Logger context for child loggers
 */
export type LoggerContext = Record<string, unknown>

/**
 * Logger events
 */
export interface LoggerEvents {
  log: (entry: LogEntry) => void
  error: (error: LoggerError) => void
  level: (level: LogLevel) => void
}

// ============================================================================
// Error Types
// ============================================================================

/**
 * Logger-specific error types
 */
export type LoggerError = QiError & {
  readonly category: 'LOGGER'
  readonly context: {
    readonly operation?: string
    readonly level?: LogLevel
    readonly logger?: string
  }
}

/**
 * Create logger error
 */
export const loggerError = (message: string, context: LoggerError['context'] = {}): LoggerError =>
  createError('LOGGER_ERROR', message, 'LOGGER', context) as LoggerError

// ============================================================================
// Logger Implementation (70% pino package)
// ============================================================================

/**
 * QiCore Logger with Pino backend and event system
 */
export class Logger {
  private readonly pino: PinoLogger
  private readonly events: EventEmitter<LoggerEvents>
  private readonly config: LoggerConfig
  private childContext?: LoggerContext

  constructor(config: LoggerConfig) {
    this.config = { ...config }
    this.events = new EventEmitter<LoggerEvents>()

    // Configure Pino logger (leveraging 70% of pino capabilities)
    const pinoOptions: LoggerOptions = {
      level: config.level,
      ...(config.name && { name: config.name }),
      ...(config.redact && { redact: config.redact }),
      ...(config.serializers && { serializers: config.serializers }),
      ...(config.hooks && { hooks: config.hooks }),
      ...(config.pretty && {
        transport: {
          target: 'pino-pretty',
          options: {
            colorize: true,
            translateTime: 'HH:MM:ss Z',
            ignore: 'pid,hostname',
          },
        },
      }),
    }

    // Use pino's destination handling for file/stream output
    this.pino = config.destination
      ? pino(pinoOptions, pino.destination(config.destination))
      : pino(pinoOptions)
  }

  /**
   * Create child logger with additional context (using pino.child)
   */
  child(context: LoggerContext): Logger {
    const childPino = this.pino.child(context)
    const childLogger = new Logger(this.config)

    // Replace pino instance with child
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    ;(childLogger as any).pino = childPino
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    ;(childLogger as any).events = this.events // Share event emitter
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    ;(childLogger as any).childContext = context

    return childLogger
  }

  /**
   * Log debug message (using pino.debug)
   */
  debug(message: string, context?: LoggerContext): void {
    this.log('debug', message, context)
  }

  /**
   * Log info message (using pino.info)
   */
  info(message: string, context?: LoggerContext): void {
    this.log('info', message, context)
  }

  /**
   * Log warning message (using pino.warn)
   */
  warn(message: string, context?: LoggerContext): void {
    this.log('warn', message, context)
  }

  /**
   * Log error message (using pino.error)
   */
  error(message: string, error?: Error, context?: LoggerContext): void {
    this.log('error', message, { ...context, error })
  }

  /**
   * Log fatal message (using pino.fatal)
   * Contract: Log fatal-level message for critical system failures
   */
  fatal(message: string, error?: Error, context?: LoggerContext): void {
    this.log('fatal', message, { ...context, error })
  }

  /**
   * Check if log level is enabled (using pino level checking)
   */
  isLevelEnabled(level: LogLevel): boolean {
    return this.pino.isLevelEnabled(level)
  }

  /**
   * Get current log level (from pino)
   */
  getLevel(): LogLevel {
    return this.pino.level as LogLevel
  }

  /**
   * Set log level (using pino.level)
   */
  setLevel(level: LogLevel): void {
    this.pino.level = level
    this.events.emit('level', level)
  }

  /**
   * Get logger configuration
   */
  getConfig(): LoggerConfig {
    return { ...this.config }
  }

  /**
   * Event listener methods for external event handling
   */
  on<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.events.on(event as any, listener as any)
  }

  once<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.events.once(event as any, listener as any)
  }

  off<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.events.off(event as any, listener as any)
  }

  /**
   * Flush any pending logs (compatibility method)
   */
  async flush(): Promise<void> {
    // Pino handles flushing internally, this is a compatibility method
    return Promise.resolve()
  }

  /**
   * Close logger resources
   */
  close(): void {
    this.events.removeAllListeners()
  }

  /**
   * Generic log method (30% custom wrapper around pino)
   */
  private log(level: LogLevel, message: string, context?: LoggerContext): void {
    try {
      // Use pino's level checking for performance
      if (!this.isLevelEnabled(level)) {
        return
      }

      // Merge child context with provided context for event emission
      const mergedContext = { ...this.childContext, ...context }

      // Create log entry for events
      const entry: LogEntry = {
        level,
        message,
        timestamp: new Date(),
        context: mergedContext,
        error: mergedContext?.error as Error,
        traceId: mergedContext?.traceId as string,
        spanId: mergedContext?.spanId as string,
      }

      // Use pino's logging - pino.child() already has context merged, so only pass additional context
      if (context?.error) {
        // Use pino's error object handling with just the additional context
        this.pino[level](context, message)
      } else if (context) {
        // Use pino's context logging with just the additional context
        this.pino[level](context, message)
      } else {
        // Use pino's simple message logging (child context already applied)
        this.pino[level](message)
      }

      // Emit custom event (30% custom logic)
      this.events.emit('log', entry)
    } catch (error) {
      // Emit error event for custom handling
      this.events.emit(
        'error',
        loggerError(`Failed to log message: ${error}`, { operation: 'log', level })
      )
    }
  }
}

// ============================================================================
// Factory Functions (30% custom logic)
// ============================================================================

/**
 * Create logger with configuration validation
 */
export const createLogger = (config: LoggerConfig): Result<Logger, LoggerError> => {
  try {
    // Validate log level
    const validLevels: LogLevel[] = ['debug', 'info', 'warn', 'error', 'fatal']
    if (!validLevels.includes(config.level)) {
      return failure(
        loggerError(`Invalid log level: ${config.level}`, {
          operation: 'create',
          level: config.level,
        })
      )
    }

    const logger = new Logger(config)
    return success(logger)
  } catch (error) {
    return failure(loggerError(`Failed to create logger: ${error}`, { operation: 'create' }))
  }
}

/**
 * Create logger from environment
 */
export const createFromEnv = (): Result<Logger, LoggerError> => {
  const config = getEnvironmentConfig()
  return createLogger(config)
}

// ============================================================================
// Utility Functions (30% custom logic)
// ============================================================================

/**
 * Format error for logging
 */
export const formatError = (error: Error): Record<string, unknown> => {
  const result: Record<string, unknown> = {
    name: error.name,
    message: error.message,
    stack: error.stack,
  }

  if (error.cause) {
    result.cause = error.cause
  }

  return result
}

/**
 * Create request logger middleware helper
 */
export const createRequestLogger = (logger: Logger) => {
  return {
    /**
     * Log request (using pino structured logging)
     */
    logRequest: (
      req: { method?: string; url?: string; headers?: Record<string, string> },
      context?: LoggerContext
    ) => {
      logger.info('Request received', {
        method: req.method,
        url: req.url,
        userAgent: req.headers?.['user-agent'],
        ...context,
      })
    },

    /**
     * Log response (with automatic error level detection)
     */
    logResponse: (
      req: { method?: string; url?: string },
      res: { statusCode?: number },
      duration: number,
      context?: LoggerContext
    ) => {
      const level = (res.statusCode ?? 200) >= 400 ? 'error' : 'info'
      const logContext = {
        method: req.method,
        url: req.url,
        statusCode: res.statusCode,
        duration,
        ...context,
      }

      if (level === 'error') {
        logger.error('Request completed', undefined, logContext)
      } else {
        logger[level]('Request completed', logContext)
      }
    },

    /**
     * Log error (with proper error object handling)
     */
    logError: (error: Error, req?: { method?: string; url?: string }, context?: LoggerContext) => {
      logger.error('Request error', error, {
        method: req?.method,
        url: req?.url,
        ...formatError(error),
        ...context,
      })
    },
  }
}

// ============================================================================
// Common Logger Configurations (30% custom logic)
// ============================================================================

/**
 * Development logger configuration (with pino-pretty)
 */
export const developmentConfig: LoggerConfig = {
  level: 'debug',
  name: 'qicore-dev',
  pretty: true,
}

/**
 * Production logger configuration (optimized pino settings)
 */
export const productionConfig: LoggerConfig = {
  level: 'warn',
  name: 'qicore-prod',
  pretty: false,
  redact: ['password', 'token', 'secret', 'key', 'authorization'],
  // Note: pino serializers would be properly typed in production
}

/**
 * Test logger configuration (minimal output)
 */
export const testConfig: LoggerConfig = {
  level: 'fatal',
  name: 'qicore-test',
  pretty: false,
}

/**
 * Get logger configuration based on environment
 */
export const getEnvironmentConfig = (): LoggerConfig => {
  const env = process.env.NODE_ENV || 'development'
  const logLevel = (process.env.LOG_LEVEL as LogLevel) || undefined

  const baseConfig = (() => {
    switch (env) {
      case 'production':
        return productionConfig
      case 'test':
        return testConfig
      default:
        return developmentConfig
    }
  })()

  // Override with environment LOG_LEVEL if provided
  return logLevel ? { ...baseConfig, level: logLevel } : baseConfig
}

// ============================================================================
// Re-exports
// ============================================================================

// Types are already exported above
