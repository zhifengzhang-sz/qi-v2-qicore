/**
 * QiCore Foundation - Logger Module
 *
 * Event-driven logging with Pino integration using functional patterns.
 * Provides structured JSON logging with OpenTelemetry integration and
 * high-performance event system.
 */

import { Err, Ok, type QiError, type Result, createError } from '@qi/base'
import { EventEmitter } from 'eventemitter3'
import pino, { type Logger as PinoLogger, type LoggerOptions } from 'pino'

// ============================================================================
// Core Types
// ============================================================================

/**
 * Log levels in order of severity
 */
export type LogLevel = 'debug' | 'info' | 'warn' | 'error'

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
  createError({
    code: 'LOGGER_ERROR',
    message,
    category: 'LOGGER',
    context,
  }) as LoggerError

// ============================================================================
// Logger Implementation
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

    // Configure Pino logger
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

    this.pino = config.destination
      ? pino(pinoOptions, pino.destination(config.destination))
      : pino(pinoOptions)
  }

  /**
   * Create child logger with additional context
   */
  child(context: LoggerContext): Logger {
    const childPino = this.pino.child(context)
    const childLogger = new Logger(this.config)
    // Use private property access to set the child context
    ;(childLogger as any).pino = childPino
    ;(childLogger as any).events = this.events // Share event emitter
    ;(childLogger as any).childContext = context
    return childLogger
  }

  /**
   * Log debug message
   */
  debug(message: string, context?: LoggerContext): void {
    this.log('debug', message, context)
  }

  /**
   * Log info message
   */
  info(message: string, context?: LoggerContext): void {
    this.log('info', message, context)
  }

  /**
   * Log warning message
   */
  warn(message: string, context?: LoggerContext): void {
    this.log('warn', message, context)
  }

  /**
   * Log error message
   */
  error(message: string, error?: Error, context?: LoggerContext): void {
    this.log('error', message, { ...context, error })
  }

  /**
   * Generic log method
   */
  private log(level: LogLevel, message: string, context?: LoggerContext): void {
    try {
      // Check if level is enabled before processing
      if (!this.isLevelEnabled(level)) {
        return
      }

      // Merge child context with provided context
      const mergedContext = { ...this.childContext, ...context }

      // Create log entry
      const entry: LogEntry = {
        level,
        message,
        timestamp: new Date(),
        context: mergedContext,
        error: mergedContext?.error as Error,
        traceId: mergedContext?.traceId as string,
        spanId: mergedContext?.spanId as string,
      }

      // Log with Pino
      if (context?.error) {
        this.pino[level](context, message)
      } else {
        this.pino[level](context || {}, message)
      }

      // Emit event
      this.events.emit('log', entry)
    } catch (error) {
      // Emit error event for logging failures
      this.events.emit(
        'error',
        loggerError(`Failed to log message: ${error}`, {
          operation: 'log',
          level,
          logger: this.config.name,
        })
      )
    }
  }

  /**
   * Check if level is enabled
   */
  isLevelEnabled(level: LogLevel): boolean {
    return this.pino.isLevelEnabled(level)
  }

  /**
   * Set log level
   */
  setLevel(level: LogLevel): void {
    this.pino.level = level
    this.events.emit('level', level)
  }

  /**
   * Get current log level
   */
  getLevel(): LogLevel {
    return this.pino.level as LogLevel
  }

  /**
   * Get logger configuration
   */
  getConfig(): LoggerConfig {
    return { ...this.config }
  }

  /**
   * Add event listener
   */
  on<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): this {
    this.events.on(event, listener as any)
    return this
  }

  /**
   * Remove event listener
   */
  off<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): this {
    this.events.off(event, listener as any)
    return this
  }

  /**
   * Add one-time event listener
   */
  once<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): this {
    this.events.once(event, listener as any)
    return this
  }

  /**
   * Flush pending logs
   */
  async flush(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.pino.flush(() => resolve())
    })
  }

  /**
   * Close logger and cleanup resources
   */
  async close(): Promise<void> {
    await this.flush()
    this.events.removeAllListeners()
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create logger with configuration
 */
export const createLogger = (config: LoggerConfig): Result<Logger, LoggerError> => {
  try {
    const logger = new Logger(config)
    return Ok(logger)
  } catch (error) {
    return Err(
      loggerError(`Failed to create logger: ${error}`, { operation: 'create', logger: config.name })
    )
  }
}

/**
 * Create logger with default configuration
 */
export const defaultLogger = (): Logger => {
  return new Logger({
    level: 'info',
    name: 'qicore',
    pretty: process.env.NODE_ENV === 'development',
  })
}

/**
 * Create logger with pretty formatting
 */
export const prettyLogger = (level: LogLevel = 'info'): Logger => {
  return new Logger({
    level,
    pretty: true,
  })
}

/**
 * Create logger with file destination
 */
export const fileLogger = (
  level: LogLevel,
  destination: string,
  name?: string
): Result<Logger, LoggerError> => {
  return createLogger({
    level,
    destination,
    name,
    pretty: false,
  })
}

/**
 * Create logger with OpenTelemetry integration
 */
export const telemetryLogger = (
  config: LoggerConfig,
  traceId?: string,
  spanId?: string
): Result<Logger, LoggerError> => {
  const result = createLogger(config)

  if (result.tag === 'success' && (traceId || spanId)) {
    const contextLogger = result.value.child({
      traceId,
      spanId,
    })
    return Ok(contextLogger)
  }

  return result
}

// ============================================================================
// Logger Utilities
// ============================================================================

/**
 * Log level comparison
 */
export const levelValue = (level: LogLevel): number => {
  const levels = { debug: 0, info: 1, warn: 2, error: 3 }
  return levels[level]
}

/**
 * Check if level is enabled for given minimum level
 */
export const isLevelEnabled = (level: LogLevel, minLevel: LogLevel): boolean => {
  return levelValue(level) >= levelValue(minLevel)
}

/**
 * Format error for logging
 */
export const formatError = (error: Error): LoggerContext => {
  return {
    error: {
      name: error.name,
      message: error.message,
      stack: error.stack,
      cause: error.cause,
    },
  }
}

/**
 * Format QiError for logging
 */
export const formatQiError = (error: QiError): LoggerContext => {
  return {
    error: {
      code: error.code,
      message: error.message,
      category: error.category,
      context: error.context,
      cause: error.cause,
      stack: (error as any).stack,
    },
  }
}

/**
 * Create request context for logging
 */
export const requestContext = (
  requestId: string,
  userId?: string,
  sessionId?: string
): LoggerContext => {
  return {
    requestId,
    userId,
    sessionId,
    timestamp: new Date().toISOString(),
  }
}

/**
 * Create performance context for logging
 */
export const performanceContext = (
  operation: string,
  duration: number,
  metadata?: Record<string, unknown>
): LoggerContext => {
  return {
    operation,
    duration,
    performance: true,
    ...metadata,
  }
}

// ============================================================================
// Logger Middleware
// ============================================================================

/**
 * Logger middleware for request/response logging
 */
export const loggerMiddleware = (logger: Logger) => {
  return {
    /**
     * Log request
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
     * Log response
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
     * Log error
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
// Common Logger Configurations
// ============================================================================

/**
 * Development logger configuration
 */
export const developmentConfig: LoggerConfig = {
  level: 'debug',
  name: 'qicore-dev',
  pretty: true,
}

/**
 * Production logger configuration
 */
export const productionConfig: LoggerConfig = {
  level: 'info',
  name: 'qicore-prod',
  pretty: false,
  redact: ['password', 'token', 'secret', 'key'],
}

/**
 * Test logger configuration
 */
export const testConfig: LoggerConfig = {
  level: 'error',
  name: 'qicore-test',
  pretty: false,
}

/**
 * Get logger configuration based on environment
 */
export const getEnvironmentConfig = (): LoggerConfig => {
  const env = process.env.NODE_ENV || 'development'

  switch (env) {
    case 'production':
      return productionConfig
    case 'test':
      return testConfig
    default:
      return developmentConfig
  }
}
