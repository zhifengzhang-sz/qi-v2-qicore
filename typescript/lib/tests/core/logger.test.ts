/**
 * Unit tests for Logger module
 */

import { unlink } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { createError } from '@qi/base'
import {
  type LogEntry,
  type LogLevel,
  Logger,
  type LoggerConfig,
  createLogger,
  defaultLogger,
  fileLogger,
  formatError,
  formatQiError,
  getLoggerEnvironmentConfig,
  isLevelEnabled,
  levelValue,
  loggerDevelopmentConfig,
  loggerError,
  loggerMiddleware,
  loggerProductionConfig,
  loggerTestConfig,
  performanceContext,
  prettyLogger,
  requestContext,
  telemetryLogger,
} from '@qi/core'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

describe('Logger Module', () => {
  let testLogFile: string | undefined

  beforeEach(() => {
    // Reset environment
    process.env.NODE_ENV = undefined
  })

  afterEach(async () => {
    // Clean up test log file
    if (testLogFile) {
      try {
        await unlink(testLogFile)
      } catch {}
      testLogFile = undefined
    }
  })

  describe('Logger Class', () => {
    test('creates logger with basic configuration', () => {
      const config: LoggerConfig = {
        level: 'info',
        name: 'test-logger',
      }

      const logger = new Logger(config)

      expect(logger.getLevel()).toBe('info')
      expect(logger.getConfig().name).toBe('test-logger')
    })

    test('logs at different levels', () => {
      const config: LoggerConfig = { level: 'debug' }
      const logger = new Logger(config)

      const logSpy = vi.fn()
      logger.on('log', logSpy)

      logger.debug('Debug message')
      logger.info('Info message')
      logger.warn('Warning message')
      logger.error('Error message')

      expect(logSpy).toHaveBeenCalledTimes(4)

      const calls = logSpy.mock.calls
      expect(calls[0][0].level).toBe('debug')
      expect(calls[1][0].level).toBe('info')
      expect(calls[2][0].level).toBe('warn')
      expect(calls[3][0].level).toBe('error')
    })

    test('logs with context', () => {
      const logger = new Logger({ level: 'info' })
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const context = { userId: '123', action: 'login' }
      logger.info('User action', context)

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.context).toEqual(context)
    })

    test('logs errors with error objects', () => {
      const logger = new Logger({ level: 'error' })
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const error = new Error('Test error')
      logger.error('Something went wrong', error)

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.error).toBe(error)
    })

    test('creates child logger with context', () => {
      const logger = new Logger({ level: 'info' })
      const childLogger = logger.child({ service: 'auth' })

      const logSpy = vi.fn()
      childLogger.on('log', logSpy)

      childLogger.info('Auth event')

      expect(logSpy).toHaveBeenCalledOnce()
    })

    test('checks if level is enabled', () => {
      const logger = new Logger({ level: 'warn' })

      expect(logger.isLevelEnabled('debug')).toBe(false)
      expect(logger.isLevelEnabled('info')).toBe(false)
      expect(logger.isLevelEnabled('warn')).toBe(true)
      expect(logger.isLevelEnabled('error')).toBe(true)
    })

    test('sets log level dynamically', () => {
      const logger = new Logger({ level: 'info' })
      const levelSpy = vi.fn()
      logger.on('level', levelSpy)

      logger.setLevel('debug')

      expect(logger.getLevel()).toBe('debug')
      expect(levelSpy).toHaveBeenCalledWith('debug')
    })

    test('handles logging errors gracefully', () => {
      const logger = new Logger({ level: 'info' })
      const errorSpy = vi.fn()
      logger.on('error', errorSpy)

      // Force an error by corrupting internal state
      ;(logger as unknown as { pino: null }).pino = null

      logger.info('This should fail')

      expect(errorSpy).toHaveBeenCalled()
      const error = errorSpy.mock.calls[0][0]
      expect(error.category).toBe('LOGGER')
    })

    test('flushes pending logs', async () => {
      const logger = new Logger({ level: 'info' })

      logger.info('Test message')

      // Should not throw
      await logger.flush()
    })

    test('closes logger and cleans up', async () => {
      const logger = new Logger({ level: 'info' })
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      await logger.close()

      // Events should be removed
      logger.info('This should not emit events')
      expect(logSpy).not.toHaveBeenCalled()
    })
  })

  describe('Factory Functions', () => {
    test('createLogger creates logger successfully', () => {
      const config: LoggerConfig = { level: 'info', name: 'test' }
      const result = createLogger(config)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value).toBeInstanceOf(Logger)
        expect(result.value.getConfig().name).toBe('test')
      }
    })

    test('defaultLogger creates logger with defaults', () => {
      const logger = defaultLogger()

      expect(logger.getLevel()).toBe('info')
      expect(logger.getConfig().name).toBe('qicore')
    })

    test('prettyLogger creates logger with pretty formatting', () => {
      const logger = prettyLogger('debug')

      expect(logger.getLevel()).toBe('debug')
      expect(logger.getConfig().pretty).toBe(true)
    })

    test('fileLogger creates logger with file destination', () => {
      testLogFile = join(tmpdir(), 'test.log')
      const result = fileLogger('info', testLogFile, 'file-logger')

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value.getConfig().destination).toBe(testLogFile)
        expect(result.value.getConfig().name).toBe('file-logger')
      }
    })

    test('telemetryLogger creates logger with trace context', () => {
      const config: LoggerConfig = { level: 'info' }
      const result = telemetryLogger(config, 'trace-123', 'span-456')

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        const logSpy = vi.fn()
        result.value.on('log', logSpy)

        result.value.info('Test message')

        expect(logSpy).toHaveBeenCalledOnce()
      }
    })
  })

  describe('Utility Functions', () => {
    test('levelValue returns correct numeric values', () => {
      expect(levelValue('debug')).toBe(0)
      expect(levelValue('info')).toBe(1)
      expect(levelValue('warn')).toBe(2)
      expect(levelValue('error')).toBe(3)
    })

    test('isLevelEnabled compares levels correctly', () => {
      expect(isLevelEnabled('debug', 'info')).toBe(false)
      expect(isLevelEnabled('info', 'info')).toBe(true)
      expect(isLevelEnabled('warn', 'info')).toBe(true)
      expect(isLevelEnabled('error', 'info')).toBe(true)
    })

    test('formatError formats Error objects', () => {
      const error = new Error('Test error')
      error.cause = 'Original cause'

      const formatted = formatError(error)

      expect(formatted.error).toMatchObject({
        name: 'Error',
        message: 'Test error',
        cause: 'Original cause',
      })
      expect(formatted.error).toHaveProperty('stack')
    })

    test('formatQiError formats QiError objects', () => {
      const qiError = createError({
        code: 'TEST_ERROR',
        message: 'Test error',
        category: 'VALIDATION',
        context: { field: 'email' },
      })

      const formatted = formatQiError(qiError)

      expect(formatted.error).toMatchObject({
        code: 'TEST_ERROR',
        message: 'Test error',
        category: 'VALIDATION',
        context: { field: 'email' },
      })
    })

    test('requestContext creates request logging context', () => {
      const context = requestContext('req-123', 'user-456', 'session-789')

      expect(context.requestId).toBe('req-123')
      expect(context.userId).toBe('user-456')
      expect(context.sessionId).toBe('session-789')
      expect(context.timestamp).toBeDefined()
    })

    test('performanceContext creates performance logging context', () => {
      const context = performanceContext('database-query', 150, { query: 'SELECT * FROM users' })

      expect(context.operation).toBe('database-query')
      expect(context.duration).toBe(150)
      expect(context.performance).toBe(true)
      expect(context.query).toBe('SELECT * FROM users')
    })
  })

  describe('Logger Middleware', () => {
    test('creates middleware with request logging', () => {
      const logger = new Logger({ level: 'info' })
      const middleware = loggerMiddleware(logger)
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const mockReq = {
        method: 'GET',
        url: '/api/users',
        headers: { 'user-agent': 'test-agent' },
      }

      middleware.logRequest(mockReq, { requestId: 'req-123' })

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.message).toBe('Request received')
      expect(logEntry.context?.method).toBe('GET')
      expect(logEntry.context?.requestId).toBe('req-123')
    })

    test('creates middleware with response logging', () => {
      const logger = new Logger({ level: 'info' })
      const middleware = loggerMiddleware(logger)
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const mockReq = { method: 'GET', url: '/api/users' }
      const mockRes = { statusCode: 200 }

      middleware.logResponse(mockReq, mockRes, 150)

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.level).toBe('info')
      expect(logEntry.message).toBe('Request completed')
      expect(logEntry.context?.statusCode).toBe(200)
      expect(logEntry.context?.duration).toBe(150)
    })

    test('logs errors with error level for 4xx/5xx responses', () => {
      const logger = new Logger({ level: 'info' })
      const middleware = loggerMiddleware(logger)
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const mockReq = { method: 'POST', url: '/api/users' }
      const mockRes = { statusCode: 400 }

      middleware.logResponse(mockReq, mockRes, 50)

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.level).toBe('error')
    })

    test('creates middleware with error logging', () => {
      const logger = new Logger({ level: 'error' })
      const middleware = loggerMiddleware(logger)
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const error = new Error('Database connection failed')
      const mockReq = { method: 'POST', url: '/api/users' }

      middleware.logError(error, mockReq, { requestId: 'req-123' })

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.level).toBe('error')
      expect(logEntry.message).toBe('Request error')
      expect(logEntry.error).toBe(error)
      expect(logEntry.context?.requestId).toBe('req-123')
    })
  })

  describe('Error Handling', () => {
    test('loggerError creates proper error structure', () => {
      const error = loggerError('Test logger error', {
        operation: 'log',
        level: 'error',
        logger: 'test-logger',
      })

      expect(error.category).toBe('LOGGER')
      expect(error.message).toBe('Test logger error')
      expect(error.context.operation).toBe('log')
      expect(error.context.level).toBe('error')
      expect(error.context.logger).toBe('test-logger')
    })

    test('createLogger handles configuration errors', () => {
      // Test with invalid configuration that might cause Pino to throw
      const invalidConfig: LoggerConfig = {
        level: 'invalid' as LogLevel,
        destination: './invalid-test-file.log',
      }

      const result = createLogger(invalidConfig)

      // Should either succeed or return proper error
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('LOGGER')
      }
    })
  })

  describe('Environment Configurations', () => {
    test('loggerDevelopmentConfig has correct settings', () => {
      expect(loggerDevelopmentConfig.level).toBe('debug')
      expect(loggerDevelopmentConfig.name).toBe('qicore-dev')
      expect(loggerDevelopmentConfig.pretty).toBe(true)
    })

    test('loggerProductionConfig has correct settings', () => {
      expect(loggerProductionConfig.level).toBe('info')
      expect(loggerProductionConfig.name).toBe('qicore-prod')
      expect(loggerProductionConfig.pretty).toBe(false)
      expect(loggerProductionConfig.redact).toContain('password')
      expect(loggerProductionConfig.redact).toContain('token')
    })

    test('loggerTestConfig has correct settings', () => {
      expect(loggerTestConfig.level).toBe('error')
      expect(loggerTestConfig.name).toBe('qicore-test')
      expect(loggerTestConfig.pretty).toBe(false)
    })

    test('getLoggerEnvironmentConfig returns correct config based on NODE_ENV', () => {
      // Test development (default)
      const devConfig = getLoggerEnvironmentConfig()
      expect(devConfig).toEqual(loggerDevelopmentConfig)

      // Test production
      process.env.NODE_ENV = 'production'
      const prodConfig = getLoggerEnvironmentConfig()
      expect(prodConfig).toEqual(loggerProductionConfig)

      // Test test
      process.env.NODE_ENV = 'test'
      const testEnvConfig = getLoggerEnvironmentConfig()
      expect(testEnvConfig).toEqual(loggerTestConfig)

      // Clean up
      process.env.NODE_ENV = undefined
    })
  })

  describe('Event System', () => {
    test('emits log events when logging', () => {
      const logger = new Logger({ level: 'info' })
      const logSpy = vi.fn()
      const errorSpy = vi.fn()
      const levelSpy = vi.fn()

      logger.on('log', logSpy)
      logger.on('error', errorSpy)
      logger.on('level', levelSpy)

      logger.info('Test message', { key: 'value' })
      logger.setLevel('debug')

      expect(logSpy).toHaveBeenCalledOnce()
      expect(levelSpy).toHaveBeenCalledWith('debug')
      expect(errorSpy).not.toHaveBeenCalled()
    })

    test('supports one-time event listeners', () => {
      const logger = new Logger({ level: 'info' })
      const onceSpy = vi.fn()

      logger.once('log', onceSpy)

      logger.info('First message')
      logger.info('Second message')

      expect(onceSpy).toHaveBeenCalledOnce()
    })

    test('removes event listeners', () => {
      const logger = new Logger({ level: 'info' })
      const logSpy = vi.fn()

      logger.on('log', logSpy)
      logger.info('First message')

      logger.off('log', logSpy)
      logger.info('Second message')

      expect(logSpy).toHaveBeenCalledOnce()
    })
  })

  describe('Integration', () => {
    test('works with different log levels and filtering', () => {
      const logger = new Logger({ level: 'warn' })
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      logger.debug('Debug message') // Should not log
      logger.info('Info message') // Should not log
      logger.warn('Warning message') // Should log
      logger.error('Error message') // Should log

      expect(logSpy).toHaveBeenCalledTimes(2)
      expect(logSpy.mock.calls[0][0].level).toBe('warn')
      expect(logSpy.mock.calls[1][0].level).toBe('error')
    })

    test('child loggers share event emitter', () => {
      const logger = new Logger({ level: 'info' })
      const childLogger = logger.child({ service: 'auth' })

      const logSpy = vi.fn()
      logger.on('log', logSpy) // Listen on parent

      childLogger.info('Child log message')

      expect(logSpy).toHaveBeenCalledOnce()
    })

    test('handles complex context objects', () => {
      const logger = new Logger({ level: 'info' })
      const logSpy = vi.fn()
      logger.on('log', logSpy)

      const complexContext = {
        user: { id: 123, name: 'Alice' },
        request: { method: 'POST', headers: { 'content-type': 'application/json' } },
        metadata: { version: '1.0.0', timestamp: new Date() },
      }

      logger.info('Complex context', complexContext)

      expect(logSpy).toHaveBeenCalledOnce()
      const logEntry: LogEntry = logSpy.mock.calls[0][0]
      expect(logEntry.context).toEqual(complexContext)
    })
  })
})
