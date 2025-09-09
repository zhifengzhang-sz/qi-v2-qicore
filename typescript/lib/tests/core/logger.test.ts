/**
 * Logger Tests - Contract Compliance
 * Focus: Law, Interfaces, Behavior
 */

import { describe, it, expect } from 'vitest'
import {
  createLogger,
  createFromEnv,
  loggerError,
  type LoggerConfig,
  type LogLevel,
} from '@qi/core'
import { isSuccess, isFailure } from '@qi/base'

describe('Logger Factory Operations', () => {
  it('createLogger with valid config returns success', () => {
    const config: LoggerConfig = {
      level: 'info',
      name: 'test-logger',
      pretty: false,
    }

    const result = createLogger(config)
    expect(isSuccess(result)).toBe(true)
  })

  it('createLogger with invalid level returns failure', () => {
    const config = {
      level: 'invalid' as LogLevel,
      name: 'test-logger',
      pretty: false,
    }

    const result = createLogger(config)
    expect(isFailure(result)).toBe(true)
  })

  it('createFromEnv creates logger from environment', () => {
    const result = createFromEnv()
    expect(isSuccess(result)).toBe(true)
  })
})

describe('Logger Interface Operations', () => {
  const config: LoggerConfig = {
    level: 'debug',
    name: 'test-logger',
    pretty: false,
  }

  // Shared logger for non-mutating tests
  const loggerResult = createLogger(config)
  const logger = loggerResult.tag === 'success' ? loggerResult.value : null

  // Helper function to create fresh logger for mutating tests to avoid state pollution
  const createFreshLogger = () => {
    const freshLoggerResult = createLogger(config)
    return freshLoggerResult.tag === 'success' ? freshLoggerResult.value : null
  }

  it('logger has all required methods', () => {
    expect(logger).toBeDefined()
    expect(typeof logger?.debug).toBe('function')
    expect(typeof logger?.info).toBe('function')
    expect(typeof logger?.warn).toBe('function')
    expect(typeof logger?.error).toBe('function')
    expect(typeof logger?.isLevelEnabled).toBe('function')
    expect(typeof logger?.getLevel).toBe('function')
    expect(typeof logger?.setLevel).toBe('function')
    expect(typeof logger?.child).toBe('function')
    expect(typeof logger?.close).toBe('function')
  })

  it('getLevel returns current level', () => {
    expect(logger?.getLevel()).toBe('debug')
  })

  it('setLevel changes level', () => {
    const logger = createFreshLogger()
    logger?.setLevel('error')
    expect(logger?.getLevel()).toBe('error')
  })

  it('isLevelEnabled respects level hierarchy', () => {
    const logger = createFreshLogger()
    logger?.setLevel('warn')
    expect(logger?.isLevelEnabled('debug')).toBe(false)
    expect(logger?.isLevelEnabled('info')).toBe(false)
    expect(logger?.isLevelEnabled('warn')).toBe(true)
    expect(logger?.isLevelEnabled('error')).toBe(true)
  })

  it('child creates new logger with context', () => {
    const childLogger = logger?.child({ userId: '123' })
    expect(childLogger).toBeDefined()
    expect(childLogger?.getLevel()).toBe(logger?.getLevel())
  })

  it('getConfig returns logger configuration', () => {
    const returnedConfig = logger?.getConfig()
    expect(returnedConfig?.level).toBe('debug') // Fixed: should match config.level, not current getLevel()
    expect(returnedConfig?.name).toBe('test-logger')
    expect(returnedConfig?.pretty).toBe(false)
  })
})

describe('Logger Behavior Contract', () => {
  const config: LoggerConfig = {
    level: 'info',
    name: 'test-logger',
    pretty: false,
  }

  const loggerResult = createLogger(config)
  const logger = loggerResult.tag === 'success' ? loggerResult.value : null

  it('logging operations are side effects (return void)', () => {
    const debugResult = logger?.debug('Debug message')
    const infoResult = logger?.info('Info message')
    const warnResult = logger?.warn('Warn message')
    const errorResult = logger?.error('Error message')

    expect(debugResult).toBeUndefined()
    expect(infoResult).toBeUndefined()
    expect(warnResult).toBeUndefined()
    expect(errorResult).toBeUndefined()
  })

  it('respects log level filtering', () => {
    logger?.setLevel('warn')

    // These should not log (below level)
    expect(logger?.isLevelEnabled('debug')).toBe(false)
    expect(logger?.isLevelEnabled('info')).toBe(false)

    // These should log (at or above level)
    expect(logger?.isLevelEnabled('warn')).toBe(true)
    expect(logger?.isLevelEnabled('error')).toBe(true)
  })

  it('level checking should not fail under load', () => {
    const loggerResult = createLogger(config)
    const logger = loggerResult.tag === 'success' ? loggerResult.value : null

    // Test behavior instead of performance - ensure no exceptions under repeated calls
    expect(() => {
      for (let i = 0; i < 1000; i++) {
        logger?.isLevelEnabled('info')
      }
    }).not.toThrow()

    // Verify the calls still return correct values for logger at info level
    expect(logger?.isLevelEnabled('debug')).toBe(false) // debug < info, so should be false
    expect(logger?.isLevelEnabled('info')).toBe(true) // info == info, so should be true
    expect(logger?.isLevelEnabled('error')).toBe(true) // error > info, so should be true
  })

  it('supports structured logging with context', () => {
    // Should accept context object
    expect(() => {
      logger?.info('Message with context', { userId: '123', action: 'login' })
    }).not.toThrow()
  })

  it('supports error logging with error object', () => {
    const error = new Error('Test error')

    // Should accept error object
    expect(() => {
      logger?.error('Error occurred', error, { context: 'test' })
    }).not.toThrow()
  })
})

describe('Logger Error Factory', () => {
  it('loggerError creates logger-specific error', () => {
    const error = loggerError('Logger failed', { operation: 'write' })

    expect(error.category).toBe('LOGGER')
    expect(error.message).toBe('Logger failed')
    expect(error.context.operation).toBe('write')
  })

  it('loggerError defaults context to empty', () => {
    const error = loggerError('Logger failed')

    expect(error.category).toBe('LOGGER')
    expect(error.message).toBe('Logger failed')
    expect(error.context).toEqual({})
  })
})
