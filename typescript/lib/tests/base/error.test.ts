/**
 * Unit tests for QiError implementation
 */

import {
  ErrorCategories,
  type QiError,
  businessError,
  chainError,
  createAggregateError,
  createError,
  createErrorCode,
  deserializeError,
  errorBuilder,
  formatErrorChain,
  getContext,
  getRetryStrategy,
  getRootCause,
  hasCategory,
  isErrorCategory,
  networkError,
  serializeError,
  systemError,
  validationError,
  withContext,
} from '@qi/base'
import { beforeEach, describe, expect, test } from 'vitest'

describe('Error Code Creation', () => {
  test('createErrorCode creates branded error code', () => {
    const code = createErrorCode('TEST_ERROR')
    // Branded types are structurally identical to strings at runtime
    expect(typeof code).toBe('string')
    expect(code).toBe('TEST_ERROR')
  })

  test('different error codes have different types at compile time', () => {
    const code1 = createErrorCode('ERROR_1')
    const code2 = createErrorCode('ERROR_2')
    expect(code1).not.toBe(code2)
  })
})

describe('Error Category Validation', () => {
  test('isErrorCategory validates valid categories', () => {
    expect(isErrorCategory('VALIDATION')).toBe(true)
    expect(isErrorCategory('NETWORK')).toBe(true)
    expect(isErrorCategory('SYSTEM')).toBe(true)
    expect(isErrorCategory('BUSINESS')).toBe(true)
  })

  test('isErrorCategory rejects invalid categories', () => {
    expect(isErrorCategory('INVALID')).toBe(false)
    expect(isErrorCategory('validation')).toBe(false) // case sensitive
    expect(isErrorCategory('')).toBe(false)
    expect(isErrorCategory('UNKNOWN')).toBe(false)
  })

  test('ErrorCategories contains all valid categories', () => {
    expect(ErrorCategories).toEqual([
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
    ])
    expect(ErrorCategories).toHaveLength(11)
  })
})

describe('Error Creation', () => {
  test('createError creates complete error object', () => {
    const context = { userId: '123', operation: 'save' }
    const error = createError({
      code: 'USER_SAVE_FAILED',
      message: 'Failed to save user data',
      category: 'SYSTEM',
      context,
      severity: 'high',
    })

    expect(error.code).toBe('USER_SAVE_FAILED')
    expect(error.message).toBe('Failed to save user data')
    expect(error.category).toBe('SYSTEM')
    expect(error.context).toEqual(context)
    expect(error.severity).toBe('high')
    expect(error.timestamp).toBeInstanceOf(Date)
    expect(error.cause).toBeUndefined()
  })

  test('createError with minimal parameters', () => {
    const error = createError({
      code: 'SIMPLE_ERROR',
      message: 'Something went wrong',
      category: 'VALIDATION',
    })

    expect(error.code).toBe('SIMPLE_ERROR')
    expect(error.message).toBe('Something went wrong')
    expect(error.category).toBe('VALIDATION')
    expect(error.context).toEqual({})
    expect(error.severity).toBeUndefined()
    expect(error.timestamp).toBeInstanceOf(Date)
  })

  test('createError accepts branded error code', () => {
    const code = createErrorCode('BRANDED_ERROR')
    const error = createError({
      code,
      message: 'Test message',
      category: 'NETWORK',
    })

    expect(error.code).toBe(code)
  })
})

describe('Error Builder Pattern', () => {
  test('errorBuilder creates error fluently', () => {
    const error = errorBuilder()
      .withCode('FLUENT_ERROR')
      .withMessage('Built fluently')
      .withCategory('BUSINESS')
      .withContext({ key: 'value' })
      .withSeverity('medium')
      .build()

    expect(error.code).toBe('FLUENT_ERROR')
    expect(error.message).toBe('Built fluently')
    expect(error.category).toBe('BUSINESS')
    expect(error.context).toEqual({ key: 'value' })
    expect(error.severity).toBe('medium')
  })

  test('errorBuilder throws on incomplete build', () => {
    expect(() => errorBuilder().withCode('INCOMPLETE').build()).toThrow()
    expect(() => errorBuilder().withMessage('Incomplete').build()).toThrow()
    expect(() => errorBuilder().withCategory('VALIDATION').build()).toThrow()
  })

  test('errorBuilder accumulates context', () => {
    const error = errorBuilder()
      .withCode('CONTEXT_ERROR')
      .withMessage('Context test')
      .withCategory('SYSTEM')
      .withContext({ first: 'value1' })
      .withContext({ second: 'value2' })
      .build()

    expect(error.context).toEqual({ first: 'value1', second: 'value2' })
  })

  test('errorBuilder with cause chain', () => {
    const rootCause = validationError('Root cause')
    const error = errorBuilder()
      .withCode('CHAINED_ERROR')
      .withMessage('Error with cause')
      .withCategory('SYSTEM')
      .withCause(rootCause)
      .build()

    expect(error.cause).toEqual(rootCause)
  })
})

describe('Predefined Error Factories', () => {
  test('validationError creates validation error', () => {
    const error = validationError('Invalid input', { field: 'email' })

    expect(error.code).toBe('VALIDATION_FAILED')
    expect(error.message).toBe('Invalid input')
    expect(error.category).toBe('VALIDATION')
    expect(error.context).toEqual({ field: 'email' })
    expect(error.severity).toBe('medium')
  })

  test('networkError creates network error', () => {
    const error = networkError('Connection timeout')

    expect(error.code).toBe('NETWORK_ERROR')
    expect(error.category).toBe('NETWORK')
    expect(error.severity).toBe('high')
  })

  test('systemError creates system error', () => {
    const error = systemError('Database connection failed')

    expect(error.code).toBe('SYSTEM_ERROR')
    expect(error.category).toBe('SYSTEM')
    expect(error.severity).toBe('high')
  })

  test('businessError creates business logic error', () => {
    const error = businessError('Insufficient funds')

    expect(error.code).toBe('BUSINESS_LOGIC_ERROR')
    expect(error.category).toBe('BUSINESS')
    expect(error.severity).toBe('medium')
  })
})

describe('Retry Strategy', () => {
  test('getRetryStrategy for VALIDATION category', () => {
    const error = validationError('Test error')
    const strategy = getRetryStrategy(error)

    expect(strategy.shouldRetry).toBe(false)
    expect(strategy.maxRetries).toBe(0)
  })

  test('getRetryStrategy for NETWORK category', () => {
    const error = networkError('Connection failed')
    const strategy = getRetryStrategy(error)

    expect(strategy.shouldRetry).toBe(true)
    expect(strategy.maxRetries).toBe(3)
    expect(strategy.backoffMs).toBe(1000)
    expect(strategy.maxBackoffMs).toBe(30000)
  })

  test('getRetryStrategy for SYSTEM category', () => {
    const error = systemError('Service unavailable')
    const strategy = getRetryStrategy(error)

    expect(strategy.shouldRetry).toBe(true)
    expect(strategy.maxRetries).toBe(2)
    expect(strategy.backoffMs).toBe(5000)
    expect(strategy.maxBackoffMs).toBe(60000)
  })

  test('getRetryStrategy for BUSINESS category', () => {
    const error = businessError('Business rule violation')
    const strategy = getRetryStrategy(error)

    expect(strategy.shouldRetry).toBe(false)
    expect(strategy.maxRetries).toBe(0)
  })
})

describe('Error Categorization', () => {
  test('hasCategory checks error category', () => {
    const error = networkError('Test error')

    expect(hasCategory(error, 'NETWORK')).toBe(true)
    expect(hasCategory(error, 'VALIDATION')).toBe(false)
    expect(hasCategory(error, 'SYSTEM')).toBe(false)
    expect(hasCategory(error, 'BUSINESS')).toBe(false)
  })
})

describe('Error Chaining', () => {
  test('chainError creates error with cause', () => {
    const originalError = validationError('Original error')
    const chainedError = chainError(originalError, {
      code: createErrorCode('CHAINED_ERROR'),
      message: 'Chained error message',
      category: 'SYSTEM',
      context: { operation: 'chain' },
    })

    expect(chainedError.cause).toEqual(originalError)
    expect(chainedError.code).toBe('CHAINED_ERROR')
    expect(chainedError.message).toBe('Chained error message')
    expect(chainedError.category).toBe('SYSTEM')
  })

  test('formatErrorChain formats error chain', () => {
    const rootError = validationError('Root error')
    const middleError = chainError(rootError, {
      code: createErrorCode('MIDDLE_ERROR'),
      message: 'Middle error',
      category: 'SYSTEM',
      context: {},
    })
    const topError = chainError(middleError, {
      code: createErrorCode('TOP_ERROR'),
      message: 'Top error',
      category: 'NETWORK',
      context: {},
    })

    const chain = formatErrorChain(topError)
    expect(chain).toEqual([
      'TOP_ERROR: Top error',
      'MIDDLE_ERROR: Middle error',
      'VALIDATION_FAILED: Root error',
    ])
  })

  test('formatErrorChain limits depth', () => {
    let error = validationError('Root error')

    // Create a deep chain
    for (let i = 1; i <= 15; i++) {
      error = chainError(error, {
        code: createErrorCode(`ERROR_${i}`),
        message: `Error ${i}`,
        category: 'SYSTEM',
        context: {},
      })
    }

    const chain = formatErrorChain(error, 5)
    expect(chain).toHaveLength(6) // 5 errors + truncation message
    expect(chain[5]).toBe('... (chain truncated)')
  })

  test('getRootCause finds root of error chain', () => {
    const rootError = validationError('Root error')
    const chainedError = chainError(rootError, {
      code: createErrorCode('CHAINED_ERROR'),
      message: 'Chained error',
      category: 'SYSTEM',
      context: {},
    })

    expect(getRootCause(chainedError)).toEqual(rootError)
    expect(getRootCause(rootError)).toEqual(rootError) // No cause
  })
})

describe('Error Serialization', () => {
  let error: QiError

  beforeEach(() => {
    const rootCause = validationError('Root cause')
    error = chainError(rootCause, {
      code: createErrorCode('SERIALIZATION_TEST'),
      message: 'Test error for serialization',
      category: 'SYSTEM',
      context: { userId: '123', operation: 'test' },
      severity: 'high',
    })
  })

  test('serializeError converts to JSON-compatible format', () => {
    const serialized = serializeError(error)

    expect(serialized.code).toBe('SERIALIZATION_TEST')
    expect(serialized.message).toBe('Test error for serialization')
    expect(serialized.category).toBe('SYSTEM')
    expect(serialized.context).toEqual({ userId: '123', operation: 'test' })
    expect(serialized.severity).toBe('high')
    expect(typeof serialized.timestamp).toBe('number')
    expect(serialized.cause).toBeDefined()
  })

  test('deserializeError reconstructs error from JSON', () => {
    const serialized = serializeError(error)
    const deserialized = deserializeError(serialized)

    expect(deserialized.code).toBe(error.code)
    expect(deserialized.message).toBe(error.message)
    expect(deserialized.category).toBe(error.category)
    expect(deserialized.context).toEqual(error.context)
    expect(deserialized.severity).toBe(error.severity)
    expect(deserialized.cause?.message).toBe(error.cause?.message)
  })

  test('deserializeError throws on invalid data', () => {
    expect(() => deserializeError({})).toThrow()
    expect(() => deserializeError({ code: 'TEST' })).toThrow()
    expect(() =>
      deserializeError({
        code: 'TEST',
        message: 'Test',
        category: 'INVALID_CATEGORY',
      })
    ).toThrow()
  })

  test('round-trip serialization preserves error structure', () => {
    const serialized = serializeError(error)
    const deserialized = deserializeError(serialized)
    const reserializedAgain = serializeError(deserialized)

    expect(reserializedAgain).toEqual(serialized)
  })
})

describe('Aggregate Errors', () => {
  test('createAggregateError combines multiple errors', () => {
    const errors = [validationError('Error 1'), networkError('Error 2'), systemError('Error 3')]

    const aggregate = createAggregateError('Multiple errors occurred', errors)

    expect(aggregate.code).toBe('AGGREGATE_ERROR')
    expect(aggregate.message).toBe('Multiple errors occurred')
    expect(aggregate.category).toBe('SYSTEM')
    expect(aggregate.severity).toBe('high')
    expect(getContext(aggregate, 'errorCount')).toBe(3)
    expect(getContext(aggregate, 'errors')).toHaveLength(3)
  })

  test('createAggregateError with empty array', () => {
    const aggregate = createAggregateError('No errors', [])

    expect(aggregate.code).toBe('AGGREGATE_ERROR')
    expect(aggregate.message).toBe('No errors')
    expect(getContext(aggregate, 'errorCount')).toBe(0)
    expect(getContext(aggregate, 'errors')).toEqual([])
  })

  test('createAggregateError with single error', () => {
    const errors = [validationError('Single error')]
    const aggregate = createAggregateError('One error occurred', errors)

    expect(getContext(aggregate, 'errorCount')).toBe(1)
    expect(getContext(aggregate, 'errors')).toHaveLength(1)
  })

  test('createAggregateError preserves individual error details', () => {
    const error1 = validationError('Validation failed', { field: 'email' })
    const error2 = networkError('Network timeout', { url: 'https://api.example.com' })
    const errors = [error1, error2]

    const aggregate = createAggregateError('Multiple failures', errors)
    const aggregatedErrors = getContext(aggregate, 'errors') as QiError[]

    expect(aggregatedErrors).toHaveLength(2)
    expect(aggregatedErrors[0]?.code).toBe('VALIDATION_FAILED')
    expect(aggregatedErrors[0]?.message).toBe('Validation failed')
    expect(aggregatedErrors[0]?.category).toBe('VALIDATION')
    expect(aggregatedErrors[0]?.context.field).toBe('email')

    expect(aggregatedErrors[1]?.code).toBe('NETWORK_ERROR')
    expect(aggregatedErrors[1]?.message).toBe('Network timeout')
    expect(aggregatedErrors[1]?.category).toBe('NETWORK')
    expect(aggregatedErrors[1]?.context.url).toBe('https://api.example.com')
  })
})

describe('Context Management', () => {
  test('getContext extracts typed context values', () => {
    const error = createError({
      code: 'CONTEXT_TEST',
      message: 'Test',
      category: 'VALIDATION',
      context: {
        userId: '123',
        count: 42,
        active: true,
        data: { nested: 'value' },
      },
    })

    expect(getContext<string>(error, 'userId')).toBe('123')
    expect(getContext<number>(error, 'count')).toBe(42)
    expect(getContext<boolean>(error, 'active')).toBe(true)
    expect(getContext<object>(error, 'data')).toEqual({ nested: 'value' })
    expect(getContext<string>(error, 'missing')).toBeUndefined()
  })

  test('withContext adds context to existing error', async () => {
    const originalError = validationError('Test error', { original: 'value' })
    const enhancedError = withContext(originalError, {
      additional: 'context',
      timestamp: 12345,
    })

    expect(enhancedError.context).toEqual({
      original: 'value',
      additional: 'context',
      timestamp: 12345,
    })

    // Original error should be unchanged
    expect(originalError.context).toEqual({ original: 'value' })

    // Should have new timestamp
    expect(enhancedError.timestamp.getTime()).toBeGreaterThanOrEqual(
      originalError.timestamp.getTime()
    )
  })

  test('withContext overwrites existing keys', () => {
    const originalError = validationError('Test', { key: 'original' })
    const enhancedError = withContext(originalError, { key: 'updated' })

    expect(enhancedError.context.key).toBe('updated')
  })
})
