/**
 * Result<T> Tests - Contract Compliance
 */

import { describe, it, expect } from 'vitest'
import {
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  isSuccess,
  isFailure,
  getValue,
  getError,
  map,
  flatMap,
  unwrap,
  unwrapOr,
  match,
  create,
} from '@qi/base'

describe('Result<T> Factory Operations', () => {
  it('success creates successful result', () => {
    const result = success(42)
    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe(42)
    }
    expect(isSuccess(result)).toBe(true)
    expect(isFailure(result)).toBe(false)
  })

  it('failure creates failed result', () => {
    const error = create('TEST_ERROR', 'Test error', 'SYSTEM')
    const result = failure(error)
    expect(result.tag).toBe('failure')
    if (result.tag === 'failure') {
      expect(result.error).toBe(error)
    }
    expect(isFailure(result)).toBe(true)
    expect(isSuccess(result)).toBe(false)
  })

  it('fromTryCatch handles success', () => {
    const result = fromTryCatch(() => 42)
    expect(isSuccess(result)).toBe(true)
    expect(getValue(result)).toBe(42)
  })

  it('fromTryCatch handles exceptions', () => {
    const result = fromTryCatch(() => {
      throw new Error('Test error')
    })
    expect(isFailure(result)).toBe(true)
    expect(getError(result)?.message).toBe('Test error')
  })

  it('fromAsyncTryCatch handles async success', async () => {
    const result = await fromAsyncTryCatch(async () => 42)
    expect(isSuccess(result)).toBe(true)
    expect(getValue(result)).toBe(42)
  })

  it('fromAsyncTryCatch handles async exceptions', async () => {
    const result = await fromAsyncTryCatch(async () => {
      throw new Error('Async error')
    })
    expect(isFailure(result)).toBe(true)
    expect(getError(result)?.message).toBe('Async error')
  })
})

describe('Result<T> Query Operations', () => {
  const successResult = success(42)
  const errorResult = failure(create('ERROR', 'Error message', 'SYSTEM'))

  it('isSuccess correctly identifies success', () => {
    expect(isSuccess(successResult)).toBe(true)
    expect(isSuccess(errorResult)).toBe(false)
  })

  it('isFailure correctly identifies failure', () => {
    expect(isFailure(errorResult)).toBe(true)
    expect(isFailure(successResult)).toBe(false)
  })

  it('getValue returns value for success', () => {
    expect(getValue(successResult)).toBe(42)
    expect(getValue(errorResult)).toBe(null)
  })

  it('getError returns error for failure', () => {
    expect(getError(errorResult)?.message).toBe('Error message')
    expect(getError(successResult)).toBe(null)
  })
})

describe('Result<T> Transformation Operations', () => {
  it('map transforms success values', () => {
    const result = success(42)
    const mapped = map((x: number) => x * 2, result)
    expect(isSuccess(mapped)).toBe(true)
    expect(getValue(mapped)).toBe(84)
  })

  it('map preserves failures', () => {
    const error = create('ERROR', 'Error message', 'SYSTEM')
    const result = failure(error)
    const mapped = map((x: number) => x * 2, result)
    expect(isFailure(mapped)).toBe(true)
    expect(getError(mapped)).toBe(error)
  })

  it('flatMap chains successful operations', () => {
    const result = success(42)
    const chained = flatMap((x: number) => success(x * 2), result)
    expect(isSuccess(chained)).toBe(true)
    expect(getValue(chained)).toBe(84)
  })

  it('flatMap propagates failures', () => {
    const error = create('ERROR', 'Error message', 'SYSTEM')
    const result = failure(error)
    const chained = flatMap((x: number) => success(x * 2), result)
    expect(isFailure(chained)).toBe(true)
    expect(getError(chained)).toBe(error)
  })
})

describe('Result<T> Extraction Operations', () => {
  it('unwrap extracts value from success', () => {
    const result = success(42)
    expect(unwrap(result)).toBe(42)
  })

  it('unwrap throws for failure', () => {
    const result = failure(create('ERROR', 'Error message', 'SYSTEM'))
    expect(() => unwrap(result)).toThrow()
  })

  it('unwrapOr returns value for success', () => {
    const result = success(42)
    expect(unwrapOr(0, result)).toBe(42)
  })

  it('unwrapOr returns default for failure', () => {
    const result = failure(create('ERROR', 'Error message', 'SYSTEM'))
    expect(unwrapOr(0, result)).toBe(0)
  })

  it('match handles both success and failure', () => {
    const successResult = success(42)
    const failureResult = failure(create('ERROR', 'Error message', 'SYSTEM'))

    const successValue = match(
      (value: number) => value * 2,
      (_error) => -1,
      successResult
    )
    expect(successValue).toBe(84)

    const failureValue = match(
      (value: number) => value * 2,
      (_error) => -1,
      failureResult
    )
    expect(failureValue).toBe(-1)
  })
})
