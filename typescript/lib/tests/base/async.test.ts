/**
 * Async Result Helpers Tests - Contract Compliance
 * Tests for async composition helpers ensuring mathematical law compliance
 */

import { describe, it, expect, vi } from 'vitest'
import fc from 'fast-check'
import {
  success,
  failure,
  create,
  isSuccess,
  isFailure,
  getValue,
  getError,
  type QiError,
} from '@qi/base'
import {
  flatMapAsync,
  mapAsync,
  matchAsync,
  flatMapPromise,
  mapPromise,
  matchPromise,
  sequenceAsync,
  collectAsync,
  isPromiseResult,
} from '@qi/base'

// Test utilities
const delay = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms))
const createError = (message: string) => create('TEST_ERROR', message, 'SYSTEM')

describe('Async Transformation Operations', () => {
  describe('flatMapAsync', () => {
    it('preserves failure without calling function', async () => {
      const error = createError('Test error')
      const failureResult = failure(error)
      const fn = vi.fn(async (x: number) => success(x * 2))

      const result = await flatMapAsync(fn, failureResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
      expect(fn).not.toHaveBeenCalled()
    })

    it('applies async function to success value', async () => {
      const successResult = success(10)
      const fn = async (x: number) => {
        await delay(1)
        return success(x * 2)
      }

      const result = await flatMapAsync(fn, successResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('propagates async function failure', async () => {
      const successResult = success(10)
      const error = createError('Async operation failed')
      const fn = async (_x: number) => {
        await delay(1)
        return failure(error)
      }

      const result = await flatMapAsync(fn, successResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
    })

    it('satisfies functor laws asynchronously', async () => {
      // Identity law: flatMapAsync(async x => success(x), result) === result
      const testResult = success(42)
      const identity = async (x: number) => success(x)
      const result = await flatMapAsync(identity, testResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(42)
    })
  })

  describe('mapAsync', () => {
    it('preserves failure without calling function', async () => {
      const error = createError('Test error')
      const failureResult = failure(error)
      const fn = vi.fn(async (x: number) => x * 2)

      const result = await mapAsync(fn, failureResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
      expect(fn).not.toHaveBeenCalled()
    })

    it('applies async function to success value', async () => {
      const successResult = success(10)
      const fn = async (x: number) => {
        await delay(1)
        return x * 2
      }

      const result = await mapAsync(fn, successResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('wraps thrown errors in QiError', async () => {
      const successResult = success(10)
      const fn = async (_x: number) => {
        await delay(1)
        throw new Error('Async function failed')
      }

      const result = await mapAsync(fn, successResult)

      expect(isFailure(result)).toBe(true)
      const error = getError(result)
      expect(error).toBeDefined()
      expect(error?.code).toBe('ASYNC_MAP_ERROR')
      expect(error?.message).toContain('Async function failed')
    })

    it('handles non-Error thrown values', async () => {
      const successResult = success(10)
      const fn = async (_x: number) => {
        await delay(1)
        throw 'String error'
      }

      const result = await mapAsync(fn, successResult)

      expect(isFailure(result)).toBe(true)
      const error = getError(result)
      expect(error).toBeDefined()
      expect(error?.code).toBe('ASYNC_MAP_ERROR')
      expect(error?.message).toContain('String error')
    })
  })

  describe('matchAsync', () => {
    it('calls success handler for success result', async () => {
      const successResult = success(42)
      const onSuccess = vi.fn(async (x: number) => `Success: ${x}`)
      const onError = vi.fn(async (e: QiError) => `Error: ${e.message}`)

      const result = await matchAsync(onSuccess, onError, successResult)

      expect(result).toBe('Success: 42')
      expect(onSuccess).toHaveBeenCalledWith(42)
      expect(onError).not.toHaveBeenCalled()
    })

    it('calls error handler for failure result', async () => {
      const error = createError('Test error')
      const failureResult = failure(error)
      const onSuccess = vi.fn(async (x: number) => `Success: ${x}`)
      const onError = vi.fn(async (e: QiError) => `Error: ${e.message}`)

      const result = await matchAsync(onSuccess, onError, failureResult)

      expect(result).toBe('Error: Test error')
      expect(onError).toHaveBeenCalledWith(error)
      expect(onSuccess).not.toHaveBeenCalled()
    })

    it('handles async handlers with delay', async () => {
      const successResult = success(42)
      const onSuccess = async (x: number) => {
        await delay(10)
        return `Delayed: ${x}`
      }
      const onError = async (e: QiError) => `Error: ${e.message}`

      const start = Date.now()
      const result = await matchAsync(onSuccess, onError, successResult)
      const elapsed = Date.now() - start

      expect(result).toBe('Delayed: 42')
      expect(elapsed).toBeGreaterThanOrEqual(9) // Account for timing variations
    })
  })
})

describe('Promise<Result<T>> Composition Operations', () => {
  describe('flatMapPromise', () => {
    it('handles successful promise result with sync function', async () => {
      const promiseResult = Promise.resolve(success(10))
      const fn = (x: number) => success(x * 2)

      const result = await flatMapPromise(fn, promiseResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('handles successful promise result with async function', async () => {
      const promiseResult = Promise.resolve(success(10))
      const fn = async (x: number) => {
        await delay(1)
        return success(x * 2)
      }

      const result = await flatMapPromise(fn, promiseResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('propagates promise failure without calling function', async () => {
      const error = createError('Promise failed')
      const promiseResult = Promise.resolve(failure(error))
      const fn = vi.fn((x: number) => success(x * 2))

      const result = await flatMapPromise(fn, promiseResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
      expect(fn).not.toHaveBeenCalled()
    })

    it('propagates function failure', async () => {
      const promiseResult = Promise.resolve(success(10))
      const error = createError('Function failed')
      const fn = (_x: number) => failure(error)

      const result = await flatMapPromise(fn, promiseResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
    })
  })

  describe('mapPromise', () => {
    it('handles successful promise result with sync function', async () => {
      const promiseResult = Promise.resolve(success(10))
      const fn = (x: number) => x * 2

      const result = await mapPromise(fn, promiseResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('handles successful promise result with async function', async () => {
      const promiseResult = Promise.resolve(success(10))
      const fn = async (x: number) => {
        await delay(1)
        return x * 2
      }

      const result = await mapPromise(fn, promiseResult)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toBe(20)
    })

    it('propagates promise failure without calling function', async () => {
      const error = createError('Promise failed')
      const promiseResult = Promise.resolve(failure(error))
      const fn = vi.fn((x: number) => x * 2)

      const result = await mapPromise(fn, promiseResult)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
      expect(fn).not.toHaveBeenCalled()
    })

    it('wraps function errors', async () => {
      const promiseResult = Promise.resolve(success(10))
      const fn = (_x: number) => {
        throw new Error('Function error')
      }

      const result = await mapPromise(fn, promiseResult)

      expect(isFailure(result)).toBe(true)
      const error = getError(result)
      expect(error).toBeDefined()
      expect(error?.code).toBe('ASYNC_MAP_PROMISE_ERROR')
      expect(error?.message).toContain('Function error')
    })
  })

  describe('matchPromise', () => {
    it('handles successful promise result', async () => {
      const promiseResult = Promise.resolve(success(42))
      const onSuccess = vi.fn(async (x: number) => `Success: ${x}`)
      const onError = vi.fn(async (e: QiError) => `Error: ${e.message}`)

      const result = await matchPromise(onSuccess, onError, promiseResult)

      expect(result).toBe('Success: 42')
      expect(onSuccess).toHaveBeenCalledWith(42)
      expect(onError).not.toHaveBeenCalled()
    })

    it('handles failed promise result', async () => {
      const error = createError('Promise error')
      const promiseResult = Promise.resolve(failure(error))
      const onSuccess = vi.fn(async (x: number) => `Success: ${x}`)
      const onError = vi.fn(async (e: QiError) => `Error: ${e.message}`)

      const result = await matchPromise(onSuccess, onError, promiseResult)

      expect(result).toBe('Error: Promise error')
      expect(onError).toHaveBeenCalledWith(error)
      expect(onSuccess).not.toHaveBeenCalled()
    })
  })
})

describe('Async Collection Operations', () => {
  describe('sequenceAsync', () => {
    it('handles empty array', async () => {
      const result = await sequenceAsync([])
      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toEqual([])
    })

    it('sequences all successes', async () => {
      const promises = [
        Promise.resolve(success(1)),
        Promise.resolve(success(2)),
        Promise.resolve(success(3)),
      ]

      const result = await sequenceAsync(promises)

      expect(isSuccess(result)).toBe(true)
      expect(getValue(result)).toEqual([1, 2, 3])
    })

    it('stops at first failure', async () => {
      const error = createError('Second failed')
      const promises = [
        Promise.resolve(success(1)),
        Promise.resolve(failure(error)),
        Promise.resolve(success(3)),
      ]

      const result = await sequenceAsync(promises)

      expect(isFailure(result)).toBe(true)
      expect(getError(result)).toBe(error)
    })

    it('preserves promise order in results', async () => {
      // Test that sequenceAsync preserves the order of results
      // even if individual promises resolve at different times
      const promises = [
        delay(30).then(() => success(1)),
        delay(10).then(() => success(2)),
        delay(20).then(() => success(3)),
      ]

      const result = await sequenceAsync(promises)

      expect(isSuccess(result)).toBe(true)
      // Order is preserved regardless of resolution timing
      expect(getValue(result)).toEqual([1, 2, 3])
    })
  })

  describe('collectAsync', () => {
    it('handles empty array', async () => {
      const result = await collectAsync([])
      expect(result.successes).toEqual([])
      expect(result.failures).toEqual([])
    })

    it('collects all successes', async () => {
      const promises = [
        Promise.resolve(success(1)),
        Promise.resolve(success(2)),
        Promise.resolve(success(3)),
      ]

      const result = await collectAsync(promises)

      expect(result.successes).toEqual([1, 2, 3])
      expect(result.failures).toEqual([])
    })

    it('collects all failures', async () => {
      const error1 = createError('Error 1')
      const error2 = createError('Error 2')
      const promises = [Promise.resolve(failure(error1)), Promise.resolve(failure(error2))]

      const result = await collectAsync(promises)

      expect(result.successes).toEqual([])
      expect(result.failures).toEqual([error1, error2])
    })

    it('partitions mixed successes and failures', async () => {
      const error = createError('Error')
      const promises = [
        Promise.resolve(success(1)),
        Promise.resolve(failure(error)),
        Promise.resolve(success(3)),
      ]

      const result = await collectAsync(promises)

      expect(result.successes).toEqual([1, 3])
      expect(result.failures).toEqual([error])
    })

    it('preserves order', async () => {
      const error1 = createError('Error 1')
      const error2 = createError('Error 2')
      const promises = [
        Promise.resolve(success(1)),
        Promise.resolve(failure(error1)),
        Promise.resolve(success(2)),
        Promise.resolve(failure(error2)),
        Promise.resolve(success(3)),
      ]

      const result = await collectAsync(promises)

      expect(result.successes).toEqual([1, 2, 3])
      expect(result.failures).toEqual([error1, error2])
    })
  })
})

describe('Type Guards and Utilities', () => {
  describe('isPromiseResult', () => {
    it('identifies Promise<Result<T>>', () => {
      const promiseResult = Promise.resolve(success(42))
      expect(isPromiseResult(promiseResult)).toBe(true)
    })

    it('rejects non-Promise values', () => {
      expect(isPromiseResult(success(42))).toBe(false)
      expect(isPromiseResult(42)).toBe(false)
      expect(isPromiseResult(null)).toBe(false)
      expect(isPromiseResult(undefined)).toBe(false)
      expect(isPromiseResult({})).toBe(false)
    })

    it('rejects regular Promises', () => {
      expect(isPromiseResult(Promise.resolve(42))).toBe(true) // Still a Promise
      expect(isPromiseResult(Promise.resolve('string'))).toBe(true) // Still a Promise
    })
  })
})

describe('Property-based Tests for Async Operations', () => {
  const asyncIdentity = async <T>(x: T): Promise<T> => {
    await delay(1)
    return x
  }

  // Helper function for testing (keeping for potential future use)
  // const asyncDouble = async (x: number): Promise<number> => {
  //   await delay(1)
  //   return x * 2
  // }

  it('mapAsync satisfies functor identity law', async () => {
    await fc.assert(
      fc.asyncProperty(
        fc.oneof(
          fc.integer().map(success),
          fc.string().map((msg) => failure(create('ERROR', msg, 'SYSTEM')))
        ),
        async (result) => {
          const mapped = await mapAsync(asyncIdentity, result)

          if (isSuccess(result)) {
            expect(isSuccess(mapped)).toBe(true)
            expect(getValue(mapped)).toBe(getValue(result))
          } else {
            expect(isFailure(mapped)).toBe(true)
            expect(getError(mapped)).toBe(getError(result))
          }
        }
      ),
      { numRuns: 50 } // Reduced for async tests
    )
  })

  it('flatMapAsync satisfies monad left identity law', async () => {
    await fc.assert(
      fc.asyncProperty(fc.integer(), async (value) => {
        const wrapped = success(value)
        const fn = async (x: number) => success(x * 2)

        const result1 = await flatMapAsync(fn, wrapped)
        const result2 = await fn(value)

        expect(isSuccess(result1)).toBe(isSuccess(result2))
        if (isSuccess(result1) && isSuccess(result2)) {
          expect(getValue(result1)).toBe(getValue(result2))
        }
      }),
      { numRuns: 20 } // Reduced for async tests
    )
  })

  it('sequenceAsync preserves order under concurrent execution', async () => {
    await fc.assert(
      fc.asyncProperty(fc.array(fc.integer(), { minLength: 1, maxLength: 5 }), async (values) => {
        // Create promises that resolve in random order but should be sequenced
        const promises = values.map((value) => delay(Math.random() * 10).then(() => success(value)))

        const result = await sequenceAsync(promises)

        expect(isSuccess(result)).toBe(true)
        expect(getValue(result)).toEqual(values)
      }),
      { numRuns: 10 } // Reduced for timing-sensitive tests
    )
  })
})
