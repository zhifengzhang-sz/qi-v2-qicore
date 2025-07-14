/**
 * Unit tests for Result<T> implementation
 */

import { type QiError, validationError } from '@qi/base'
import {
  Err,
  Ok,
  type Result,
  apply,
  asyncMap,
  asyncTryCatch,
  combine2,
  filter,
  flatMap,
  fromNullable,
  getError,
  getValue,
  isFailure,
  isSuccess,
  map,
  match,
  orElse,
  partition,
  pure,
  sequence,
  traverse,
  tryCatch,
  unwrapOr,
} from '@qi/base'
import { describe, expect, test } from 'vitest'

describe('Result<T> Factory Functions', () => {
  test('Ok creates success result', () => {
    const result = Ok(42)
    expect(result.tag).toBe('success')
    if (isSuccess(result)) {
      expect(result.value).toBe(42)
    }
  })

  test('Err creates failure result', () => {
    const error = validationError('Test error')
    const result = Err(error)
    expect(result.tag).toBe('failure')
    if (isFailure(result)) {
      expect(result.error).toEqual(error)
    }
  })

  test('Ok with different types', () => {
    const stringResult = Ok('hello')
    const arrayResult = Ok([1, 2, 3])
    const objectResult = Ok({ name: 'test' })
    const nullResult = Ok(null)
    const undefinedResult = Ok(undefined)

    if (isSuccess(stringResult)) expect(stringResult.value).toBe('hello')
    if (isSuccess(arrayResult)) expect(arrayResult.value).toEqual([1, 2, 3])
    if (isSuccess(objectResult)) expect(objectResult.value).toEqual({ name: 'test' })
    if (isSuccess(nullResult)) expect(nullResult.value).toBe(null)
    if (isSuccess(undefinedResult)) expect(undefinedResult.value).toBe(undefined)
  })
})

describe('Result<T> Type Guards', () => {
  test('isSuccess identifies success results', () => {
    expect(isSuccess(Ok(42))).toBe(true)
    expect(isSuccess(Err(validationError('error')))).toBe(false)
  })

  test('isFailure identifies failure results', () => {
    expect(isFailure(Err(validationError('error')))).toBe(true)
    expect(isFailure(Ok(42))).toBe(false)
  })

  test('getValue and getError work correctly', () => {
    const success = Ok(42)
    const failure = Err(validationError('error'))

    expect(getValue(success)).toBe(42)
    expect(getValue(failure)).toBe(undefined)
    expect(getError(success)).toBe(undefined)
    expect(getError(failure)).toEqual(validationError('error'))
  })
})

describe('Pure Function API', () => {
  test('map transforms success values', () => {
    const result = map((x: number) => x * 2, Ok(42))
    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe(84)
    }
  })

  test('map chains multiple transformations', () => {
    const step1 = map((x: number) => x * 2, Ok(10))
    const step2 = map((x: number) => x + 5, step1)
    const result = map((x: number) => x.toString(), step2)

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe('25')
    }
  })

  test('map preserves failure', () => {
    const error = validationError('Test error')
    const result = map((x: number) => x * 2, Err<QiError>(error))

    expect(result.tag).toBe('failure')
    if (result.tag === 'failure') {
      expect(result.error).toEqual(error)
    }
  })

  test('flatMap enables monadic composition', () => {
    const parseNumber = (s: string): Result<number> =>
      Number.isNaN(Number(s)) ? Err(validationError('Not a number')) : Ok(Number(s))

    const result = flatMap(parseNumber, Ok('42'))
    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe(42)
    }
  })

  test('flatMap chains operations', () => {
    const parseNumber = (s: string): Result<number> =>
      Number.isNaN(Number(s)) ? Err(validationError('Not a number')) : Ok(Number(s))

    const step1 = flatMap(parseNumber, Ok('42'))
    const result = flatMap((x: number) => Ok(x * 2), step1)

    if (result.tag === 'success') {
      expect(result.value).toBe(84)
    }
  })

  test('filter validates success values', () => {
    const isEven = (n: number) => n % 2 === 0
    const error = validationError('Not even')

    const evenResult = filter(isEven, error, Ok(42))
    const oddResult = filter(isEven, error, Ok(43))

    expect(isSuccess(evenResult)).toBe(true)
    expect(isFailure(oddResult)).toBe(true)
  })

  test('apply enables applicative operations', () => {
    const addFn = (x: number) => (y: number) => x + y
    const fnResult = Ok(addFn(5))
    const valueResult = Ok(10)

    const result = apply(fnResult, valueResult)
    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe(15)
    }
  })

  test('unwrapOr extracts values safely', () => {
    expect(unwrapOr(0, Ok(42))).toBe(42)
    expect(unwrapOr(0, Err(validationError('error')))).toBe(0)
  })

  test('orElse provides error recovery', () => {
    const recovery = (_: QiError) => Ok(100)

    expect(unwrapOr(0, orElse(recovery, Ok(42)))).toBe(42)
    expect(unwrapOr(0, orElse(recovery, Err(validationError('error'))))).toBe(100)
  })

  test('asyncMap handles async transformations', async () => {
    const asyncDouble = async (x: number) => x * 2

    const successResult = await asyncMap(asyncDouble, Ok(21))
    const failureResult = await asyncMap(asyncDouble, Err(validationError('error')))

    expect(successResult.tag).toBe('success')
    if (successResult.tag === 'success') {
      expect(successResult.value).toBe(42)
    }

    expect(failureResult.tag).toBe('failure')
  })

  test('async error handling works correctly', async () => {
    const asyncThrow = async (_: number) => {
      throw new Error('Async error')
    }

    const result = await asyncMap(asyncThrow, Ok(42))
    expect(result.tag).toBe('failure')
  })
})

describe('Utility Functions', () => {
  test('fromNullable handles null/undefined', () => {
    const error = validationError('Value is null')

    expect(isSuccess(fromNullable(42, error))).toBe(true)
    expect(isFailure(fromNullable(null, error))).toBe(true)
    expect(isFailure(fromNullable(undefined, error))).toBe(true)
  })

  test('tryCatch handles exceptions', () => {
    const safeDiv = (a: number, b: number) => {
      if (b === 0) throw new Error('Division by zero')
      return a / b
    }

    const success = tryCatch(() => safeDiv(10, 2))
    const failure = tryCatch(() => safeDiv(10, 0))

    expect(isSuccess(success)).toBe(true)
    expect(isFailure(failure)).toBe(true)
  })

  test('asyncTryCatch handles async exceptions', async () => {
    const asyncDiv = async (a: number, b: number) => {
      if (b === 0) throw new Error('Division by zero')
      return a / b
    }

    const success = await asyncTryCatch(() => asyncDiv(10, 2))
    const failure = await asyncTryCatch(() => asyncDiv(10, 0))

    expect(isSuccess(success)).toBe(true)
    expect(isFailure(failure)).toBe(true)
  })

  test('match enables pattern matching', () => {
    const successResult = match(
      (value: number) => `Success: ${value}`,
      (error: QiError) => `Error: ${error.message}`,
      Ok(42)
    )

    const failureResult = match(
      (value: number) => `Success: ${value}`,
      (error: QiError) => `Error: ${error.message}`,
      Err(validationError('test error'))
    )

    expect(successResult).toBe('Success: 42')
    expect(failureResult).toBe('Error: test error')
  })
})

describe('Collection Operations', () => {
  test('sequence collects all successes', () => {
    const results = [Ok(1), Ok(2), Ok(3)]
    const sequenced = sequence(results)

    expect(isSuccess(sequenced)).toBe(true)
    if (isSuccess(sequenced)) {
      expect(sequenced.value).toEqual([1, 2, 3])
    }
  })

  test('sequence fails fast on first failure', () => {
    const results = [Ok(1), Err(validationError('error')), Ok(3)]
    const sequenced = sequence(results)

    expect(isFailure(sequenced)).toBe(true)
  })

  test('traverse maps and sequences', () => {
    const parseNumber = (s: string): Result<number> =>
      Number.isNaN(Number(s)) ? Err(validationError('Not a number')) : Ok(Number(s))

    const success = traverse(parseNumber, ['1', '2', '3'])
    const failure = traverse(parseNumber, ['1', 'not-a-number', '3'])

    expect(isSuccess(success)).toBe(true)
    expect(isFailure(failure)).toBe(true)
  })

  test('partition separates successes and failures', () => {
    const results = [
      Ok(1),
      Err(validationError('error1')),
      Ok(2),
      Err(validationError('error2')),
      Ok(3),
    ]

    const { successes, failures } = partition(results)

    expect(successes).toEqual([1, 2, 3])
    expect(failures).toHaveLength(2)
  })

  test('combine2 merges two Results', () => {
    const add = (a: number, b: number) => a + b

    const both = combine2(Ok(5), Ok(10), add)
    const firstFail = combine2(Err(validationError('error')), Ok(10), add)
    const secondFail = combine2(Ok(5), Err(validationError('error')), add)

    expect(isSuccess(both)).toBe(true)
    if (isSuccess(both)) {
      expect(both.value).toBe(15)
    }

    expect(isFailure(firstFail)).toBe(true)
    expect(isFailure(secondFail)).toBe(true)
  })
})

describe('Applicative Laws', () => {
  test('pure creates success Result', () => {
    const result = pure(42)
    expect(isSuccess(result)).toBe(true)
    expect(getValue(result)).toBe(42)
  })

  test('identity law: apply(pure(id), result) === result', () => {
    const id = <T>(x: T) => x
    const result = Ok(42)
    const applied = apply(pure(id<number>), result)

    expect(applied).toEqual(result)
  })

  test('homomorphism law: apply(pure(f), pure(x)) === pure(f(x))', () => {
    const f = (x: number) => x * 2
    const x = 42

    const left = apply(pure(f), pure(x))
    const right = pure(f(x))

    expect(left).toEqual(right)
  })
})
