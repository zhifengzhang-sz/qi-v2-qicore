/**
 * Unit tests for Result<T> implementation
 */

import { type QiError, validationError } from '@/base/error.js'
import {
  Err,
  Ok,
  type Result,
  asyncTryCatch,
  combine2,
  from,
  fromNullable,
  isFailure,
  isSuccess,
  match,
  partition,
  sequence,
  traverse,
  traverseAsync,
  tryCatch,
} from '@/base/result.js'
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

  test('type guards provide correct narrowing', () => {
    const result: Result<number> = Ok(42)

    if (isSuccess(result)) {
      // TypeScript should know this is success
      expect(result.value).toBe(42)
    } else {
      // This shouldn't execute
      expect.fail('Should be success')
    }
  })
})

describe('Pattern Matching', () => {
  test('match handles success case', () => {
    const result = Ok(42)
    const matched = match(result, {
      success: (value) => `Got: ${value}`,
      failure: (error) => `Error: ${error.message}`,
    })
    expect(matched).toBe('Got: 42')
  })

  test('match handles failure case', () => {
    const error = validationError('Something went wrong')
    const result = Err(error)
    const matched = match(result, {
      success: (value) => `Got: ${value}`,
      failure: (error) => `Error: ${error.message}`,
    })
    expect(matched).toBe('Error: Something went wrong')
  })

  test('match with different return types', () => {
    const result = Ok('hello')

    const stringResult = match(result, {
      success: (value) => value.toUpperCase(),
      failure: () => 'ERROR',
    })
    expect(stringResult).toBe('HELLO')

    const numberResult = match(result, {
      success: (value) => value.length,
      failure: () => -1,
    })
    expect(numberResult).toBe(5)
  })
})

describe('Fluent API Builder', () => {
  test('map transforms success values', () => {
    const result = from(Ok(42))
      .map((x) => x * 2)
      .build()

    expect(result).toEqual(Ok(84))
  })

  test('map preserves failures', () => {
    const error = validationError('Test error')
    const result = from(Err(error))
      .map((x: unknown) => (x as number) * 2)
      .build()

    expect(result).toEqual(Err(error))
  })

  test('map chains multiple transformations', () => {
    const result = from(Ok(10))
      .map((x) => x * 2)
      .map((x) => x + 5)
      .map((x) => x.toString())
      .build()

    expect(result).toEqual(Ok('25'))
  })

  test('flatMap chains operations that return Results', () => {
    const safeDivide = (x: number): Result<number> =>
      x === 0 ? Err(validationError('Division by zero')) : Ok(10 / x)

    const result = from(Ok(2)).flatMap(safeDivide).build()

    expect(result).toEqual(Ok(5))
  })

  test('flatMap short-circuits on failure', () => {
    const safeDivide = (x: number): Result<number> =>
      x === 0 ? Err(validationError('Division by zero')) : Ok(10 / x)

    const result = from(Ok(0))
      .flatMap(safeDivide)
      .map((x) => x * 2) // This shouldn't execute
      .build()

    expect(result.tag).toBe('failure')
    if (result.tag === 'failure') {
      expect(result.error.message).toBe('Division by zero')
    }
  })

  test('filter with predicate', () => {
    const isEven = (x: number) => x % 2 === 0
    const error = () => validationError('Number is not even')

    const evenResult = from(Ok(4)).filter(isEven, error).build()
    expect(evenResult).toEqual(Ok(4))

    const oddResult = from(Ok(3)).filter(isEven, error).build()
    expect(oddResult.tag).toBe('failure')
  })

  test('apply for combining Results', () => {
    const add = (x: number) => (y: number) => x + y
    const result = from(Ok(5))
      .apply(from(Ok(3)).apply(Ok(add)).build())
      .build()

    expect(result).toEqual(Ok(8))
  })

  test('unwrap returns success value', () => {
    const value = from(Ok(42)).unwrap()
    expect(value).toBe(42)
  })

  test('unwrap throws on failure', () => {
    const error = validationError('Test error')
    expect(() => from(Err(error)).unwrap()).toThrow()
  })

  test('unwrapOr returns value or default', () => {
    expect(from(Ok(42)).unwrapOr(0)).toBe(42)
    expect(from(Err(validationError('error'))).unwrapOr(0)).toBe(0)
  })

  test('unwrapOrElse computes default from error', () => {
    const defaultFn = (error: QiError) => error.message.length

    expect(from(Ok(42)).unwrapOrElse(defaultFn)).toBe(42)
    expect(from(Err(validationError('test'))).unwrapOrElse(defaultFn)).toBe(4)
  })

  test('tap executes side effect on success', () => {
    let sideEffect = ''
    const result = from(Ok('hello'))
      .tap((value) => {
        sideEffect = value.toUpperCase()
      })
      .build()

    expect(result).toEqual(Ok('hello'))
    expect(sideEffect).toBe('HELLO')
  })

  test('tapError executes side effect on failure', () => {
    let sideEffect = ''
    const error = validationError('Test error')
    const result = from(Err(error))
      .tapError((error) => {
        sideEffect = error.message
      })
      .build()

    expect(result).toEqual(Err(error))
    expect(sideEffect).toBe('Test error')
  })
})

describe('Async Operations', () => {
  test('mapAsync transforms with async function', async () => {
    const asyncDouble = async (x: number): Promise<number> => {
      await new Promise((resolve) => setTimeout(resolve, 1))
      return x * 2
    }

    const result = await from(Ok(21)).mapAsync(asyncDouble)
    expect(result.build()).toEqual(Ok(42))
  })

  test('mapAsync handles async errors', async () => {
    const asyncFail = async (): Promise<number> => {
      throw new Error('Async error')
    }

    const result = await from(Ok(21)).mapAsync(asyncFail)
    expect(result.build().tag).toBe('failure')
  })

  test('flatMapAsync chains async operations', async () => {
    const asyncSafeDivide = async (x: number): Promise<Result<number>> => {
      await new Promise((resolve) => setTimeout(resolve, 1))
      return x === 0 ? Err(validationError('Division by zero')) : Ok(10 / x)
    }

    const result = await from(Ok(2)).flatMapAsync(asyncSafeDivide)
    expect(result.build()).toEqual(Ok(5))
  })
})

describe('Utility Functions', () => {
  test('tryCatch wraps throwing functions', () => {
    const throwing = () => {
      throw new Error('Oops')
    }
    const safe = () => 42

    expect(tryCatch(throwing).tag).toBe('failure')
    expect(tryCatch(safe)).toEqual(Ok(42))
  })

  test('asyncTryCatch wraps async throwing functions', async () => {
    const asyncThrowing = async () => {
      throw new Error('Async oops')
    }
    const asyncSafe = async () => 42

    expect((await asyncTryCatch(asyncThrowing)).tag).toBe('failure')
    expect(await asyncTryCatch(asyncSafe)).toEqual(Ok(42))
  })

  test('fromNullable converts nullable values', () => {
    const error = validationError('Value is null')

    expect(fromNullable(42, error)).toEqual(Ok(42))
    expect(fromNullable(null, error)).toEqual(Err(error))
    expect(fromNullable(undefined, error)).toEqual(Err(error))
  })

  test('sequence converts array of Results to Result of array', () => {
    const results = [Ok(1), Ok(2), Ok(3)]
    expect(sequence(results)).toEqual(Ok([1, 2, 3]))

    const withFailure = [Ok(1), Err(validationError('error')), Ok(3)]
    expect(sequence(withFailure).tag).toBe('failure')
  })

  test('traverse applies function to array', () => {
    const safeParse = (s: string): Result<number> => {
      const num = Number.parseInt(s, 10)
      return Number.isNaN(num) ? Err(validationError(`Invalid: ${s}`)) : Ok(num)
    }

    expect(traverse(['1', '2', '3'], safeParse)).toEqual(Ok([1, 2, 3]))
    expect(traverse(['1', 'invalid', '3'], safeParse).tag).toBe('failure')
  })

  test('traverseAsync applies async function to array', async () => {
    const asyncSafeParse = async (s: string): Promise<Result<number>> => {
      await new Promise((resolve) => setTimeout(resolve, 1))
      const num = Number.parseInt(s, 10)
      return Number.isNaN(num) ? Err(validationError(`Invalid: ${s}`)) : Ok(num)
    }

    const result = await traverseAsync(['1', '2', '3'], asyncSafeParse)
    expect(result).toEqual(Ok([1, 2, 3]))
  })

  test('combine2 combines two Results with function', () => {
    const add = (x: number, y: number) => x + y

    expect(combine2(Ok(3), Ok(5), add)).toEqual(Ok(8))
    expect(combine2(Err<QiError, number>(validationError('error')), Ok(5), add).tag).toBe('failure')
    expect(combine2(Ok(3), Err<QiError, number>(validationError('error')), add).tag).toBe('failure')
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
    expect(failures[0]?.message).toBe('error1')
    expect(failures[1]?.message).toBe('error2')
  })
})

describe('Complex Integration Scenarios', () => {
  test('complex fluent chain with multiple operations', () => {
    const parseAndProcess = (input: string) => {
      const parseResult = tryCatch(() => JSON.parse(input))
      const convertedResult: Result<unknown, QiError> =
        parseResult.tag === 'success'
          ? Ok(parseResult.value)
          : Err(validationError(parseResult.error.message))

      return from(convertedResult)
        .flatMap((obj: unknown) =>
          typeof (obj as { value?: unknown }).value === 'number'
            ? Ok((obj as { value: number }).value)
            : Err(validationError('Missing numeric value'))
        )
        .filter(
          (n: unknown) => (n as number) > 0,
          () => validationError('Value must be positive')
        )
        .map((n: unknown) => (n as number) * 2)
        .map((n: unknown) => ({ result: n as number, timestamp: new Date() }))
    }

    const validJson = '{"value": 5}'
    const result = parseAndProcess(validJson).build()

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value.result).toBe(10)
      expect(result.value.timestamp).toBeInstanceOf(Date)
    }
  })

  test('error propagation through complex chain', () => {
    const process = (input: number) =>
      from(Ok(input))
        .filter(
          (n) => n > 0,
          () => validationError('Must be positive')
        )
        .flatMap((n) => (n < 100 ? Ok(n) : Err(validationError('Too large'))))
        .map((n: unknown) => (n as number) * 2)
        .filter(
          (n: unknown) => (n as number) % 4 === 0,
          () => validationError('Result must be divisible by 4')
        )

    // Should succeed
    expect(process(2).build()).toEqual(Ok(4))

    // Should fail at first filter
    expect(process(-1).build().tag).toBe('failure')

    // Should fail at flatMap
    expect(process(150).build().tag).toBe('failure')

    // Should fail at final filter (3 * 2 = 6, not divisible by 4)
    expect(process(3).build().tag).toBe('failure')
  })
})
