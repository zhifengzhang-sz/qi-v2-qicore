/**
 * Property-based tests for Monad laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Monad laws
 * using pure functions.
 */

import { createError } from '@qi/base'
import { Err, Ok, type Result, flatMap, pure } from '@qi/base'
import { describe, expect, test } from 'vitest'

describe('Monad Laws for Result<T>', () => {
  test('Left Identity Law: flatMap(f, pure(x)) === f(x)', () => {
    const f = (x: number): Result<string> => Ok(x.toString())
    const testValues = [0, 42, -1, 100]

    for (const x of testValues) {
      const leftSide = flatMap(f, pure(x))
      const rightSide = f(x)

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toBe(rightSide.value)
      }
    }
  })

  test('Right Identity Law: flatMap(pure, result) === result', () => {
    const testValues = [42, 'hello', [1, 2, 3], { name: 'test' }]

    for (const value of testValues) {
      const result = Ok(value)
      const flattened = flatMap(pure, result)

      expect(flattened.tag).toBe(result.tag)
      if (flattened.tag === 'success' && result.tag === 'success') {
        expect(flattened.value).toEqual(result.value)
      }
    }
  })

  test('Right Identity Law applies to failures', () => {
    const error = createError({
      code: 'TEST_ERROR',
      message: 'test-error',
      category: 'VALIDATION',
    })
    const result: Result<string> = Err(error)

    const flattened = flatMap(pure, result)

    expect(flattened.tag).toBe(result.tag)
    if (flattened.tag === 'failure' && result.tag === 'failure') {
      expect(flattened.error).toEqual(result.error)
    }
  })

  test('Associativity Law: flatMap(g, flatMap(f, result)) === flatMap(x => flatMap(g, f(x)), result)', () => {
    const f = (x: number): Result<string> => Ok(x.toString())
    const g = (s: string): Result<number> => Ok(s.length)

    const testValues = [0, 42, 123]

    for (const value of testValues) {
      const result = Ok(value)

      // Left side: flatMap(g, flatMap(f, result))
      const leftSide = flatMap(g, flatMap(f, result))

      // Right side: flatMap(x => flatMap(g, f(x)), result)
      const rightSide = flatMap((x: number) => flatMap(g, f(x)), result)

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toBe(rightSide.value)
      }
    }
  })

  test('Associativity Law with array operations', () => {
    const f = (x: number): Result<number[]> => Ok([x, x * 2])
    const g = (arr: number[]): Result<string[]> => Ok(arr.map((n) => n.toString()))

    const testValues = [1, 5, 10]

    for (const value of testValues) {
      const result = Ok(value)

      // Left side
      const leftSide = flatMap(g, flatMap(f, result))

      // Right side
      const rightSide = flatMap((x: number) => flatMap(g, f(x)), result)

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toEqual(rightSide.value)
      }
    }
  })

  test('Associativity Law with object transformations', () => {
    interface User {
      name: string
      age: number
    }
    interface UserProfile {
      user: User
      isActive: boolean
    }

    const f = (x: number): Result<User> => Ok({ name: `User${x}`, age: x })
    const g = (user: User): Result<UserProfile> => Ok({ user, isActive: user.age >= 18 })

    const testValues = [16, 18, 25]

    for (const value of testValues) {
      const result = Ok(value)

      const leftSide = flatMap(g, flatMap(f, result))
      const rightSide = flatMap((x: number) => flatMap(g, f(x)), result)

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toEqual(rightSide.value)
      }
    }
  })

  test('Monad laws with failure propagation', () => {
    const error = createError({
      code: 'CALC_ERROR',
      message: 'calculation failed',
      category: 'BUSINESS',
    })

    const f = (x: number): Result<number> => (x > 0 ? Ok(x * 2) : Err(error))

    const g = (x: number): Result<string> => (x < 100 ? Ok(x.toString()) : Err(error))

    // Test with value that fails in f
    const failingResult = Ok(-1)
    const leftFail = flatMap(g, flatMap(f, failingResult))
    const rightFail = flatMap((x: number) => flatMap(g, f(x)), failingResult)

    expect(leftFail.tag).toBe('failure')
    expect(rightFail.tag).toBe('failure')

    if (leftFail.tag === 'failure' && rightFail.tag === 'failure') {
      expect(leftFail.error).toEqual(rightFail.error)
    }
  })

  test('Complex monad composition preserves types', () => {
    interface ApiResponse<T> {
      data: T
      status: number
    }

    const parseId = (s: string): Result<number> => {
      const num = Number.parseInt(s, 10)
      return Number.isNaN(num)
        ? Err(createError({ code: 'PARSE_ERROR', message: 'Invalid ID', category: 'VALIDATION' }))
        : Ok(num)
    }

    const fetchUser = (id: number): Result<ApiResponse<{ name: string }>> => {
      return id > 0
        ? Ok({ data: { name: `User ${id}` }, status: 200 })
        : Err(createError({ code: 'NOT_FOUND', message: 'User not found', category: 'BUSINESS' }))
    }

    const extractName = (response: ApiResponse<{ name: string }>): Result<string> => {
      return response.status === 200
        ? Ok(response.data.name)
        : Err(
            createError({ code: 'API_ERROR', message: 'API returned error', category: 'NETWORK' })
          )
    }

    // Test successful chain
    const validIdResult = Ok('123')
    const result = flatMap(
      (name: string) => Ok(name.toUpperCase()),
      flatMap(extractName, flatMap(fetchUser, flatMap(parseId, validIdResult)))
    )

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe('USER 123')
    }

    // Test failure propagation
    const invalidIdResult = Ok('invalid')
    const failureResult = flatMap(
      (name: string) => Ok(name.toUpperCase()),
      flatMap(extractName, flatMap(fetchUser, flatMap(parseId, invalidIdResult)))
    )

    expect(failureResult.tag).toBe('failure')
  })

  test('Monad laws with async-style operations', () => {
    interface DatabaseRecord {
      id: number
      data: string
    }

    const validateId = (id: number): Result<number> =>
      id > 0 && id < 1000
        ? Ok(id)
        : Err(
            createError({ code: 'INVALID_ID', message: 'ID out of range', category: 'VALIDATION' })
          )

    const loadRecord = (id: number): Result<DatabaseRecord> =>
      Ok({ id, data: `Record data for ${id}` })

    const processData = (record: DatabaseRecord): Result<string> =>
      record.data.length > 0
        ? Ok(record.data.toUpperCase())
        : Err(
            createError({ code: 'EMPTY_DATA', message: 'No data to process', category: 'BUSINESS' })
          )

    // Chain operations
    const validInput = Ok(42)
    const result = flatMap(processData, flatMap(loadRecord, flatMap(validateId, validInput)))

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe('RECORD DATA FOR 42')
    }

    // Test with invalid input
    const invalidInput = Ok(1500)
    const invalidResult = flatMap(
      processData,
      flatMap(loadRecord, flatMap(validateId, invalidInput))
    )

    expect(invalidResult.tag).toBe('failure')
  })

  test('Monad composition preserves error context', () => {
    const errorWithContext = createError({
      code: 'CONTEXT_ERROR',
      message: 'Error with context',
      category: 'BUSINESS',
      context: { userId: 123, action: 'update', timestamp: '2023-01-01' },
    })

    const f = (_: number): Result<string> => Err(errorWithContext)
    const g = (_: string): Result<number> => Ok(42)

    const result = Ok(10)
    const composed = flatMap(g, flatMap(f, result))

    expect(composed.tag).toBe('failure')
    if (composed.tag === 'failure') {
      expect(composed.error).toEqual(errorWithContext)
      expect(composed.error.context).toEqual({
        userId: 123,
        action: 'update',
        timestamp: '2023-01-01',
      })
    }
  })
})
