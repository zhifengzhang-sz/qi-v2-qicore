/**
 * Property-based tests for Functor laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Functor laws
 * using fast-check property-based testing.
 */

import { createError } from '@/base/error.js'
import { Err, Ok, type Result, from } from '@/base/result.js'
import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'

describe('Functor Laws for Result<T>', () => {
  test.prop([fc.anything()])('Functor Identity Law: map(id) === id', (value) => {
    const identity = <T>(x: T): T => x
    const result = Ok(value)

    const mapped = from(result).map(identity).build()

    expect(mapped).toEqual(result)
  })

  test.prop([fc.anything()])('Functor Identity Law applies to failures', (errorMessage) => {
    const identity = <T>(x: T): T => x
    const error = createError({
      code: 'TEST_ERROR',
      message: JSON.stringify(errorMessage) ?? 'test-error',
      category: 'VALIDATION',
    })
    const result: Result<number> = Err(error)

    const mapped = from(result).map(identity).build()

    expect(mapped).toEqual(result)
  })

  test.prop([fc.integer()])('Functor Composition Law: map(f ∘ g) === map(f) ∘ map(g)', (value) => {
    const f = (x: number) => x * 2
    const g = (x: number) => x + 1
    const composed = (x: number) => f(g(x))

    const result = Ok(value)

    // Left side: map(f ∘ g)
    const leftSide = from(result).map(composed).build()

    // Right side: map(f) ∘ map(g)
    const rightSide = from(result).map(g).map(f).build()

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.string()])('Functor Composition Law with string operations', (value) => {
    const f = (s: string) => s.toUpperCase()
    const g = (s: string) => s.trim()
    const composed = (s: string) => f(g(s))

    const result = Ok(value)

    const leftSide = from(result).map(composed).build()
    const rightSide = from(result).map(g).map(f).build()

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.array(fc.integer())])('Functor Composition Law with array operations', (value) => {
    const f = (arr: number[]) => arr.map((x) => x * 2)
    const g = (arr: number[]) => arr.filter((x) => x > 0)
    const composed = (arr: number[]) => f(g(arr))

    const result = Ok(value)

    const leftSide = from(result).map(composed).build()
    const rightSide = from(result).map(g).map(f).build()

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([
    fc.record({
      name: fc.string(),
      age: fc.integer({ min: 0, max: 120 }),
    }),
  ])('Functor laws with object transformations', (user) => {
    const addTitle = (u: typeof user) => ({ ...u, title: 'Dr.' })
    const incrementAge = (u: typeof user) => ({ ...u, age: u.age + 1 })
    const composed = (u: typeof user) => addTitle(incrementAge(u))

    const result = Ok(user)

    const leftSide = from(result).map(composed).build()
    const rightSide = from(result).map(incrementAge).map(addTitle).build()

    expect(leftSide).toEqual(rightSide)
  })

  // Test that Functor laws hold for complex type transformations
  test.prop([fc.anything()])('Functor laws with type transformation', (value) => {
    const toStr = (x: unknown): string => JSON.stringify(x) ?? 'undefined'
    const getLength = (s: string): number => s.length
    const composed = (x: unknown): number => getLength(toStr(x))

    const result = Ok(value)

    const leftSide = from(result).map(composed).build()
    const rightSide = from(result).map(toStr).map(getLength).build()

    expect(leftSide).toEqual(rightSide)
  })

  // Edge case: empty transformations
  test('Functor laws with empty operations', () => {
    const emptyString = ''
    const emptyArray: number[] = []
    const emptyObject = {}

    const identity = <T>(x: T): T => x

    // Test with different empty values
    for (const emptyValue of [emptyString, emptyArray, emptyObject]) {
      const result = Ok(emptyValue)
      const mapped = from(result).map(identity).build()
      expect(mapped).toEqual(result)
    }
  })

  // Verify that mapping preserves failure structure
  test.prop([fc.string(), fc.string()])(
    'Mapping over failure preserves error structure',
    (errorCode, errorMessage) => {
      const error = createError({
        code: errorCode,
        message: errorMessage,
        category: 'SYSTEM',
      })
      const result: Result<number> = Err(error)

      const mapped = from(result)
        .map((x) => x * 2)
        .map((x) => x.toString())
        .build()

      expect(mapped).toEqual(result)
      expect(mapped.tag).toBe('failure')
      if (mapped.tag === 'failure') {
        expect(mapped.error).toEqual(error)
      }
    }
  )
})
