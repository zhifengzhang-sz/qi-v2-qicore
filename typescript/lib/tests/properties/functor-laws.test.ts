/**
 * Property-based tests for Functor laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Functor laws
 * using pure functions.
 */

import { createError } from '@qi/base'
import { Err, Ok, type Result, map } from '@qi/base'
import { describe, expect, test } from 'vitest'

describe('Functor Laws for Result<T>', () => {
  test('Functor Identity Law: map(id, result) === result', () => {
    const identity = <T>(x: T): T => x

    // Test with various values
    const testValues = [42, 'hello', [1, 2, 3], { name: 'test' }, null, undefined]

    for (const value of testValues) {
      const result = Ok(value)
      const mapped = map(identity, result)

      // Compare structure and values, not function references
      expect(mapped.tag).toBe(result.tag)
      if (mapped.tag === 'success' && result.tag === 'success') {
        expect(mapped.value).toEqual(result.value)
      }
    }
  })

  test('Functor Identity Law applies to failures', () => {
    const identity = <T>(x: T): T => x
    const error = createError({
      code: 'TEST_ERROR',
      message: 'test-error',
      category: 'VALIDATION',
    })
    const result: Result<number> = Err(error)

    const mapped = map(identity, result)

    // Compare structure and values
    expect(mapped.tag).toBe(result.tag)
    if (mapped.tag === 'failure' && result.tag === 'failure') {
      expect(mapped.error).toEqual(result.error)
    }
  })

  test('Functor Composition Law: map(f, map(g, result)) === map(compose(f, g), result)', () => {
    const f = (x: number) => x * 2
    const g = (x: number) => x + 1
    const compose =
      <A, B, C>(f: (b: B) => C, g: (a: A) => B) =>
      (x: A) =>
        f(g(x))

    const testValues = [0, 42, -1, 100]

    for (const value of testValues) {
      const result = Ok(value)

      // Left side: map(f) ∘ map(g)
      const leftSide = map(f, map(g, result))

      // Right side: map(f ∘ g)
      const rightSide = map(compose(f, g), result)

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toBe(rightSide.value)
      }
    }
  })

  test('Functor Composition Law applies to failures', () => {
    const f = (x: number) => x * 2
    const g = (x: number) => x + 1
    const compose =
      <A, B, C>(f: (b: B) => C, g: (a: A) => B) =>
      (x: A) =>
        f(g(x))

    const error = createError({
      code: 'TEST_ERROR',
      message: 'test-error',
      category: 'VALIDATION',
    })
    const result: Result<number> = Err(error)

    // Both sides should preserve the failure
    const leftSide = map(f, map(g, result))
    const rightSide = map(compose(f, g), result)

    expect(leftSide.tag).toBe('failure')
    expect(rightSide.tag).toBe('failure')

    if (leftSide.tag === 'failure' && rightSide.tag === 'failure') {
      expect(leftSide.error).toEqual(rightSide.error)
    }
  })

  test('Functor law: map preserves structure for different types', () => {
    const numberToString = (x: number) => x.toString()

    // Success case
    const successResult = Ok(42)
    const mappedSuccess = map(numberToString, successResult)

    expect(mappedSuccess.tag).toBe('success')
    if (mappedSuccess.tag === 'success') {
      expect(mappedSuccess.value).toBe('42')
      expect(typeof mappedSuccess.value).toBe('string')
    }

    // Failure case
    const error = createError({
      code: 'TEST_ERROR',
      message: 'test-error',
      category: 'VALIDATION',
    })
    const failureResult: Result<number> = Err(error)
    const mappedFailure = map(numberToString, failureResult)

    expect(mappedFailure.tag).toBe('failure')
    if (mappedFailure.tag === 'failure') {
      expect(mappedFailure.error).toEqual(error)
    }
  })

  test('Functor map maintains type safety', () => {
    // Test type transformations work correctly
    const result = Ok(42)

    // number -> string
    const stringResult = map((x: number) => x.toString(), result)
    expect(stringResult.tag).toBe('success')
    if (stringResult.tag === 'success') {
      expect(typeof stringResult.value).toBe('string')
    }

    // number -> boolean
    const boolResult = map((x: number) => x > 0, result)
    expect(boolResult.tag).toBe('success')
    if (boolResult.tag === 'success') {
      expect(typeof boolResult.value).toBe('boolean')
      expect(boolResult.value).toBe(true)
    }

    // number -> object
    const objResult = map((x: number) => ({ value: x }), result)
    expect(objResult.tag).toBe('success')
    if (objResult.tag === 'success') {
      expect(typeof objResult.value).toBe('object')
      expect(objResult.value).toEqual({ value: 42 })
    }
  })

  test('Functor laws with complex transformations', () => {
    interface User {
      name: string
      age: number
    }

    const user: User = { name: 'Alice', age: 30 }
    const result = Ok(user)

    const getName = (u: User) => u.name
    const toUpperCase = (s: string) => s.toUpperCase()
    const getLength = (s: string) => s.length

    // Test composition: getLength ∘ toUpperCase ∘ getName
    const step1 = map(getName, result)
    const step2 = map(toUpperCase, step1)
    const step3 = map(getLength, step2)

    // Compare with direct composition
    const compose =
      <A, B, C, D>(f: (c: C) => D, g: (b: B) => C, h: (a: A) => B) =>
      (x: A) =>
        f(g(h(x)))

    const directResult = map(compose(getLength, toUpperCase, getName), result)

    expect(step3.tag).toBe(directResult.tag)
    if (step3.tag === 'success' && directResult.tag === 'success') {
      expect(step3.value).toBe(directResult.value)
      expect(step3.value).toBe(5) // 'ALICE'.length
    }
  })

  test('Functor preserves error context through transformations', () => {
    const error = createError({
      code: 'COMPLEX_ERROR',
      message: 'A complex error occurred',
      category: 'BUSINESS',
      context: { userId: 123, operation: 'update' },
    })

    const result: Result<{ data: string }> = Err(error)

    // Chain multiple transformations
    const step1 = map((x: { data: string }) => x.data, result)
    const step2 = map((s: string) => s.toUpperCase(), step1)
    const step3 = map((s: string) => s.split(''), step2)

    expect(step3.tag).toBe('failure')
    if (step3.tag === 'failure') {
      expect(step3.error).toEqual(error)
      expect(step3.error.context).toEqual({ userId: 123, operation: 'update' })
    }
  })
})
