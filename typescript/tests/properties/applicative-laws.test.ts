/**
 * Property-based tests for Applicative laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Applicative laws:
 * - Identity: apply(Ok(id), v) === v
 * - Composition: apply(apply(apply(Ok(compose), u), v), w) === apply(u, apply(v, w))
 * - Homomorphism: apply(Ok(f), Ok(x)) === Ok(f(x))
 * - Interchange: apply(u, Ok(y)) === apply(Ok(f => f(y)), u)
 */

import { createError } from '@/base/error.js'
import { Err, Ok, type Result, from } from '@/base/result.js'
import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'

describe('Applicative Laws for Result<T>', () => {
  test.prop([fc.anything()])('Identity Law: apply(Ok(id), v) === v', (value) => {
    const identity = <T>(x: T): T => x
    const v = Ok(value)

    // apply(Ok(id), v) should equal v
    const applied = from(v).apply(Ok(identity)).build()

    expect(applied).toEqual(v)
  })

  test.prop([fc.string()])('Identity Law applies to failures', (errorMessage) => {
    const identity = <T>(x: T): T => x
    const error = createError({
      code: 'TEST_ERROR',
      message: errorMessage,
      category: 'VALIDATION',
    })
    const v: Result<number> = Err(error)

    const applied = from(v).apply(Ok(identity)).build()

    expect(applied).toEqual(v)
  })

  test.prop([fc.integer()])(
    'Composition Law: apply(apply(apply(Ok(compose), u), v), w) === apply(u, apply(v, w))',
    (value) => {
      // compose :: (b -> c) -> (a -> b) -> (a -> c)
      const compose =
        (f: (x: number) => number) =>
        (g: (x: number) => number) =>
        (x: number): number =>
          f(g(x))

      const u = Ok((x: number) => x * 2)
      const v = Ok((x: number) => x + 1)
      const w = Ok(value)

      // Left side: apply(apply(apply(Ok(compose), u), v), w)
      const leftSide = from(w)
        .apply(
          from(v)
            .apply(from(u).apply(Ok(compose)).build())
            .build()
        )
        .build()

      // Right side: apply(u, apply(v, w))
      const rightSide = from(from(w).apply(v).build()).apply(u).build()

      expect(leftSide).toEqual(rightSide)
    }
  )

  test.prop([fc.integer()])('Homomorphism Law: apply(Ok(f), Ok(x)) === Ok(f(x))', (value) => {
    const f = (x: number): string => x.toString()

    // Left side: apply(Ok(f), Ok(x))
    const leftSide = from(Ok(value)).apply(Ok(f)).build()

    // Right side: Ok(f(x))
    const rightSide = Ok(f(value))

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.string()])('Homomorphism Law with string operations', (value) => {
    const f = (s: string): number => s.length

    const leftSide = from(Ok(value)).apply(Ok(f)).build()
    const rightSide = Ok(f(value))

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.array(fc.integer())])(
    'Interchange Law: apply(u, Ok(y)) === apply(Ok(f => f(y)), u)',
    (value) => {
      const u = Ok((arr: number[]) => arr.length)
      const y = value

      // Left side: apply(u, Ok(y))
      const leftSide = from(Ok(y)).apply(u).build()

      // Right side: apply(Ok(f => f(y)), u)
      const rightSide = from(u)
        .apply(Ok((f: (arr: number[]) => number) => f(y)))
        .build()

      expect(leftSide).toEqual(rightSide)
    }
  )

  // Test with multiple argument functions using currying
  test.prop([fc.integer(), fc.integer()])('Applicative with curried binary function', (x, y) => {
    const add = (a: number) => (b: number) => a + b

    const resultX = Ok(x)
    const resultY = Ok(y)

    // Apply curried function step by step
    const applied = from(resultY)
      .apply(from(resultX).apply(Ok(add)).build())
      .build()

    const expected = Ok(x + y)

    expect(applied).toEqual(expected)
  })

  test.prop([fc.integer(), fc.integer(), fc.integer()])(
    'Applicative with curried ternary function',
    (x, y, z) => {
      const add3 = (a: number) => (b: number) => (c: number) => a + b + c

      const resultX = Ok(x)
      const resultY = Ok(y)
      const resultZ = Ok(z)

      // Chain applicative applications
      const applied = from(resultZ)
        .apply(
          from(resultY)
            .apply(from(resultX).apply(Ok(add3)).build())
            .build()
        )
        .build()

      const expected = Ok(x + y + z)

      expect(applied).toEqual(expected)
    }
  )

  // Test failure propagation in applicative chains
  test.prop([fc.string(), fc.integer()])(
    'Failure propagation in applicative chains',
    (errorMessage, value) => {
      const error = createError({
        code: 'TEST_ERROR',
        message: errorMessage,
        category: 'SYSTEM',
      })

      const failedResult: Result<number> = Err(error)
      const successResult = Ok(value)
      const add = (a: number) => (b: number) => a + b

      // Test that failure propagates from first argument
      const firstFails = from(successResult)
        .apply(from(failedResult).apply(Ok(add)).build())
        .build()

      expect(firstFails.tag).toBe('failure')
      if (firstFails.tag === 'failure') {
        expect(firstFails.error).toEqual(error)
      }

      // Test that failure propagates from second argument
      const secondFails = from(failedResult)
        .apply(from(successResult).apply(Ok(add)).build())
        .build()

      expect(secondFails.tag).toBe('failure')
      if (secondFails.tag === 'failure') {
        expect(secondFails.error).toEqual(error)
      }
    }
  )

  // Test with complex object transformations
  test.prop([
    fc.record({
      name: fc.string(),
      age: fc.integer({ min: 0, max: 120 }),
    }),
  ])('Applicative with object transformations', (user) => {
    const createUser = (name: string) => (age: number) => (id: string) => ({
      id,
      name,
      age,
      createdAt: new Date(),
    })

    const nameResult = Ok(user.name)
    const ageResult = Ok(user.age)
    const idResult = Ok('user-123')

    // Apply function to build complex object
    const result = from(idResult)
      .apply(
        from(ageResult)
          .apply(from(nameResult).apply(Ok(createUser)).build())
          .build()
      )
      .build()

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value.name).toBe(user.name)
      expect(result.value.age).toBe(user.age)
      expect(result.value.id).toBe('user-123')
      expect(result.value.createdAt).toBeInstanceOf(Date)
    }
  })

  // Test associativity of applicative operations
  test.prop([fc.integer(), fc.integer(), fc.integer()])(
    'Applicative associativity with multiple operations',
    (a, b, c) => {
      const multiply = (x: number) => (y: number) => x * y
      const add = (x: number) => (y: number) => x + y

      const resultA = Ok(a)
      const resultB = Ok(b)
      const resultC = Ok(c)

      // (a * b) + c
      const leftAssoc = from(resultC)
        .apply(
          from(
            from(resultB)
              .apply(from(resultA).apply(Ok(multiply)).build())
              .build()
          )
            .apply(Ok(add))
            .build()
        )
        .build()

      // a * (b + c) - different operation but we test structure consistency
      const rightAssoc = from(
        from(resultC)
          .apply(from(resultB).apply(Ok(add)).build())
          .build()
      )
        .apply(from(resultA).apply(Ok(multiply)).build())
        .build()

      // Both should be successful results
      expect(leftAssoc.tag).toBe('success')
      expect(rightAssoc.tag).toBe('success')

      if (leftAssoc.tag === 'success' && rightAssoc.tag === 'success') {
        expect(leftAssoc.value).toBe(a * b + c)
        expect(rightAssoc.value).toBe(a * (b + c))
      }
    }
  )

  // Test with type transformations
  test.prop([fc.string(), fc.integer()])('Applicative with type transformations', (str, num) => {
    const combine = (s: string) => (n: number) => `${s}-${n}`

    const strResult = Ok(str)
    const numResult = Ok(num)

    const combined = from(numResult)
      .apply(from(strResult).apply(Ok(combine)).build())
      .build()

    expect(combined).toEqual(Ok(`${str}-${num}`))
  })

  // Edge case: empty values
  test('Applicative laws with empty values', () => {
    const combine = (s: string) => (arr: number[]) => ({ text: s, numbers: arr })

    const emptyString = Ok('')
    const emptyArray = Ok([] as number[])

    const result = from(emptyArray)
      .apply(from(emptyString).apply(Ok(combine)).build())
      .build()

    expect(result).toEqual(Ok({ text: '', numbers: [] }))
  })
})
