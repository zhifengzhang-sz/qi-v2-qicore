/**
 * Property-based tests for Monad laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Monad laws:
 * - Left Identity: flatMap(f)(Ok(x)) === f(x)
 * - Right Identity: m.flatMap(Ok) === m
 * - Associativity: m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))
 */

import { createError, validationError } from '@/base/error.js'
import { Err, Ok, type Result, from } from '@/base/result.js'
import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'

describe('Monad Laws for Result<T>', () => {
  test.prop([fc.integer()])('Left Identity Law: flatMap(f)(Ok(x)) === f(x)', (value) => {
    const f = (x: number): Result<string> => Ok(x.toString())

    // Left side: flatMap(f)(Ok(x))
    const leftSide = from(Ok(value)).flatMap(f).build()

    // Right side: f(x)
    const rightSide = f(value)

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.string()])('Left Identity Law with string to number conversion', (value) => {
    const parseError = validationError('Cannot parse as number')
    const f = (s: string): Result<number> => {
      const parsed = Number.parseInt(s, 10)
      return Number.isNaN(parsed) ? Err(parseError) : Ok(parsed)
    }

    const leftSide = from(Ok(value)).flatMap(f).build()
    const rightSide = f(value)

    expect(leftSide).toEqual(rightSide)
  })

  test.prop([fc.anything()])('Right Identity Law: m.flatMap(Ok) === m', (value) => {
    const result = Ok(value)

    // m.flatMap(Ok) should equal m
    const flatMapped = from(result)
      .flatMap((x) => Ok(x))
      .build()

    expect(flatMapped).toEqual(result)
  })

  test.prop([fc.string()])('Right Identity Law applies to failures', (errorMessage) => {
    const error = createError({
      code: 'TEST_ERROR',
      message: errorMessage,
      category: 'VALIDATION',
    })
    const result: Result<number> = Err(error)

    const flatMapped = from(result)
      .flatMap((x) => Ok(x))
      .build()

    expect(flatMapped).toEqual(result)
  })

  test.prop([fc.integer()])(
    'Associativity Law: m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))',
    (value) => {
      const f = (x: number): Result<string> => Ok(x.toString())
      const g = (s: string): Result<number> => Ok(s.length)

      const result = Ok(value)

      // Left side: m.flatMap(f).flatMap(g)
      const leftSide = from(result).flatMap(f).flatMap(g).build()

      // Right side: m.flatMap(x => f(x).flatMap(g))
      const rightSide = from(result)
        .flatMap((x) => from(f(x)).flatMap(g).build())
        .build()

      expect(leftSide).toEqual(rightSide)
    }
  )

  test.prop([fc.array(fc.integer())])('Associativity Law with array operations', (values) => {
    const f = (arr: number[]): Result<string[]> => Ok(arr.map((x) => x.toString()))
    const g = (strArr: string[]): Result<number> => Ok(strArr.length)

    const result = Ok(values)

    const leftSide = from(result).flatMap(f).flatMap(g).build()

    const rightSide = from(result)
      .flatMap((x) => from(f(x)).flatMap(g).build())
      .build()

    expect(leftSide).toEqual(rightSide)
  })

  // Test with operations that can fail
  test.prop([fc.integer()])('Monad laws with fallible operations', (value) => {
    const divisionError = validationError('Division by zero')
    const negativeError = validationError('Square root of negative number')

    const safeDivide = (x: number): Result<number> => (x === 0 ? Err(divisionError) : Ok(10 / x))

    const safeSquareRoot = (x: number): Result<number> =>
      x < 0 ? Err(negativeError) : Ok(Math.sqrt(x))

    const result = Ok(value)

    // Test associativity with fallible operations
    const leftSide = from(result).flatMap(safeDivide).flatMap(safeSquareRoot).build()

    const rightSide = from(result)
      .flatMap((x) => from(safeDivide(x)).flatMap(safeSquareRoot).build())
      .build()

    expect(leftSide).toEqual(rightSide)
  })

  // Test early failure behavior
  test.prop([fc.string()])('Early failure in flatMap chain', (errorMessage) => {
    const error = createError({
      code: 'INITIAL_ERROR',
      message: errorMessage,
      category: 'SYSTEM',
    })
    const result: Result<number> = Err(error)

    const f = (x: number): Result<string> => Ok(x.toString())
    const g = (s: string): Result<number> => Ok(s.length)

    // Chain should short-circuit on initial failure
    const chained = from(result).flatMap(f).flatMap(g).build()

    expect(chained).toEqual(result)
    expect(chained.tag).toBe('failure')
  })

  // Test complex chaining scenarios
  test.prop([
    fc.record({
      name: fc.string(),
      age: fc.integer({ min: 0, max: 120 }),
    }),
  ])('Complex monad chains with object transformations', (user) => {
    const ageError = validationError('User must be 18 or older')
    const nameError = validationError('Name cannot be empty')

    const validateAge = (u: typeof user): Result<typeof user> =>
      u.age >= 18 ? Ok(u) : Err(ageError)

    const validateName = (u: typeof user): Result<typeof user> =>
      u.name.trim().length > 0 ? Ok(u) : Err(nameError)

    const addWelcomeMessage = (u: typeof user): Result<typeof user & { welcome: string }> =>
      Ok({ ...u, welcome: `Welcome, ${u.name}!` })

    const result = Ok(user)

    // Test that chaining preserves monad laws
    const processed = from(result)
      .flatMap(validateAge)
      .flatMap(validateName)
      .flatMap(addWelcomeMessage)
      .build()

    // Verify the result is consistent with step-by-step application
    const step1 = validateAge(user)
    if (step1.tag === 'failure') {
      expect(processed).toEqual(step1)
      return
    }

    const step2 = validateName(step1.value)
    if (step2.tag === 'failure') {
      expect(processed).toEqual(step2)
      return
    }

    const step3 = addWelcomeMessage(step2.value)
    expect(processed).toEqual(step3)
  })

  // Test type transformation through monad chain
  test.prop([fc.string()])('Type transformations preserve monad laws', (value) => {
    const parseNumber = (s: string): Result<number> => {
      const num = Number.parseFloat(s)
      return Number.isNaN(num) ? Err(validationError(`"${s}" is not a valid number`)) : Ok(num)
    }

    const formatCurrency = (n: number): Result<string> => Ok(`$${n.toFixed(2)}`)

    const result = Ok(value)

    // Left identity with type transformation
    const directApplication = from(parseNumber(value)).flatMap(formatCurrency).build()

    const monadicApplication = from(result).flatMap(parseNumber).flatMap(formatCurrency).build()

    expect(monadicApplication).toEqual(directApplication)
  })

  // Test that monad laws hold with nested Result operations
  test.prop([fc.integer({ min: 1, max: 100 })])(
    'Nested Result operations preserve laws',
    (depth) => {
      const nestResult = (x: number): Result<number> =>
        x > 0 ? Ok(x - 1) : Err(validationError('Reached zero'))

      // Build a chain of operations
      let result: Result<number> = Ok(depth)
      let expected: Result<number> = Ok(depth)

      for (let i = 0; i < 3; i++) {
        result = from(result).flatMap(nestResult).build()
        if (expected.tag === 'success') {
          expected = nestResult(expected.value)
        }
      }

      // Verify that repeated application matches expected result
      if (depth >= 3) {
        expect(result.tag).toBe('success')
        if (result.tag === 'success') {
          expect(result.value).toBe(depth - 3)
        }
      } else {
        expect(result.tag).toBe('failure')
      }
    }
  )
})
