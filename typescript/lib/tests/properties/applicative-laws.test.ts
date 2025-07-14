/**
 * Property-based tests for Applicative laws
 *
 * Verifies that Result<T> implementation satisfies mathematical Applicative laws
 * using pure functions.
 */

import { createError } from '@qi/base'
import { Err, Ok, type Result, apply, pure } from '@qi/base'
import { describe, expect, test } from 'vitest'

describe('Applicative Laws for Result<T>', () => {
  test('Identity Law: apply(pure(id), v) === v', () => {
    const identity = <T>(x: T): T => x

    // Test with explicit types to avoid TypeScript inference issues
    const numberResult = Ok(42)
    const stringResult = Ok('hello')
    const arrayResult = Ok([1, 2, 3])
    const objectResult = Ok({ name: 'test' })

    const appliedNumber = apply(pure(identity<number>), numberResult)
    const appliedString = apply(pure(identity<string>), stringResult)
    const appliedArray = apply(pure(identity<number[]>), arrayResult)
    const appliedObject = apply(pure(identity<{ name: string }>), objectResult)

    expect(appliedNumber).toEqual(numberResult)
    expect(appliedString).toEqual(stringResult)
    expect(appliedArray).toEqual(arrayResult)
    expect(appliedObject).toEqual(objectResult)
  })

  test('Identity Law applies to failures', () => {
    const identity = <T>(x: T): T => x
    const error = createError({
      code: 'TEST_ERROR',
      message: 'test-error',
      category: 'VALIDATION',
    })
    const v: Result<number> = Err(error)

    const applied = apply(pure(identity<number>), v)

    expect(applied.tag).toBe(v.tag)
    if (applied.tag === 'failure' && v.tag === 'failure') {
      expect(applied.error).toEqual(v.error)
    }
  })

  test('Composition Law: apply(apply(apply(pure(compose), u), v), w) === apply(u, apply(v, w))', () => {
    const compose =
      <A, B, C>(f: (b: B) => C) =>
      (g: (a: A) => B) =>
      (x: A) =>
        f(g(x))
    const f = (x: number) => x * 2
    const g = (x: number) => x + 1
    const value = 10

    const u = pure(f)
    const v = pure(g)
    const w = Ok(value)

    // Left side: apply(apply(apply(pure(compose), u), v), w)
    const leftSide = apply(apply(apply(pure(compose<number, number, number>), u), v), w)

    // Right side: apply(u, apply(v, w))
    const rightSide = apply(u, apply(v, w))

    expect(leftSide.tag).toBe(rightSide.tag)
    if (leftSide.tag === 'success' && rightSide.tag === 'success') {
      expect(leftSide.value).toBe(rightSide.value)
    }
  })

  test('Homomorphism Law: apply(pure(f), pure(x)) === pure(f(x))', () => {
    const f = (x: number) => x * 2
    const testValues = [0, 42, -1, 100]

    for (const x of testValues) {
      const leftSide = apply(pure(f), pure(x))
      const rightSide = pure(f(x))

      expect(leftSide.tag).toBe(rightSide.tag)
      if (leftSide.tag === 'success' && rightSide.tag === 'success') {
        expect(leftSide.value).toBe(rightSide.value)
      }
    }
  })

  test('Interchange Law: apply(u, pure(y)) === apply(pure(f => f(y)), u)', () => {
    const f = (x: number) => x * 2
    const y = 42

    const u = pure(f)

    // Left side: apply(u, pure(y))
    const leftSide = apply(u, pure(y))

    // Right side: apply(pure(f => f(y)), u)
    const rightSide = apply(
      pure((fn: typeof f) => fn(y)),
      u
    )

    expect(leftSide.tag).toBe(rightSide.tag)
    if (leftSide.tag === 'success' && rightSide.tag === 'success') {
      expect(leftSide.value).toBe(rightSide.value)
    }
  })

  test('Applicative preserves failure propagation', () => {
    const error1 = createError({
      code: 'ERROR_1',
      message: 'First error',
      category: 'VALIDATION',
    })

    const error2 = createError({
      code: 'ERROR_2',
      message: 'Second error',
      category: 'BUSINESS',
    })

    const f = (x: number) => (y: number) => x + y

    // Function failure
    const fnFailure = Err(error1)
    const valueSuccess = Ok(42)
    const result1 = apply(fnFailure, valueSuccess)

    expect(result1.tag).toBe('failure')
    if (result1.tag === 'failure') {
      expect(result1.error).toEqual(error1)
    }

    // Value failure
    const fnSuccess = Ok(f(10))
    const valueFailure = Err(error2)
    const result2 = apply(fnSuccess, valueFailure)

    expect(result2.tag).toBe('failure')
    if (result2.tag === 'failure') {
      expect(result2.error).toEqual(error2)
    }

    // Both failure (function failure takes precedence)
    const result3 = apply(fnFailure, valueFailure)

    expect(result3.tag).toBe('failure')
    if (result3.tag === 'failure') {
      expect(result3.error).toEqual(error1)
    }
  })

  test('Applicative with complex types', () => {
    interface User {
      name: string
      age: number
    }

    interface Address {
      street: string
      city: string
    }

    interface UserWithAddress {
      user: User
      address: Address
    }

    const createUserWithAddress =
      (user: User) =>
      (address: Address): UserWithAddress => ({
        user,
        address,
      })

    const user = { name: 'Alice', age: 30 }
    const address = { street: '123 Main St', city: 'Springfield' }

    const userResult = Ok(user)
    const addressResult = Ok(address)

    const result = apply(apply(pure(createUserWithAddress), userResult), addressResult)

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value.user).toEqual(user)
      expect(result.value.address).toEqual(address)
    }
  })

  test('Applicative with curried functions', () => {
    const add = (a: number) => (b: number) => (c: number) => a + b + c

    const aResult = Ok(10)
    const bResult = Ok(20)
    const cResult = Ok(30)

    // Build up the computation step by step
    const step1 = apply(pure(add), aResult)
    const step2 = apply(step1, bResult)
    const result = apply(step2, cResult)

    expect(result.tag).toBe('success')
    if (result.tag === 'success') {
      expect(result.value).toBe(60)
    }
  })

  test('Applicative validation pattern', () => {
    interface UserInput {
      name: string
      email: string
      age: number
    }

    const validateName = (name: string): Result<string> =>
      name.length >= 2
        ? Ok(name)
        : Err(
            createError({ code: 'INVALID_NAME', message: 'Name too short', category: 'VALIDATION' })
          )

    const validateEmail = (email: string): Result<string> =>
      email.includes('@')
        ? Ok(email)
        : Err(
            createError({ code: 'INVALID_EMAIL', message: 'Invalid email', category: 'VALIDATION' })
          )

    const validateAge = (age: number): Result<number> =>
      age >= 0 && age <= 150
        ? Ok(age)
        : Err(createError({ code: 'INVALID_AGE', message: 'Invalid age', category: 'VALIDATION' }))

    const createUser =
      (name: string) =>
      (email: string) =>
      (age: number): UserInput => ({
        name,
        email,
        age,
      })

    // Valid input
    const validName = validateName('Alice')
    const validEmail = validateEmail('alice@example.com')
    const validAge = validateAge(30)

    const validResult = apply(apply(apply(pure(createUser), validName), validEmail), validAge)

    expect(validResult.tag).toBe('success')
    if (validResult.tag === 'success') {
      expect(validResult.value.name).toBe('Alice')
      expect(validResult.value.email).toBe('alice@example.com')
      expect(validResult.value.age).toBe(30)
    }

    // Invalid input (fails fast on first error)
    const invalidName = validateName('A')
    const invalidEmail = validateEmail('invalid-email')
    const invalidAge = validateAge(-5)

    const invalidResult = apply(
      apply(apply(pure(createUser), invalidName), invalidEmail),
      invalidAge
    )

    expect(invalidResult.tag).toBe('failure')
  })

  test('Applicative maintains type safety', () => {
    const concat = (a: string) => (b: string) => a + b
    const multiply = (a: number) => (b: number) => a * b

    // String concatenation
    const stringResult = apply(apply(pure(concat), Ok('Hello, ')), Ok('World!'))
    expect(stringResult.tag).toBe('success')
    if (stringResult.tag === 'success') {
      expect(stringResult.value).toBe('Hello, World!')
      expect(typeof stringResult.value).toBe('string')
    }

    // Number multiplication
    const numberResult = apply(apply(pure(multiply), Ok(6)), Ok(7))
    expect(numberResult.tag).toBe('success')
    if (numberResult.tag === 'success') {
      expect(numberResult.value).toBe(42)
      expect(typeof numberResult.value).toBe('number')
    }
  })

  test('Applicative error context preservation', () => {
    const errorWithContext = createError({
      code: 'COMPLEX_ERROR',
      message: 'Error with context',
      category: 'BUSINESS',
      context: { operation: 'user_creation', timestamp: '2023-01-01' },
    })

    const _add = (a: number) => (b: number) => a + b

    const errorResult: Result<(b: number) => number> = Err(errorWithContext)
    const valueResult = Ok(42)

    const result = apply(errorResult, valueResult)

    expect(result.tag).toBe('failure')
    if (result.tag === 'failure') {
      expect(result.error).toEqual(errorWithContext)
      expect(result.error.context).toEqual({
        operation: 'user_creation',
        timestamp: '2023-01-01',
      })
    }
  })
})
