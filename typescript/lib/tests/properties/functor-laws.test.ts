/**
 * Property-based tests for Functor Laws
 * Contract: Result<T> must satisfy functor laws
 */

import { describe, it, expect } from 'vitest'
import fc from 'fast-check'
import { success, failure, map, isSuccess, getValue, getError, create } from '@qi/base'

const identity = <T>(x: T): T => x
const compose =
  <A, B, C>(f: (b: B) => C, g: (a: A) => B) =>
  (a: A) =>
    f(g(a))

describe('Functor Laws for Result<T>', () => {
  it('Functor Identity Law: map(id, result) === result', () => {
    fc.assert(
      fc.property(
        fc.oneof(
          fc.integer().map(success),
          fc.string().map((msg) => failure(create('ERROR', msg, 'SYSTEM')))
        ),
        (result) => {
          const mapped = map(identity, result)

          if (isSuccess(result)) {
            expect(isSuccess(mapped)).toBe(true)
            expect(getValue(mapped)).toBe(getValue(result))
          } else {
            expect(isSuccess(mapped)).toBe(false)
            expect(getError(mapped)).toBe(getError(result))
          }
        }
      ),
      { numRuns: 1000 }
    )
  })

  it('Functor Composition Law: map(f, map(g, result)) === map(compose(f, g), result)', () => {
    fc.assert(
      fc.property(
        fc.oneof(
          fc.integer().map(success),
          fc.string().map((msg) => failure(create('ERROR', msg, 'SYSTEM')))
        ),
        (result) => {
          const f = (x: number) => x * 2
          const g = (x: number) => x + 1

          const leftSide = map(f, map(g, result))
          const rightSide = map(compose(f, g), result)

          if (isSuccess(result)) {
            expect(isSuccess(leftSide)).toBe(true)
            expect(isSuccess(rightSide)).toBe(true)
            expect(getValue(leftSide)).toBe(getValue(rightSide))
          } else {
            expect(isSuccess(leftSide)).toBe(false)
            expect(isSuccess(rightSide)).toBe(false)
            expect(getError(leftSide)).toBe(getError(rightSide))
          }
        }
      ),
      { numRuns: 1000 }
    )
  })

  it('Functor preserves structure for different types', () => {
    fc.assert(
      fc.property(fc.string(), (str) => {
        const result = success(str)
        const mapped = map((s: string) => s.length, result)

        expect(isSuccess(mapped)).toBe(true)
        expect(getValue(mapped)).toBe(str.length)
      }),
      { numRuns: 1000 }
    )
  })
})
