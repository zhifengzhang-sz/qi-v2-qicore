/**
 * Property-based tests for Monad Laws
 * Contract: Result<T> must satisfy monad laws
 */

import { describe, it, expect } from 'vitest'
import fc from 'fast-check'
import { success, failure, flatMap, isSuccess, getValue, getError, create } from '@qi/base'

describe('Monad Laws for Result<T>', () => {
  it('Left Identity Law: flatMap(f, success(x)) === f(x)', () => {
    fc.assert(
      fc.property(fc.integer(), (x) => {
        const f = (n: number) => success(n * 2)

        const leftSide = flatMap(f, success(x))
        const rightSide = f(x)

        expect(isSuccess(leftSide)).toBe(true)
        expect(isSuccess(rightSide)).toBe(true)
        expect(getValue(leftSide)).toBe(getValue(rightSide))
      }),
      { numRuns: 1000 }
    )
  })

  it('Right Identity Law: flatMap(success, result) === result', () => {
    fc.assert(
      fc.property(
        fc.oneof(
          fc.integer().map(success),
          fc.string().map((msg) => failure(create('ERROR', msg, 'SYSTEM')))
        ),
        (result) => {
          const flattened = flatMap(success, result)

          if (isSuccess(result)) {
            expect(isSuccess(flattened)).toBe(true)
            expect(getValue(flattened)).toBe(getValue(result))
          } else {
            expect(isSuccess(flattened)).toBe(false)
            expect(getError(flattened)).toBe(getError(result))
          }
        }
      ),
      { numRuns: 1000 }
    )
  })

  it('Associativity Law: flatMap(g, flatMap(f, result)) === flatMap(x => flatMap(g, f(x)), result)', () => {
    fc.assert(
      fc.property(
        fc.oneof(
          fc.integer().map(success),
          fc.string().map((msg) => failure(create('ERROR', msg, 'SYSTEM')))
        ),
        (result) => {
          const f = (x: number) => success(x + 1)
          const g = (x: number) => success(x * 2)

          const leftSide = flatMap(g, flatMap(f, result))
          const rightSide = flatMap((x: number) => flatMap(g, f(x)), result)

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

  it('Monad failure propagation', () => {
    fc.assert(
      fc.property(fc.string(), (errorMsg) => {
        const error = create('ERROR', errorMsg, 'SYSTEM')
        const result = failure(error)
        const f = (x: number) => success(x * 2)

        const chained = flatMap(f, result)

        expect(isSuccess(chained)).toBe(false)
        expect(getError(chained)).toBe(error)
      }),
      { numRuns: 1000 }
    )
  })
})
