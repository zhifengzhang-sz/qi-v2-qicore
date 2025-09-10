/**
 * Tests for no-result-anti-patterns ESLint rule
 */

import { RuleTester } from '@typescript-eslint/rule-tester'
import { noResultAntiPatterns } from './no-result-anti-patterns'

const ruleTester = new RuleTester({
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaVersion: 2022,
    sourceType: 'module',
    project: './tsconfig.json',
  },
})

ruleTester.run('no-result-anti-patterns', noResultAntiPatterns, {
  valid: [
    // ✅ Proper usage with match()
    {
      code: `
        import { match, Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          return match(
            (value) => console.log(value),
            (error) => console.error(error),
            result
          )
        }
      `,
    },
    
    // ✅ Proper usage with map()
    {
      code: `
        import { map, Result } from '@qi/base'
        function transformResult(result: Result<number, Error>) {
          return map(x => x * 2, result)
        }
      `,
    },
    
    // ✅ Proper usage with flatMap()
    {
      code: `
        import { flatMap, Result } from '@qi/base'
        function chainResult(result: Result<number, Error>) {
          return flatMap(x => success(x.toString()), result)
        }
      `,
    },
    
    // ✅ Proper usage with type guards
    {
      code: `
        import { isSuccess, isFailure, Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          if (isSuccess(result)) {
            console.log('Success!')
          } else if (isFailure(result)) {
            console.log('Failed!')
          }
        }
      `,
    },

    // ✅ Non-Result objects should be allowed
    {
      code: `
        const obj = { tag: 'success', value: 42 }
        if (obj.tag === 'success') {
          console.log(obj.value)
        }
      `,
    },
  ],

  invalid: [
    // ❌ Direct tag checking
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          if (result.tag === 'success') {
            console.log('Success!')
          }
        }
      `,
      errors: [
        {
          messageId: 'directTagCheck',
          line: 4,
        },
      ],
    },
    
    // ❌ Direct tag checking (flipped)
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          if ('failure' === result.tag) {
            console.log('Failed!')
          }
        }
      `,
      errors: [
        {
          messageId: 'directTagCheck',
          line: 4,
        },
      ],
    },

    // ❌ Direct value access
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          const val = result.value
          console.log(val)
        }
      `,
      errors: [
        {
          messageId: 'directPropertyAccess',
          data: { property: 'value' },
          line: 4,
        },
      ],
    },

    // ❌ Direct error access
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          const err = result.error
          console.error(err)
        }
      `,
      errors: [
        {
          messageId: 'directPropertyAccess',
          data: { property: 'error' },
          line: 4,
        },
      ],
    },

    // ❌ Destructuring Result
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          const { tag, value, error } = result
          if (tag === 'success') console.log(value)
        }
      `,
      errors: [
        {
          messageId: 'resultDestructuring',
          line: 4,
        },
      ],
    },

    // ❌ Partial destructuring (still bad)
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          const { value } = result
          console.log(value)
        }
      `,
      errors: [
        {
          messageId: 'resultDestructuring',
          line: 4,
        },
      ],
    },

    // ❌ Assignment destructuring
    {
      code: `
        import { Result } from '@qi/base'
        function handleResult(result: Result<string, Error>) {
          let tag, value;
          ({ tag, value } = result)
          if (tag === 'success') console.log(value)
        }
      `,
      errors: [
        {
          messageId: 'resultDestructuring',
          line: 5,
        },
      ],
    },

    // ❌ Mixed anti-patterns
    {
      code: `
        import { Result } from '@qi/base'
        function badExample(result: Result<string, Error>) {
          if (result.tag === 'success') {
            return result.value
          } else {
            throw result.error
          }
        }
      `,
      errors: [
        {
          messageId: 'directTagCheck',
          line: 4,
        },
        {
          messageId: 'directPropertyAccess',
          data: { property: 'value' },
          line: 5,
        },
        {
          messageId: 'directPropertyAccess',
          data: { property: 'error' },
          line: 7,
        },
      ],
    },
  ],
})

console.log('✅ All ESLint rule tests passed!')