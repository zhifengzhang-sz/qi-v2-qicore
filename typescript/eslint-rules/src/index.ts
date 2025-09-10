/**
 * @qi/eslint-plugin - ESLint rules for QiCore patterns
 *
 * Enforces proper usage of Result<T> and prevents anti-patterns
 */

import { noResultAntiPatterns } from './no-result-anti-patterns.js'

const plugin = {
  meta: {
    name: '@qi/eslint-plugin',
    version: '1.0.0',
  },
  rules: {
    'no-result-anti-patterns': noResultAntiPatterns,
  },
  configs: {
    recommended: {
      plugins: ['@qi'],
      rules: {
        '@qi/no-result-anti-patterns': 'error',
      },
    },
    strict: {
      plugins: ['@qi'],
      rules: {
        '@qi/no-result-anti-patterns': 'error',
      },
    },
  },
}

export default plugin