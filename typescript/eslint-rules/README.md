# @qi/eslint-plugin

ESLint rules to enforce proper usage of `Result<T>` from `@qi/base` and prevent common anti-patterns.

## Installation

```bash
bun add --dev @qi/eslint-plugin
```

## Configuration

### ESLint 9+ (Flat Config)

```javascript
// eslint.config.js
import qiPlugin from '@qi/eslint-plugin'
import tseslint from 'typescript-eslint'

export default [
  ...tseslint.configs.recommended,
  {
    plugins: {
      '@qi': qiPlugin
    },
    rules: {
      '@qi/no-result-anti-patterns': 'error'
    }
  }
]
```

### Legacy ESLint Config

```json
{
  "plugins": ["@qi"],
  "rules": {
    "@qi/no-result-anti-patterns": "error"
  }
}
```

## Rules

### `@qi/no-result-anti-patterns`

Prevents anti-patterns when using `Result<T>` from `@qi/base`.

#### ❌ Invalid (will be flagged)

```typescript
import { Result } from '@qi/base'

function bad(result: Result<string, Error>) {
  // Direct tag checking
  if (result.tag === 'success') {
    return result.value  // Direct property access
  }
  
  // Destructuring
  const { tag, value, error } = result
  
  // Direct property access
  const val = result.value
  const err = result.error
}
```

#### ✅ Valid (recommended patterns)

```typescript
import { Result, match, map, flatMap, isSuccess, isFailure } from '@qi/base'

function good(result: Result<string, Error>) {
  // Use match for exhaustive handling
  return match(
    (value) => `Success: ${value}`,
    (error) => `Error: ${error.message}`,
    result
  )
  
  // Use map for transformations
  const transformed = map(s => s.toUpperCase(), result)
  
  // Use flatMap for chaining
  const chained = flatMap(s => success(s.length), result)
  
  // Use type guards when needed
  if (isSuccess(result)) {
    console.log('Operation succeeded')
  }
  
  if (isFailure(result)) {
    console.log('Operation failed')
  }
}
```

## Why These Rules?

`Result<T>` is designed to make error handling explicit and safe. The anti-patterns this rule prevents can lead to:

1. **Runtime errors**: Accessing `result.value` on a failure case
2. **Missed error handling**: Not handling both success and failure cases
3. **Type safety violations**: Bypassing the intended API

By enforcing proper usage patterns, this rule helps you:

- Write safer, more predictable code
- Catch errors at compile time instead of runtime  
- Follow functional programming best practices
- Maintain consistent error handling across your codebase

## Integration with QiCore

This plugin is designed specifically for projects using `@qi/base`. It understands the `Result<T>` type contracts and enforces the proper usage patterns documented in the QiCore Foundation.

For more information about `Result<T>` patterns, see the [QiCore Base tutorial](../docs/tutorial/nb/01-qi-base.ipynb).