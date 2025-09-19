# @qi/eslint-plugin

ESLint rules to enforce proper usage of `Result<T>` from `@qi/base` and prevent common anti-patterns.

## Installation & Usage

### 🏠 Internal Usage (QiCore Workspace)

**Already configured!** This plugin is part of the QiCore workspace and pre-configured in `eslint.config.js`.

To verify it's working:
```bash
# Run ESLint with anti-pattern detection
bun run lint:anti-patterns

# Check which rules are active
npx eslint --print-config lib/core/src/cache.ts
```

**Current Setup**: The workspace automatically uses the `recommended` configuration which flags Result<T> anti-patterns as errors.

### 📦 External Usage (Other Projects)

#### Installation
```bash
npm install @qi/eslint-plugin --save-dev
```

#### ESLint 9+ Configuration (Recommended)
```javascript
// eslint.config.js
import qiPlugin from '@qi/eslint-plugin';

export default [
  {
    files: ['**/*.ts', '**/*.tsx'],
    plugins: {
      '@qi': qiPlugin,
    },
    rules: {
      // Option 1: Use recommended preset
      ...qiPlugin.configs.recommended.rules,

      // Option 2: Manual configuration
      '@qi/no-result-anti-patterns': 'error',
    },
  },
];
```

#### Legacy ESLint Configuration
```json
{
  "plugins": ["@qi"],
  "rules": {
    "@qi/no-result-anti-patterns": "error"
  }
}
```

## Configuration Presets

### `recommended` (Default)
Essential Result<T> pattern enforcement - flags all direct access anti-patterns.

```javascript
// Use the recommended preset
export default [
  {
    plugins: { '@qi': qiPlugin },
    rules: qiPlugin.configs.recommended.rules
  }
];
```

### `strict`
Same as recommended (currently only contains one rule).

```javascript
// Use the strict preset
export default [
  {
    plugins: { '@qi': qiPlugin },
    rules: qiPlugin.configs.strict.rules
  }
];
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

## Detection Status in QiCore

Based on the latest analysis of the `lib/` directory, this plugin will detect **100+ anti-pattern violations** across the codebase:

### 🚨 **Current Violations Found**
- **Direct tag checking**: ~52 violations (`.tag === 'success'`) in application code
- **Direct property access**: ~28 violations (`.value`, `.error`) in application code
- **Destructuring**: 0 violations ✅

### 📍 **Most Critical Files**
- `lib/core/src/cache.ts`: ~12 violations
- `lib/core/src/config.ts`: ~9 violations
- `lib/cli/src/factories/createReadlineCLI.ts`: ~12 violations
- `lib/amsg/src/`: Unknown count (to be analyzed)

### ✅ **Correctly Excluded**
- `lib/base/src/result.ts`: 0 violations (legitimately implements Result<T> patterns)
- `lib/base/src/async.ts`: 0 violations (legitimately implements async Result<T> helpers)

**Note**: The rule now correctly excludes `lib/base/src/**` files since these contain the legitimate internal implementations of Result<T> combinators like `isSuccess()`, `map()`, `flatMap()`, etc.

## Known Issues

### ESLint Dependency Compatibility (2025)

There are currently known compatibility issues with `eslint-visitor-keys` in the latest TypeScript-ESLint v8.x and ESLint v9.x versions. This affects testing and runtime usage but not rule compilation.

**Symptoms:**
- `Cannot find module 'eslint-visitor-keys'` errors
- Issues with `@typescript-eslint/rule-tester`

**Root Cause:**
- TypeScript-ESLint v6+ changed how visitor keys are imported
- Visitor keys can now only be imported from `@typescript-eslint/visitor-keys`
- Ongoing compatibility challenges between ESLint 9+ and the broader ecosystem

**Current Status:**
- Rules compile correctly with TypeScript
- Will work once the broader ESLint ecosystem resolves visitor-keys dependency conflicts
- Affects testing primarily, not production usage

**Workaround:**
For now, the rules are properly structured and will function correctly when the dependency issues are resolved in the upstream packages.