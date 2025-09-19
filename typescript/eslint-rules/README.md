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
bunx eslint --print-config lib/core/src/cache.ts
```

**Current Setup**: The workspace automatically uses the `recommended` configuration which flags Result<T> anti-patterns as errors.

### 📦 External Usage (Other Projects)

#### Quick Start

1. **Install the plugin:**
```bash
npm install @qi/eslint-plugin --save-dev
```

2. **Add to your ESLint config:**
```javascript
// eslint.config.js
import qiPlugin from '@qi/eslint-plugin';

export default [
  {
    files: ['**/*.ts', '**/*.tsx'],
    plugins: { '@qi': qiPlugin },
    rules: qiPlugin.configs.recommended.rules
  }
];
```

3. **Start linting:**
```bash
npx eslint .
```

That's it! The rule will now catch Result<T> anti-patterns in your codebase.

#### Advanced Configuration
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

#### Configuration Options

```javascript
'@qi/no-result-anti-patterns': ['error', {
  excludePatterns: ['/path/to/exclude/', '/another/pattern/']
}]
```

**Options:**
- `excludePatterns` (optional): Array of path patterns to exclude from anti-pattern checking. Useful for legitimate Result<T> implementation files.

**Example with exclusions:**
```javascript
// eslint.config.js
export default [
  {
    files: ['**/*.ts', '**/*.tsx'],
    plugins: { '@qi': qiPlugin },
    rules: {
      '@qi/no-result-anti-patterns': ['error', {
        excludePatterns: ['/lib/base/src/', '/internal/result/']
      }]
    }
  }
];
```

#### ❌ Invalid (will be flagged)

```typescript
import { Result } from '@qi/base'

function bad(result: Result<string, Error>) {
  // Direct tag checking (binary expressions)
  if (result.tag === 'success') {
    return result.value  // Direct property access
  }

  // Switch statements on tag
  switch (result.tag) {
    case 'success': return result.value;
    case 'failure': return result.error;
  }

  // Ternary operators with tag
  const val = result.tag === 'success' ? result.value : null;

  // Logical operators with tag
  const isOk = result.tag === 'success' && result.value.length > 0;

  // Destructuring
  const { tag, value, error } = result

  // Direct property access
  const directVal = result.value;
  const directErr = result.error;
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

Based on the latest analysis with enhanced detection, this plugin will detect **200+ anti-pattern violations** across the codebase:

### 🚨 **Current Violations Found**
- **Direct tag checking**: ~150+ violations (binary expressions, ternary, logical operators)
- **Direct property access**: ~50+ violations (`.value`, `.error`)
- **Switch statements**: ~5+ violations (`switch (result.tag)`)
- **Destructuring**: 0 violations ✅

### 📍 **Most Critical Files**
- `lib/core/src/cache.ts`: ~12 violations
- `lib/core/src/config.ts`: ~9 violations
- `lib/cli/src/factories/createReadlineCLI.ts`: ~12 violations
- `lib/amsg/src/`: Unknown count (to be analyzed)

### ✅ **Correctly Excluded**
- `lib/base/src/result.ts`: 0 violations (legitimately implements Result<T> patterns)
- `lib/base/src/async.ts`: 0 violations (legitimately implements async Result<T> helpers)

**Note**: This workspace configures the rule with `excludePatterns: ["/lib/base/src/", "/lib/tests/base/"]` to exclude legitimate internal implementations of Result<T> combinators like `isSuccess()`, `map()`, `flatMap()`, etc.

## Anti-Pattern Detection Coverage

This rule detects the following Result<T> anti-patterns:

### Direct Tag Checking
- **Binary expressions**: `result.tag === 'success'`, `result.tag !== 'failure'`
- **Switch statements**: `switch (result.tag) { case 'success': ... }`
- **Ternary operators**: `result.tag === 'success' ? value : null`
- **Logical operators**: `result.tag === 'success' && doSomething()`

### Direct Property Access
- **Value access**: `result.value`, `result.error`
- **Destructuring**: `const { tag, value, error } = result`

### Why These Are Anti-Patterns

1. **Type safety**: Accessing `result.value` on a failure case causes runtime errors
2. **Error handling**: Missing exhaustive case handling leads to bugs
3. **API consistency**: Bypasses the intended functional programming interface
4. **Maintainability**: Hard to refactor and test compared to `match()` patterns

Use the provided combinators (`match`, `map`, `flatMap`, `isSuccess`, `isFailure`) instead for safer, more maintainable code.