# Biome GritQL Plugins for QiCore Result<T> Patterns

This directory contains GritQL plugins for Biome to enforce proper `Result<T>` usage patterns from `@qi/base`.

## Prerequisites

- Biome v2.0 or later (for plugin support)
- Currently compatible plugins for future use

## Plugins

### 1. `no-result-tag-check.grit`
Prevents direct tag checking on Result objects.

**❌ Invalid:**
```typescript
if (result.tag === 'success') { ... }
if (result.tag === "failure") { ... }
```

**✅ Valid:**
```typescript
import { isSuccess, isFailure, match } from '@qi/base'

if (isSuccess(result)) { ... }
if (isFailure(result)) { ... }

match(
  (value) => console.log('Success:', value),
  (error) => console.log('Error:', error),
  result
)
```

### 2. `no-result-property-access.grit`
Prevents direct property access on Result objects.

**❌ Invalid:**
```typescript
const value = result.value
const error = result.error
```

**✅ Valid:**
```typescript
import { map, match } from '@qi/base'

const transformed = map(value => value.toUpperCase(), result)

match(
  (value) => console.log(value),
  (error) => console.error(error),
  result
)
```

### 3. `no-result-destructuring.grit`
Prevents destructuring Result objects.

**❌ Invalid:**
```typescript
const { tag, value, error } = result
const { tag } = result
let { value } = result
```

**✅ Valid:**
```typescript
import { match, flatMap } from '@qi/base'

// Use functional composition instead
const processed = flatMap(value => 
  success(processValue(value)), 
  result
)
```

## Configuration

### Biome v2.0+

Add to your `biome.json`:

```json
{
  "linter": {
    "enabled": true,
    "rules": {
      "recommended": true
    },
    "plugins": [
      "./biome-plugins/no-result-tag-check.grit",
      "./biome-plugins/no-result-property-access.grit", 
      "./biome-plugins/no-result-destructuring.grit"
    ]
  }
}
```

### Current Setup

These plugins are prepared for Biome v2.0+. Until then, use the ESLint rule in `eslint-rules/` directory which provides the same functionality with TypeScript type checking.

## Usage with ESLint Alternative

For immediate use, the project includes an ESLint plugin with equivalent functionality:

```bash
bun run lint:anti-patterns
```

## Philosophy

These plugins enforce functional programming patterns for `Result<T>` to:

1. **Prevent runtime errors** - No accessing `.value` on failure cases
2. **Ensure exhaustive error handling** - Forces proper pattern matching  
3. **Maintain type safety** - Leverages the discriminated union properly
4. **Encourage composition** - Promotes `map`, `flatMap`, and `match` usage

For more information about `Result<T>` patterns, see the [QiCore Base documentation](../docs).