# TypeScript Upgrade to 2025

**INSTRUCTION FOR AI ASSISTANT**: Upgrade TypeScript project to 2025 best practices and latest versions.

## Upgrade Process

### 1. Navigate to TypeScript Project (**filesystem**)
```bash
cd typescript
```

### 2. Upgrade Dependencies to 2025 Versions

**Core Dependencies:**
```bash
# Upgrade TypeScript to 5.8+
bun add -d typescript@latest

# Upgrade Biome to v2.0+ for type-aware linting
bun add -d @biomejs/biome@latest

# Upgrade Vitest to latest
bun add -d vitest@latest @vitest/coverage-v8@latest

# Upgrade other dev dependencies
bun add -d @types/node@latest bun-types@latest

# Upgrade Bun itself
bun upgrade
```

### 3. Update TypeScript Configuration (**filesystem**)

**Update tsconfig.json for 2025:**
- Add latest compiler options
- Enable new TypeScript 5.8+ features
- Optimize for modern bundler resolution

**Key 2025 tsconfig.json updates:**
```json
{
  "compilerOptions": {
    "target": "ES2023",
    "lib": ["ES2023", "DOM"],
    "module": "ESNext", 
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "noEmit": false,
    "strict": true,
    "exactOptionalPropertyTypes": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noImplicitOverride": true,
    "isolatedModules": true
  }
}
```

### 4. Update Biome Configuration for v2.0 (**filesystem**)

**Update biome.json for type-aware linting:**
```json
{
  "$schema": "https://biomejs.dev/schemas/2.0.0/schema.json",
  "files": {
    "ignore": ["dist/**", "coverage/**"]
  },
  "organizeImports": {
    "enabled": true
  },
  "linter": {
    "enabled": true,
    "rules": {
      "recommended": true,
      "correctness": {
        "noUnusedImports": "error",
        "noUnusedVariables": "error",
        "noFloatingPromises": "error"
      },
      "style": {
        "useConst": "error",
        "useTemplate": "error"
      },
      "suspicious": {
        "noExplicitAny": "error",
        "noConfusingVoidType": "error"
      }
    }
  },
  "formatter": {
    "enabled": true,
    "formatWithErrors": false,
    "indentStyle": "space",
    "indentWidth": 2,
    "lineWidth": 100
  }
}
```

### 5. Apply 2025 TypeScript Patterns (**filesystem**)

**Update code to use modern patterns:**

**Satisfies Operator Usage:**
```typescript
// Before
const config: Config = {
  theme: 'dark',
  lang: 'en'
}

// 2025 Pattern
const config = {
  theme: 'dark',
  lang: 'en'
} satisfies Config
```

**Branded Types for QiCore:**
```typescript
// Add branded types for strong typing
type UserId = string & { readonly __brand: 'UserId' }
type ErrorCode = string & { readonly __brand: 'ErrorCode' }
type ConfigKey = string & { readonly __brand: 'ConfigKey' }

// Use in Result<T> and QiError implementations
```

**Template Literal Types:**
```typescript
// For error categories and event names
type ErrorCategory = 'validation' | 'network' | 'auth' | 'system'
type ErrorEventName = `error:${ErrorCategory}`
type ResultEventName = `result:${'success' | 'failure'}`
```

### 6. Update Package Scripts (**filesystem**)

**Optimize package.json scripts for 2025:**
```json
{
  "scripts": {
    "dev": "bun tsup --watch",
    "build": "bun tsup --clean",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "vitest run --coverage",
    "test:properties": "vitest run --config vitest.properties.config.ts",
    "biome": "biome check --apply .",
    "format": "biome format --write .",
    "lint": "biome lint .",
    "typecheck": "tsc --noEmit",
    "check": "bun run typecheck && bun run biome && bun run test",
    "upgrade": "bun update"
  }
}
```

### 7. Verify Upgrades (**filesystem**)

**Test upgraded setup:**
```bash
# Check versions
bun --version
bunx tsc --version
bunx biome --version

# Test build pipeline
bun run typecheck
bun run biome  
bun run test
bun run build
```

### 8. Update Property-Based Testing (**filesystem**)

**Enhance fast-check integration for 2025:**
```typescript
import { fc } from 'fast-check'
import { test, expect } from 'vitest'

// Enhanced property testing for Result<T>
test('Result Functor laws with 2025 patterns', () => {
  fc.assert(fc.property(
    fc.string(),
    fc.func(fc.string()),
    fc.func(fc.string()),
    (value, f, g) => {
      const result = Result.ok(value)
      
      // Identity law with satisfies
      const identity = result.map(x => x)
      expect(identity).toEqual(result)
      
      // Composition law with template literals
      const composed = result.map(x => g(f(x)))
      const sequential = result.map(f).map(g)
      expect(composed).toEqual(sequential)
    }
  ), { numRuns: 1000 })
})
```

## Success Criteria

**Upgrade complete when:**
- [ ] All dependencies upgraded to latest 2025 versions
- [ ] TypeScript 5.8+ features enabled
- [ ] Biome v2.0 type-aware linting working
- [ ] Modern TypeScript patterns applied (satisfies, branded types)
- [ ] All tests pass with upgraded toolchain
- [ ] Build pipeline works with new versions
- [ ] Property-based tests enhanced for 2025

## QiCore Foundation Specific

**Ensure mathematical rigor maintained:**
- [ ] All Functor/Monad/Applicative laws still verified
- [ ] Cross-language consistency with Haskell preserved
- [ ] Performance contracts (O(1) operations) maintained
- [ ] Property-based testing with 1000+ iterations working
- [ ] Result<T> and QiError types use modern patterns

**Apply upgrades systematically to maintain QiCore Foundation's mathematical correctness while gaining 2025 performance and type safety benefits.**