# TypeScript Project Setup Guide - QiCore Foundation v-0.3.1

## Overview

This guide provides comprehensive setup instructions for developing QiCore Foundation TypeScript implementation using modern 2025 tooling and best practices.

## Prerequisites

### Required Software
- **Bun** 1.1.0+ (recommended runtime and package manager)
- **Node.js** 18.0+ (alternative runtime, if not using Bun)
- **Git** 2.30+
- **TypeScript** 5.3+ (installed via package manager)

### Development Environment
- **VS Code** with TypeScript/Biome extensions
- **Terminal** with modern shell (bash/zsh/fish)

## Project Initialization

### 1. Clone Repository
```bash
git clone https://github.com/qi-platform/qi-v2-qicore.git
cd qi-v2-qicore/typescript
```

### 2. Install Dependencies
```bash
# Using Bun (recommended)
bun install

# Alternative: Using npm
npm install
```

### 3. Verify Installation
```bash
# Check TypeScript compilation
bun run typecheck

# Verify all tools work
bun run check
```

## Development Workflow

### Core Commands
```bash
# Development with hot reload
bun run dev

# Build dual CJS/ESM packages
bun run build

# Run all tests
bun run test

# Run tests with coverage
bun run test:coverage

# Property-based testing
bun run test:properties

# Format and lint code
bun run format

# Type checking
bun run typecheck

# Complete validation pipeline
bun run check
```

### File Structure
```
typescript/
├── src/
│   ├── base/           # Mathematical foundations
│   │   ├── error.ts    # QiError implementation
│   │   ├── result.ts   # Result<T> with discriminated unions
│   │   └── index.ts    # Base exports
│   ├── core/           # Infrastructure services
│   │   ├── config.ts   # Configuration management
│   │   ├── logger.ts   # Structured logging
│   │   ├── cache.ts    # In-memory and Redis caching
│   │   └── index.ts    # Core exports
│   ├── types/          # Shared type definitions
│   └── index.ts        # Main entry point
├── tests/
│   ├── base/           # Base component tests
│   ├── core/           # Core component tests
│   ├── integration/    # Cross-component tests
│   └── properties/     # Property-based tests
├── docs/
│   ├── examples/       # Usage examples
│   └── api/           # Generated API docs
├── dist/              # Build output (generated)
├── package.json       # Project configuration
├── tsconfig.json      # TypeScript configuration
├── biome.json         # Linting and formatting
└── vitest.config.ts   # Test configuration
```

## Configuration Files

### TypeScript Configuration (tsconfig.json)
```json
{
  "compilerOptions": {
    "target": "ES2022",
    "lib": ["ES2022", "DOM"],
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "allowJs": true,
    "strict": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUncheckedIndexedAccess": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "outDir": "./dist",
    "baseUrl": ".",
    "paths": {
      "@/*": ["./src/*"],
      "@/base/*": ["./src/base/*"],
      "@/core/*": ["./src/core/*"]
    }
  },
  "include": [
    "src/**/*",
    "tests/**/*"
  ],
  "exclude": [
    "node_modules",
    "dist"
  ]
}
```

### Biome Configuration (biome.json)
```json
{
  "$schema": "https://biomejs.dev/schemas/1.9.4/schema.json",
  "organizeImports": {
    "enabled": true
  },
  "linter": {
    "enabled": true,
    "rules": {
      "recommended": true,
      "correctness": {
        "noUnusedImports": "error",
        "noUnusedVariables": "error"
      },
      "style": {
        "useConst": "error",
        "useTemplate": "error"
      },
      "suspicious": {
        "noExplicitAny": "warn",
        "noConfusingVoidType": "error"
      }
    }
  },
  "formatter": {
    "enabled": true,
    "formatWithErrors": false,
    "indentStyle": "space",
    "indentWidth": 2,
    "lineWidth": 100,
    "lineEnding": "lf"
  },
  "javascript": {
    "formatter": {
      "quoteStyle": "single",
      "trailingCommas": "es5",
      "semicolons": "asNeeded"
    }
  },
  "typescript": {
    "preferences": {
      "includePackageJsonAutoImports": "on"
    }
  }
}
```

### Vitest Configuration (vitest.config.ts)
```typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        'dist/',
        '**/*.test.ts',
        '**/*.spec.ts'
      ],
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80
        }
      }
    },
    pool: 'threads',
    poolOptions: {
      threads: {
        singleThread: false
      }
    },
    testTimeout: 10000,
    hookTimeout: 10000
  },
  resolve: {
    alias: {
      '@': new URL('./src', import.meta.url).pathname,
      '@/base': new URL('./src/base', import.meta.url).pathname,
      '@/core': new URL('./src/core', import.meta.url).pathname
    }
  }
})
```

### Property-Based Testing Configuration (vitest.properties.config.ts)
```typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['tests/properties/**/*.test.ts'],
    testTimeout: 30000, // Longer timeout for property tests
    pool: 'threads'
  },
  resolve: {
    alias: {
      '@': new URL('./src', import.meta.url).pathname,
      '@/base': new URL('./src/base', import.meta.url).pathname,
      '@/core': new URL('./src/core', import.meta.url).pathname
    }
  }
})
```

### Build Configuration (tsup.config.ts)
```typescript
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
    base: 'src/base/index.ts',
    core: 'src/core/index.ts'
  },
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2022',
  outDir: 'dist',
  external: [
    'ioredis',
    'pino',
    'eventemitter3',
    '@opentelemetry/api',
    '@opentelemetry/sdk-node'
  ]
})
```

## Package Dependencies

### Production Dependencies
```json
{
  "dependencies": {
    "ioredis": "^5.3.2",
    "pino": "^8.17.0",
    "eventemitter3": "^5.0.1",
    "@opentelemetry/sdk-node": "^0.48.0",
    "@opentelemetry/api": "^1.7.0"
  }
}
```

### Development Dependencies
```json
{
  "devDependencies": {
    "@biomejs/biome": "^1.9.4",
    "@types/node": "^20.10.0",
    "@vitest/coverage-v8": "^1.0.0",
    "@fast-check/vitest": "^0.2.1",
    "bun-types": "^1.1.0",
    "fast-check": "^3.15.0",
    "rimraf": "^5.0.5",
    "tsup": "^8.0.0",
    "typedoc": "^0.25.0",
    "typescript": "^5.3.0",
    "vitest": "^1.0.0"
  }
}
```

## Development Guidelines

### Code Style
- Use **single quotes** for strings
- **2 spaces** for indentation
- **100 character** line width
- **Trailing commas** for multi-line structures
- **Semicolons** only where needed (ASI)

### Import Organization
```typescript
// 1. Node.js built-ins
import { readFile } from 'node:fs/promises'

// 2. External libraries
import { Redis } from 'ioredis'
import pino from 'pino'

// 3. Internal modules (absolute paths)
import { Result, Ok, Err } from '@/base/result'
import { QiError } from '@/base/error'

// 4. Relative imports
import { validateConfig } from './validation'
```

### Naming Conventions
- **PascalCase**: Types, interfaces, classes, enums
- **camelCase**: Variables, functions, methods, properties
- **SCREAMING_SNAKE_CASE**: Constants
- **kebab-case**: File names, package names

### Type Definitions
```typescript
// Use branded types for domain-specific values
type UserId = string & { readonly __brand: unique symbol }
type ConfigPath = string & { readonly __brand: unique symbol }

// Use discriminated unions for Result types
type Result<T, E = Error> = 
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

// Use template literal types for validation
type LogLevel = 'debug' | 'info' | 'warn' | 'error'
type APIPath = `/api/${string}`
```

## Testing Strategy

### Unit Tests
```typescript
// tests/base/result.test.ts
import { describe, test, expect } from 'vitest'
import { Ok, Err, match } from '@/base/result'

describe('Result<T>', () => {
  test('Ok creates success result', () => {
    const result = Ok(42)
    expect(result.tag).toBe('success')
    expect(result.value).toBe(42)
  })

  test('pattern matching works correctly', () => {
    const result = Ok(42)
    const message = match(result, {
      success: value => `Got: ${value}`,
      failure: error => `Error: ${error}`
    })
    expect(message).toBe('Got: 42')
  })
})
```

### Property-Based Tests
```typescript
// tests/properties/result.laws.test.ts
import { test } from 'vitest'
import fc from 'fast-check'
import { Ok, from } from '@/base/result'

test.prop([fc.integer()])(
  'Functor identity law',
  (value) => {
    const identity = <T>(x: T): T => x
    const result = from(Ok(value)).map(identity).build()
    expect(result).toEqual(Ok(value))
  }
)
```

### Integration Tests
```typescript
// tests/integration/config-cache.test.ts
import { describe, test, expect } from 'vitest'
import { Config } from '@/core/config'
import { Cache } from '@/core/cache'

describe('Config-Cache Integration', () => {
  test('config can be cached and retrieved', async () => {
    const config = Config.fromObject({ app: { name: 'test' } })
    const cache = new Cache({ maxSize: 100 })
    
    await cache.set('config', config.toObject())
    const cached = await cache.get('config')
    
    expect(cached.isSuccess()).toBe(true)
  })
})
```

## Performance Considerations

### V8 Optimizations
- Use **consistent object shapes** for hidden class optimization
- Prefer **Map/Set** over plain objects for collections
- Use **typed arrays** for numeric data when appropriate

### Memory Management
- Implement **object pooling** for high-frequency allocations
- Use **WeakMap/WeakSet** for caches that don't prevent GC
- Avoid **memory leaks** in event listeners and timers

### Async Patterns
- Use **Promise.allSettled** for parallel operations
- Implement **request deduplication** for expensive operations
- Use **AbortController** for cancellable operations

## Documentation Generation

### API Documentation
```bash
# Generate TypeDoc documentation
bun run docs

# Output: docs/api/
```

### Examples
```typescript
// docs/examples/basic-usage.ts
import { Ok, Err, match } from '@qi/qicore-foundation/base'
import { Config, Logger, Cache } from '@qi/qicore-foundation/core'

// Basic Result usage
const result = Ok(42)
const message = match(result, {
  success: value => `Success: ${value}`,
  failure: error => `Error: ${error}`
})

// Service initialization
const config = Config.fromObject({
  logger: { level: 'info' },
  cache: { maxSize: 1000 }
})

const logger = new Logger(config.get('logger').unwrap())
const cache = new Cache(config.get('cache').unwrap())
```

## Troubleshooting

### Common Issues

#### TypeScript Compilation Errors
```bash
# Clear TypeScript cache
rm -rf node_modules/.cache

# Rebuild TypeScript project references
bun run typecheck --build --force
```

#### Biome Configuration Issues
```bash
# Check Biome configuration
bun run format:check

# Fix formatting issues
bun run format
```

#### Test Failures
```bash
# Run tests with verbose output
bun run test --reporter=verbose

# Run specific test file
bun run test tests/base/result.test.ts
```

### Performance Debugging
```bash
# Profile test execution
bun run test --reporter=verbose --coverage

# Check bundle size
bun run build && du -sh dist/
```

## IDE Configuration

### VS Code Settings (.vscode/settings.json)
```json
{
  "typescript.preferences.includePackageJsonAutoImports": "on",
  "typescript.suggest.autoImports": true,
  "editor.formatOnSave": true,
  "editor.defaultFormatter": "biomejs.biome",
  "editor.codeActionsOnSave": {
    "quickfix.biome": "explicit",
    "source.organizeImports.biome": "explicit"
  }
}
```

### Recommended Extensions
- **Biome** - Formatting and linting
- **TypeScript Importer** - Auto import suggestions
- **Error Lens** - Inline error display
- **Thunder Client** - API testing
- **GitLens** - Git integration

---

**Document Status**: Complete ✅  
**TypeScript Version**: 5.3+  
**Target Runtime**: Bun 1.1+ / Node.js 18+  
**Toolchain**: Biome + Vitest + tsup + fast-check  
**Last Updated**: 2025-01-14