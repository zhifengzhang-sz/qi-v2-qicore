# TypeScript Project Setup Guide - QiCore Foundation v-0.3.3

## Overview

This guide covers the specific setup requirements for the QiCore Foundation TypeScript project, focusing on proper directory structure and aliasing configuration.

## Project Structure

The project uses a `lib/` directory containing both source code and tests:

```
typescript/
├── lib/
│   ├── src/
│   │   ├── base/           # Mathematical foundations (Result<T>, QiError)
│   │   │   ├── error.ts    # QiError implementation
│   │   │   ├── result.ts   # Result<T> with discriminated unions
│   │   │   └── index.ts    # Base exports
│   │   ├── types/          # Shared type definitions
│   │   └── index.ts        # Main entry point
│   └── tests/
│       ├── base/           # Base component tests
│       └── properties/     # Property-based tests (mathematical laws)
├── dist/                   # Build output (generated)
├── package.json           # Project configuration
├── tsconfig.json          # TypeScript configuration
├── biome.json             # Linting and formatting
└── vitest.config.ts       # Test configuration
```

## TypeScript Configuration

### Critical Path Aliasing Setup

The project uses `@qi/` aliasing to match the eventual published package structure:

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "lib": ["ES2022", "DOM"],
    "module": "ESNext",
    "moduleResolution": "bundler",
    "rootDir": "./lib",
    "outDir": "./dist",
    "baseUrl": ".",
    "paths": {
      "@qi/base": ["./lib/src/base/index.ts"],
      "@qi/base/*": ["./lib/src/base/*"]
    }
  },
  "include": [
    "lib/src/**/*",
    "lib/tests/**/*"
  ],
  "exclude": [
    "node_modules",
    "dist"
  ]
}
```

### Key Configuration Points

1. **rootDir**: Set to `./lib` to match the actual source structure
2. **outDir**: Set to `./dist` for build output
3. **baseUrl**: Set to `"."` (project root) for path resolution
4. **paths**: Use `@qi/base` aliases to match published package imports
5. **include**: Only include files from `lib/src/` and `lib/tests/`

## Vitest Configuration

The test configuration must also use the correct aliasing:

```typescript
// vitest.config.ts
import { defineConfig } from 'vitest/config'
import { resolve } from 'path'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['lib/src/**/*'],
      exclude: ['node_modules/', 'dist/', '**/*.test.ts']
    }
  },
  resolve: {
    alias: {
      '@qi/base': resolve(__dirname, 'lib/src/base/index.ts')
    }
  }
})
```

## tsup Build Configuration

For building the library with correct output structure:

```typescript
// tsup.config.ts
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'lib/src/index.ts',
    base: 'lib/src/base/index.ts'
  },
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: true,
  clean: true,
  outDir: 'dist'
})
```

## Important Notes

### Directory Structure Requirements
- **Source code**: Must be in `lib/src/`
- **Tests**: Must be in `lib/tests/`
- **Build output**: Goes to `dist/`
- **Root directory**: `lib/` contains both source and tests

### Aliasing Requirements
- Use `@qi/base` for base module imports
- Paths must point to actual file locations in `lib/src/`
- Both TypeScript and Vitest configs must use same aliasing

### Common Issues
1. **Path resolution fails**: Check that `baseUrl` is set to `"."` and paths are relative to project root
2. **Tests can't find imports**: Ensure Vitest alias config matches TypeScript paths
3. **Build fails**: Verify `rootDir` is set to `./lib` and entry points are correct

---

**Document Status**: Updated for v-0.3.3 project structure ✅  
**Focus**: Directory structure and aliasing setup  
**Key Points**: lib/ directory structure, @qi/ aliasing, rootDir configuration  
**Last Updated**: 2025-01-14