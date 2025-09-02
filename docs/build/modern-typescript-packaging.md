# Modern TypeScript Package Building with tsup

This document outlines QiCore's modern TypeScript package building approach using tsup for efficient, single-step builds that eliminate complex temporary directory processes.

## Overview

QiCore uses [tsup](https://tsup.egoist.dev/) as the primary build tool for all TypeScript packages, providing:

- **Single-step builds** that generate JS, CJS, and TypeScript declarations in one command
- **Zero configuration** for most common use cases
- **Native TypeScript support** with built-in declaration file generation
- **Modern bundling** with tree-shaking and code splitting
- **Fast performance** compared to complex multi-tool pipelines

## Architecture

### Package Structure

Each QiCore package follows this standard structure:

```
lib/{package}/
├── src/
│   ├── index.ts          # Main entry point
│   └── ...               # Source files
├── dist/                 # Generated build output
│   ├── index.js          # ESM build
│   ├── index.cjs         # CommonJS build
│   ├── index.d.ts        # TypeScript declarations (ESM)
│   ├── index.d.cts       # TypeScript declarations (CJS)
│   └── *.map            # Source maps
├── package.json
├── tsconfig.json
└── tsup.config.ts        # Build configuration
```

### Build Process

The build process is intentionally simple:

1. **Single Command**: `tsup` reads configuration and generates all outputs
2. **Parallel Generation**: JS, CJS, and TypeScript declarations are built simultaneously
3. **Direct Output**: Everything goes straight to `dist/` with no intermediate steps
4. **Automatic Cleanup**: `clean: true` ensures fresh builds

## Configuration Patterns

### Basic Package (base, core, amsg)

For packages with simple TypeScript declaration needs:

```typescript
// tsup.config.ts
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: true, // Generate TypeScript declarations directly
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2023',
  outDir: 'dist',
  external: ['@qi/base'], // External dependencies (if any)
  bundle: true,
  tsconfig: './tsconfig.json',
})
```

### Complex Package (cli)

For packages with complex type dependencies that need bundling:

```typescript
// tsup.config.ts
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: {
    resolve: true, // Bundle external types into single .d.ts file
  },
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2023',
  outDir: 'dist',
  external: ['react', 'ink', 'ink-text-input', 'chalk', 'xstate'],
  bundle: true,
  tsconfig: './tsconfig.json',
})
```

### Package.json Scripts

All packages use the same simplified build scripts:

```json
{
  "scripts": {
    "build": "tsup",
    "dev": "tsup --watch",
    "clean": "rimraf dist",
    "typecheck": "tsc --noEmit --pretty"
  }
}
```

## Key Benefits

### Eliminated Complex Processes

**Before (Legacy)**:
```bash
# 4-step complex process with temp directories
1. tsup                                    # JS files to dist/
2. tsc --project tsconfig.build.json      # .d.ts files to temp/
3. dts-bundle-generator -o dist/ temp/     # Bundle temp/ to dist/
4. rimraf temp                             # Clean up temporary files
```

**After (Modern)**:
```bash
# Single-step modern process
tsup  # Everything directly to dist/ (JS + .d.ts)
```

### Performance Improvements

- **Build Speed**: ~60% faster builds by eliminating multi-step processes
- **Development**: Watch mode with hot reloading for rapid iteration
- **CI/CD**: Simpler build pipelines with fewer failure points
- **Disk Usage**: No temporary directories cluttering the workspace

### Maintainability Benefits

- **Fewer Dependencies**: Removed `dts-bundle-generator` from all packages
- **Simpler Configuration**: Single `tsup.config.ts` instead of multiple build configs
- **Reduced Complexity**: No temporary directories to manage or clean up
- **Modern Tooling**: Leverages latest TypeScript and bundling best practices

## Declaration File Strategies

### Simple Declarations (`dts: true`)

For packages with straightforward type exports:

```typescript
// Generates individual .d.ts files that mirror source structure
dts: true
```

**Use for**: Base types, utilities, simple libraries

### Bundled Declarations (`dts: { resolve: true }`)

For packages with complex interdependencies:

```typescript
// Bundles all types into single .d.ts file, resolving external references
dts: {
  resolve: true
}
```

**Use for**: Complex frameworks, packages with many internal modules

## Migration Guide

### From dts-bundle-generator

If migrating existing packages from the old complex build process:

1. **Update tsup.config.ts**:
   ```typescript
   // Change from:
   dts: false, // Using dts-bundle-generator instead
   
   // To:
   dts: true, // Generate TypeScript declarations directly
   // Or for complex packages:
   dts: { resolve: true }
   ```

2. **Simplify package.json scripts**:
   ```json
   // Remove complex build:types script
   "scripts": {
     "build": "tsup",  // Simple!
     "clean": "rimraf dist"  // No more temp cleanup
   }
   ```

3. **Remove dependencies**:
   ```bash
   bun remove dts-bundle-generator
   ```

4. **Delete unnecessary files**:
   ```bash
   rm tsconfig.build.json  # No longer needed
   ```

### Testing the Migration

After migration, verify the build works correctly:

```bash
# Clean build
bun run clean && bun run build

# Verify outputs
ls dist/
# Should see: index.js, index.cjs, index.d.ts, index.d.cts, *.map

# Run full quality checks
bun run check
```

## Troubleshooting

### Common Issues

**Type Declaration Conflicts**:
```typescript
// If you see type name collisions, use bundled declarations:
dts: { resolve: true }
```

**External Dependencies**:
```typescript
// Ensure runtime dependencies are externalized:
external: ['react', '@qi/base', 'lodash']
```

**Build Performance**:
```typescript
// For faster development builds:
sourcemap: process.env.NODE_ENV !== 'production',
minify: process.env.NODE_ENV === 'production',
```

### Best Practices

1. **Keep externals minimal**: Only externalize true runtime dependencies
2. **Use resolve: true sparingly**: Only for packages with complex type interdependencies  
3. **Verify declaration quality**: Check generated `.d.ts` files for completeness
4. **Test both formats**: Ensure both ESM and CJS builds work correctly

## Related Documentation

- [tsup Documentation](https://tsup.egoist.dev/)
- [TypeScript Declaration Files](https://www.typescriptlang.org/docs/handbook/declaration-files/introduction.html)
- [Node.js Package.json Exports](https://nodejs.org/api/packages.html#exports)

## Implementation History

This modern build approach was implemented in January 2025 to:

- Eliminate complex temporary directory processes
- Improve build performance and developer experience  
- Simplify maintenance and CI/CD pipelines
- Adopt modern TypeScript packaging best practices

The migration successfully reduced build complexity from 4 steps to 1 step while maintaining full compatibility and improving performance.