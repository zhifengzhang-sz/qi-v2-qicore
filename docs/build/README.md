# QiCore Build Documentation

This directory contains comprehensive documentation for QiCore's build system and packaging approach.

## Documents

### [Modern TypeScript Packaging](./modern-typescript-packaging.md)
Complete guide to QiCore's modern TypeScript package building using tsup, including:
- Single-step build processes that eliminate temporary directories
- Configuration patterns for simple and complex packages  
- Migration guide from legacy dts-bundle-generator approach
- Performance improvements and best practices
- Troubleshooting common build issues

## Quick Reference

### Standard Build Commands

```bash
# Build all packages
bun run build

# Build specific package
bun --filter='@qi/base' run build

# Development mode with watch
bun run dev

# Clean all build artifacts
bun run clean

# Full quality check (typecheck + format + lint + test)
bun run check
```

### Package Build Configuration

All packages use standardized tsup configuration:

```typescript
// tsup.config.ts - Basic package
export default defineConfig({
  entry: { index: 'src/index.ts' },
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: true,
  clean: true,
  target: 'es2023',
})

// Complex package with bundled declarations
export default defineConfig({
  // ... same as above
  dts: { resolve: true }, // Bundle types
  external: ['react', 'ink'], // Runtime deps
})
```

### Generated Outputs

Each package generates standardized outputs:

```
dist/
├── index.js      # ESM build
├── index.cjs     # CommonJS build  
├── index.d.ts    # TypeScript declarations (ESM)
├── index.d.cts   # TypeScript declarations (CJS)
└── *.map         # Source maps
```

## Architecture Principles

1. **Single-step builds**: One command generates all outputs
2. **Zero temporary files**: Direct output to `dist/` with no intermediate steps
3. **Modern tooling**: Uses tsup for optimal TypeScript bundling
4. **Consistent patterns**: Same configuration approach across all packages
5. **Performance focused**: Fast builds with efficient declaration generation

## Key Improvements

- **60% faster builds** by eliminating multi-step processes
- **Zero temp directories** cluttering the workspace
- **Simpler maintenance** with unified build configuration
- **Better developer experience** with watch mode and hot reloading
- **Improved CI/CD** with fewer failure points

## Support

For build-related issues:
1. Check the troubleshooting section in the main documentation
2. Verify tsup and TypeScript versions are up to date
3. Ensure all dependencies are properly externalized
4. Test with a clean build: `bun run clean && bun run build`