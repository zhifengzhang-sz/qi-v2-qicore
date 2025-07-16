# QiCore Foundation API Documentation

Complete API reference for QiCore Foundation TypeScript implementation.

## Table of Contents

### Base Module (`@qi/qicore-foundation/base`)
- [Result<T>](./base/result.md) - Functional error handling type
- [QiError](./base/error.md) - Structured error management
- [Utility Functions](./base/utils.md) - Helper functions and utilities

### Core Module (`@qi/qicore-foundation`)
- [Configuration](./core/config.md) - Configuration management
- [Logger](./core/logger.md) - Structured logging
- [Cache](./core/cache.md) - Caching strategies
- [Integration](./core/integration.md) - Service integration patterns

## Quick Reference

### Base Module

```typescript
// Import base types
import { 
  Ok, Err, type Result, type QiError,
  map, flatMap, match, unwrapOr
} from '@qi/qicore-foundation/base'

// Basic usage
const result: Result<number> = Ok(42)
const mapped = map(result, x => x * 2)
```

### Core Module

```typescript
// Import core services
import { 
  createLogger, createCache, ConfigBuilder 
} from '@qi/qicore-foundation'

// Basic usage
const logger = createLogger({ name: 'app' })
const cache = createCache({ backend: 'memory' })
const config = ConfigBuilder.create().fromEnv().build()
```

## API Design Principles

### 1. Functional First
- Pure functions for transformations
- Immutable data structures
- Composable operations

### 2. Type Safety
- Explicit error types in function signatures
- No runtime exceptions
- Comprehensive TypeScript support

### 3. Mathematical Foundations
- Functor, Applicative, and Monad laws
- Predictable behavior
- Composable abstractions

### 4. Real-World Practicality
- Integration with existing TypeScript ecosystems
- Performance-conscious implementations
- Production-ready patterns

## Module Organization

### Base Module Structure
```
@qi/qicore-foundation/base
├── Result<T>           # Core Result type and operations
├── QiError             # Structured error handling
└── Utilities           # Helper functions and type guards
```

### Core Module Structure
```
@qi/qicore-foundation
├── Config              # Configuration management
├── Logger              # Structured logging
├── Cache               # Caching strategies
└── Integration         # Service composition patterns
```

## Version Information

- **Current Version**: 0.3.4
- **TypeScript Version**: 5.8+
- **Node.js Version**: 18+
- **ES Target**: ES2023

## Stability Guarantees

### Stable APIs
- All base module functions and types
- Core service creation functions
- Configuration interfaces

### Experimental Features
- Advanced composition utilities
- Performance optimization helpers
- Integration helpers

## Migration Guide

### From v0.3.x to v0.4.x
- No breaking changes planned
- Additive API improvements
- Backward compatibility maintained

## Contributing

- All public APIs must include comprehensive documentation
- Breaking changes require major version bump
- New features should include usage examples

## Support

- [GitHub Issues](https://github.com/qi-platform/qi-v2-qicore/issues)
- [Discussions](https://github.com/qi-platform/qi-v2-qicore/discussions)
- [Tutorial](../tutorial/README.md)
- [Examples](../../app/README.md)