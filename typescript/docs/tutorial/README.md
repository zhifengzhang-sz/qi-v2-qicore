# QiCore Foundation Tutorial

**One framework, three tools for reliable TypeScript development.**

## Overview

QiCore Foundation provides:
- **qi/base**: Consistent Result<T> and QiError framework for all error handling
- **qi/core**: Three practical tools that use the framework:
  - **Config**: Multi-source configuration loading
  - **Logger**: Structured logging  
  - **Cache**: Performance optimization

## Why QiCore Foundation?

Every development project needs:
1. **Reliable error handling** - Result<T> makes errors explicit and composable
2. **Configuration management** - Load from multiple sources with validation
3. **Structured logging** - Observe what's happening in your application
4. **Performance optimization** - Cache expensive operations

QiCore provides these as a unified system where each tool uses the same Result<T> patterns.

## Tutorial Structure

### 0. [Project Setup](./setup.md) 
**Start here:** How to configure package.json, tsconfig.json, and build system to make `@qi/base` imports work.

**Why you need this:** Without proper setup, `import { success } from '@qi/base'` won't work.

### 1. [Framework: qi/base](./qi-base.md)
The foundation - learn Result<T> and QiError patterns that everything else builds on.

**Why you need this:** Every function that can fail should return Result<T> instead of throwing exceptions.

### 2. [Tool: Config](./qi-core-config.md) 
Multi-source configuration loading with validation.

**Why you need this:** Every application needs configuration from files, environment variables, and defaults.

### 3. [Tool: Logger](./qi-core-logger.md)
Structured logging with context accumulation.

**Why you need this:** You need to observe what's happening in your application, especially errors.

### 4. [Tool: Cache](./qi-core-cache.md)
Performance optimization with memory and Redis caching.

**Why you need this:** When performance matters, caching expensive operations is essential.

## Learning Path

1. **Start with Project Setup** - Configure your build system and imports
2. **Learn qi/base** - The framework everything else uses
3. **Add Config** - Essential for any production application  
4. **Add Logger** - Required for observability and debugging
5. **Add Cache** - When performance optimization is needed

## Import and Export Patterns

QiCore Foundation uses modern package.json exports with clean import paths:

```typescript
// Base foundation types and error handling
import { success, failure, Result, QiError, match, flatMap } from '@qi/base'

// Core infrastructure services  
import { ConfigBuilder } from '@qi/core/config'
import { createLogger } from '@qi/core/logger'
import { createCache } from '@qi/core/cache'

// Convenient aliasing (alternative import names)
import { Ok, Err } from '@qi/base'  // success/failure aliases
```

**Available Import Paths:**
- `@qi/base` - Result<T>, QiError, and all combinators
- `@qi/core/config` - Configuration management
- `@qi/core/logger` - Structured logging
- `@qi/core/cache` - Memory and Redis caching

**Type Imports:**
```typescript
import type { Result, QiError, ErrorCategory } from '@qi/base'
import type { Config, ValidatedConfig } from '@qi/core/config'  
import type { Logger } from '@qi/core/logger'
import type { Cache } from '@qi/core/cache'
```

## Working Examples

All concepts are demonstrated with runnable examples in [`typescript/app/`](../../app/):

- `basic-result/` - qi/base Result<T> patterns
- `error-extension/` - Domain-specific error handling
- `config-example/` - Config + Logger integration  
- `cache-example/` - Caching strategies

## Key Principles

- **Explicit over implicit**: Errors are visible in function signatures
- **Composable**: Small functions combine reliably into larger ones
- **Practical**: Patterns you'll use in every real project
- **Type-safe**: TypeScript ensures correctness at compile time

Each tool naturally uses the qi/base framework, creating a consistent development experience across your entire application.