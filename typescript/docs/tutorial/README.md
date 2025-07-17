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

1. **Start with qi/base** - The framework everything else uses
2. **Add Config** - Essential for any production application  
3. **Add Logger** - Required for observability and debugging
4. **Add Cache** - When performance optimization is needed

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