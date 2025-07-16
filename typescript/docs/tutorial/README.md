# QiCore Foundation Tutorial

Welcome to the comprehensive guide for QiCore Foundation - a TypeScript implementation of mathematical foundation types and infrastructure services.

## Table of Contents

1. [Installation and Setup](./01-installation-setup.md)
2. [Understanding Result<T> - The Foundation](./02-result-basics.md)
3. [Error Handling with QiError](./03-error-handling.md)
4. [Advanced Result Operations](./04-advanced-result.md)
5. [Configuration Management](./05-configuration.md)
6. [Logging with QiCore Logger](./06-logging.md)
7. [Caching Strategies](./07-caching.md)
8. [Integration Patterns](./08-integration-patterns.md)
9. [Testing with QiCore](./09-testing.md)
10. [Best Practices and Patterns](./10-best-practices.md)

## What You'll Learn

- How to use `Result<T>` for functional error handling
- Creating and managing structured errors with `QiError`
- Setting up configuration management with validation
- Implementing structured logging with OpenTelemetry support
- Building caching strategies with memory and Redis backends
- Integrating all components in real-world applications

## Prerequisites

- TypeScript 5.8+ knowledge
- Node.js 18+ or Bun 1.0+
- Basic understanding of functional programming concepts
- Familiarity with async/await patterns

## Quick Start

```bash
# Install the package
npm install @qi/qicore-foundation

# Or with bun
bun add @qi/qicore-foundation
```

```typescript
import { Ok, Err, type Result } from '@qi/qicore-foundation/base'
import { createLogger, createCache } from '@qi/qicore-foundation'

// Your first Result operation
const divide = (a: number, b: number): Result<number> => {
  if (b === 0) return Err(new Error('Division by zero'))
  return Ok(a / b)
}

const result = divide(10, 2)
console.log(result) // { tag: 'success', value: 5 }
```

## Example Applications

All tutorial examples are available in the [`typescript/app`](../app) directory:

- `basic-result` - Simple Result<T> usage patterns
- `error-handling` - Comprehensive error management
- `web-service` - Complete web service with all components
- `cli-tool` - Command-line application example
- `microservice` - Microservice architecture patterns

## Next Steps

Start with [Installation and Setup](./01-installation-setup.md) to begin your journey with QiCore Foundation.