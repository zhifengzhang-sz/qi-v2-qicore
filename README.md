# QiCore Foundation

Mathematical foundation types and infrastructure services for the QiCore platform.

## Overview

QiCore Foundation provides functional programming building blocks with Result<T> error handling:

- **@qi/base**: Core types (Result<T>, QiError) with functional composition
- **@qi/core**: Infrastructure services (Config, Logger, Cache) 
- **@qi/amsg**: Async message queue with h2A patterns
- **@qi/cli**: Message-driven CLI framework

## Architecture

```
qi-v2-qicore/
├── typescript/                       # TypeScript implementation (v3.0.1)
│   ├── lib/base/                     # @qi/base - Result<T> foundation
│   ├── lib/core/                     # @qi/core - Infrastructure services  
│   ├── lib/amsg/                     # @qi/amsg - Async messaging
│   ├── lib/cli/                      # @qi/cli - CLI framework
│   ├── app/                          # Example applications
│   └── docs/                         # API documentation
├── haskell/                          # Haskell implementation (planned)
├── python/                           # Python implementation (planned)
└── cpp/                              # C++ implementation (planned)
```

## Installation

```bash
# Install packages
bun add @qi/base @qi/core @qi/amsg @qi/cli

# Or clone for development
git clone https://github.com/your-org/qi-v2-qicore.git
cd qi-v2-qicore/typescript
bun install
```

## Quick Start

```typescript
import { type Result, success, failure, match } from '@qi/base';
import { createLogger, createMemoryCache } from '@qi/core';

// Result<T> error handling
const divide = (a: number, b: number): Result<number, string> => 
  b === 0 ? failure("Division by zero") : success(a / b);

const result = divide(10, 2);
match(
  (value) => console.log(`Result: ${value}`),
  (error) => console.error(`Error: ${error}`),
  result
);

// Infrastructure services
const logger = createLogger({ level: 'info' });
const cache = createMemoryCache({ maxSize: 1000 });
```

## Core Components

### @qi/base

Mathematical foundation with Result<T> monadic error handling.

```typescript
// Function composition with error handling
const pipeline = (input: string) =>
  parseNumber(input)
    .flatMap(validateRange)
    .flatMap(processValue)
    .map(formatOutput);

// Error context chaining
const error = networkError('Connection failed', {
  endpoint: '/api/data',
  retryCount: 3
});
```

### @qi/core

Infrastructure services with functional patterns.

```typescript
// Configuration
const config = await loadConfig(['app.json', 'local.json']);
const dbUrl = config.get('database.url', 'localhost:5432');

// Logging with structured context
logger.info('User authenticated', { userId: '123', role: 'admin' });

// Caching
await cache.set('user:123', userData, 3600); // TTL in seconds
const user = await cache.get('user:123');
```

### @qi/amsg

Async message queue with h2A-inspired patterns.

```typescript
import { QiAsyncMessageQueue, MessageType } from '@qi/amsg';

const queue = new QiAsyncMessageQueue({ maxSize: 1000 });

// Producer
await queue.enqueue(messageFactory.createUserInputMessage('Hello', 'cli'));

// Consumer (h2A pattern)
for await (const message of queue) {
  console.log(`Processing: ${message.type}`);
}
```

### @qi/cli

Message-driven CLI framework.

```typescript
import { createCLIAsync, CLIStateManager } from '@qi/cli';

const cli = await createCLIAsync({
  messageQueue: new QiAsyncMessageQueue(),
  stateManager: new CLIStateManager(),
  config: { framework: 'hybrid', enableHotkeys: true }
});

await cli.start();
```

## Development

### Build and Test

```bash
cd typescript

# Build all packages
bun run build

# Run tests (170 tests)
bun run test

# Type checking and linting
bun run check
```

### Examples

```bash
# Basic Result<T> usage
bun app/basic-result/src/index.ts

# Error handling patterns
bun app/error-handling/src/index.ts

# Configuration and logging
bun app/config-example/src/index.ts

# Caching with Redis (requires Redis service)
bun app/cache-example/src/index.ts

# CLI-AMSG integration
bun app/cli-amsg-example/src/index.ts
```

## Implementation Status

| Component | Status | Version | Tests | Features |
|-----------|--------|---------|-------|----------|
| **@qi/base** | ✅ Complete | 3.0.1 | 75 | Result<T>, QiError, async patterns |
| **@qi/core** | ✅ Complete | 3.0.1 | 80 | Config, Logger, Cache |
| **@qi/amsg** | ✅ Complete | 3.0.1 | 15 | Message queue, h2A patterns |
| **@qi/cli** | ✅ Complete | 3.0.1 | 0 | CLI framework, state management |

**Total: 170 tests passing**

## Documentation

- **[API Reference](typescript/docs/api/cli.md)**: Complete @qi/cli API documentation
- **[CLI-AMSG Integration](typescript/docs/cli-amsg/)**: Architecture and usage patterns
- **[Example Apps](typescript/app/)**: Working examples and patterns

## Architecture Principles

- **Functional programming**: Result<T> error handling, immutable data
- **Type safety**: Comprehensive TypeScript coverage
- **Performance**: O(1) operations, efficient memory usage
- **Composability**: Function composition, monadic patterns
- **Error handling**: Structured errors with context chains

## Requirements

- **TypeScript 5.8+**
- **Node.js 22+** or **Bun 1.0+**
- **Redis** (for cache integration tests)

## License

MIT