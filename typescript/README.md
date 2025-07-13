# QiCore v4.0 Foundation - TypeScript Implementation

**Mathematical foundation types and infrastructure services implemented in modern TypeScript**

## Overview

This TypeScript implementation provides the foundational types and infrastructure services defined by the QiCore v4.0 contracts. It implements Result<T> monadic error handling and essential infrastructure services (Config, Logger, Cache) with full type safety and performance optimization.

## Project Structure

```
typescript/
├── README.md                     # This file
├── package.json                  # Dependencies and scripts
├── tsconfig.json                 # TypeScript configuration
├── src/
│   ├── base/                     # qi/base implementation
│   │   ├── result.ts            # Result<T> monad
│   │   ├── error.ts             # QiError type
│   │   └── index.ts             # Base exports
│   ├── core/                     # qi/core implementation
│   │   ├── config.ts            # Configuration service
│   │   ├── logger.ts            # Logging service
│   │   ├── cache.ts             # Caching service
│   │   └── index.ts             # Core exports
│   └── index.ts                  # Main exports
├── tests/                        # Test suite
│   ├── base/                     # Base component tests
│   │   ├── result.test.ts       # Result<T> monad tests
│   │   └── error.test.ts        # QiError tests
│   ├── core/                     # Core component tests
│   │   ├── config.test.ts       # Configuration tests
│   │   ├── logger.test.ts       # Logger tests
│   │   └── cache.test.ts        # Cache tests
│   └── integration/              # Integration tests
│       └── foundation.test.ts   # Cross-component tests
├── docs/                         # TypeScript-specific docs
│   ├── api/                      # API documentation
│   ├── examples/                 # Usage examples
│   └── guide/                    # Implementation guide
└── examples/                     # Code examples
    ├── basic-usage.ts           # Basic Result<T> usage
    ├── configuration.ts         # Config examples
    ├── logging.ts               # Logger examples
    └── caching.ts               # Cache examples
```

## Installation

```bash
# Install dependencies with bun
bun install

# Build the library
bun run build

# Run tests
bun test

# Run tests with coverage
bun run test:coverage

# Format and lint code
bun run format
bun run lint

# Type checking
bun run typecheck

# Complete check (format + lint + test + typecheck)
bun run check

# Generate documentation
bun run docs
```

## Quick Start

### Basic Result<T> Usage

```typescript
import { Result, success, failure, QiError } from '@qi/qicore-foundation';

// Create successful results
const validData = success(42);
const computedResult = validData.map(x => x * 2); // Result<number> containing 84

// Handle errors functionally
const riskyOperation = (input: number): Result<number> => {
  if (input < 0) {
    return failure(QiError.create(
      'INVALID_INPUT',
      'Input must be non-negative',
      'VALIDATION'
    ));
  }
  return success(Math.sqrt(input));
};

// Compose operations safely
const pipeline = success(16)
  .flatMap(riskyOperation)     // Result<number> containing 4
  .map(x => `Result: ${x}`);   // Result<string> containing "Result: 4"

// Handle results
pipeline.match(
  value => console.log(value),           // Success case
  error => console.error(error.message)  // Error case
);
```

### Configuration Service

```typescript
import { ConfigService, ConfigData } from '@qi/qicore-foundation';

// Load configuration from multiple sources
const configService = new ConfigService();

// From file
const fileConfig = await configService.fromFile('./config.json');

// From environment variables
const envConfig = await configService.fromEnvironment('APP_');

// From object
const defaultConfig = configService.fromObject({
  database: { host: 'localhost', port: 5432 },
  logging: { level: 'INFO' }
});

// Merge configurations (right-biased)
const finalConfig = ConfigData.merge([
  defaultConfig.unwrap(),
  fileConfig.unwrap(),
  envConfig.unwrap()
]);

// Access nested values safely
const dbHost = finalConfig.get('database.host').unwrapOr('localhost');
const logLevel = finalConfig.get('logging.level').unwrapOr('INFO');
```

### Logging Service

```typescript
import { Logger, LogLevel, LoggerConfig } from '@qi/qicore-foundation';

// Create logger with configuration
const loggerConfig = new LoggerConfig({
  level: LogLevel.INFO,
  format: 'JSON',
  destination: { type: 'console' }
});

const logger = await Logger.create(loggerConfig);

// Structured logging with context
logger.info('User login', { 
  userId: '12345', 
  ip: '192.168.1.1',
  timestamp: new Date().toISOString()
});

// Error logging with full error objects
try {
  await riskyDatabaseOperation();
} catch (error) {
  logger.error('Database operation failed', error, { 
    operation: 'user_lookup',
    retries: 3
  });
}

// Performance-optimized level checking
if (logger.isLevelEnabled(LogLevel.DEBUG)) {
  const expensiveDebugData = computeExpensiveDebugInfo();
  logger.debug('Debug info', { data: expensiveDebugData });
}
```

### Caching Service

```typescript
import { Cache, CacheConfig, TTL } from '@qi/qicore-foundation';

// Create memory cache
const cacheConfig = new CacheConfig({
  maxSize: 1000,
  defaultTTL: TTL.minutes(30),
  evictionPolicy: 'LRU'
});

const cache = await Cache.createMemory(cacheConfig);

// Basic cache operations
await cache.set('user:12345', { name: 'John', email: 'john@example.com' });
const user = await cache.get('user:12345');

// Cache with custom TTL
await cache.set('session:abc123', sessionData, TTL.hours(2));

// Get or compute pattern
const expensiveData = await cache.getOrSet(
  'expensive:calculation',
  async () => {
    return await performExpensiveCalculation();
  },
  TTL.minutes(10)
);

// Batch operations
const userIds = ['12345', '12346', '12347'];
const users = await cache.getMany(userIds.map(id => `user:${id}`));

await cache.setMany(new Map([
  ['key1', 'value1'],
  ['key2', 'value2'],
  ['key3', 'value3']
]), TTL.minutes(5));
```

## Advanced Usage

### Error Handling Patterns

```typescript
import { Result, QiError, ErrorCategory } from '@qi/qicore-foundation';

// Custom error types
class DatabaseError extends QiError {
  constructor(message: string, cause?: Error) {
    super('DB_ERROR', message, ErrorCategory.SYSTEM, {}, cause);
  }
}

// Error recovery patterns
const retryWithBackoff = async <T>(
  operation: () => Promise<Result<T>>,
  maxRetries: number = 3
): Promise<Result<T>> => {
  let lastError: QiError;
  
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    const result = await operation();
    if (result.isSuccess()) {
      return result;
    }
    
    lastError = result.getError();
    
    // Exponential backoff
    if (attempt < maxRetries) {
      await new Promise(resolve => 
        setTimeout(resolve, Math.pow(2, attempt) * 1000)
      );
    }
  }
  
  return failure(lastError.withContext({ 
    totalAttempts: maxRetries 
  }));
};
```

### Configuration Validation

```typescript
import { ConfigData, ConfigValidator } from '@qi/qicore-foundation';

// Define configuration schema
const configSchema = {
  database: {
    host: { type: 'string', required: true },
    port: { type: 'number', required: true, min: 1, max: 65535 },
    ssl: { type: 'boolean', default: false }
  },
  logging: {
    level: { 
      type: 'string', 
      required: true, 
      enum: ['DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'] 
    }
  }
};

// Validate configuration
const validator = new ConfigValidator(configSchema);
const validationResult = validator.validate(config);

if (validationResult.isFailure()) {
  console.error('Configuration validation failed:', 
    validationResult.getError().message);
  process.exit(1);
}
```

### Structured Logging

```typescript
import { Logger, LogContext } from '@qi/qicore-foundation';

// Create logger with persistent context
const requestLogger = logger.withContext({
  requestId: 'req_12345',
  userId: 'user_67890',
  correlationId: 'corr_abcdef'
});

// All subsequent logs include the context automatically
requestLogger.info('Processing request');  // Includes requestId, userId, correlationId
requestLogger.warn('Rate limit approaching', { currentRate: 95 });
requestLogger.error('Request failed', error);

// Nested context (additive)
const operationLogger = requestLogger.withContext({
  operation: 'database_query',
  table: 'users'
});

operationLogger.debug('Query executed', { 
  query: 'SELECT * FROM users WHERE id = ?',
  executionTime: '45ms'
});
```

### High-Performance Caching

```typescript
import { Cache, CacheConfig, EvictionPolicy } from '@qi/qicore-foundation';

// Create optimized cache for high-throughput scenarios
const highPerfCache = await Cache.createMemory(new CacheConfig({
  maxSize: 10000,
  evictionPolicy: EvictionPolicy.LRU,
  defaultTTL: TTL.minutes(15),
  // Enable performance optimizations
  enableMetrics: true,
  preallocation: true
}));

// Batch operations for better performance
const keys = Array.from({ length: 1000 }, (_, i) => `key_${i}`);
const values = await highPerfCache.getMany(keys);

// Streaming large datasets
const largeDataset = await cache.stream('large_dataset:*')
  .map(entry => processEntry(entry))
  .filter(entry => entry.isValid)
  .collect();
```

## Type Definitions

### Core Types

```typescript
// Result<T> monad for error handling
export type Result<T> = Success<T> | Failure;

export class Success<T> {
  constructor(public readonly value: T) {}
  
  isSuccess(): this is Success<T> { return true; }
  isFailure(): this is Failure { return false; }
  
  map<U>(fn: (value: T) => U): Result<U>;
  flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
  match<U>(onSuccess: (value: T) => U, onFailure: (error: QiError) => U): U;
  // ... other monad operations
}

export class Failure {
  constructor(public readonly error: QiError) {}
  
  isSuccess(): this is Success<never> { return false; }
  isFailure(): this is Failure { return true; }
  
  map<U>(_fn: (value: never) => U): Result<U> { return this; }
  flatMap<U>(_fn: (value: never) => Result<U>): Result<U> { return this; }
  // ... error propagation
}

// Structured error type
export class QiError {
  constructor(
    public readonly code: string,
    public readonly message: string,
    public readonly category: ErrorCategory,
    public readonly context: Record<string, unknown> = {},
    public readonly cause?: Error | QiError,
    public readonly timestamp: Date = new Date()
  ) {}
  
  withContext(additionalContext: Record<string, unknown>): QiError;
  withCause(cause: Error | QiError): QiError;
  toString(): string;
  toJSON(): object;
}

// Error categories for retry strategies
export enum ErrorCategory {
  VALIDATION = 'VALIDATION',     // Never retry
  NETWORK = 'NETWORK',          // Exponential backoff
  SYSTEM = 'SYSTEM',            // Linear backoff
  BUSINESS = 'BUSINESS',        // Never retry
  SECURITY = 'SECURITY',        // Never retry
  PARSING = 'PARSING',          // Never retry
  TIMEOUT = 'TIMEOUT',          // Timeout backoff
  UNKNOWN = 'UNKNOWN'           // Cautious retry
}
```

### Configuration Types

```typescript
export class ConfigData {
  constructor(
    private readonly data: ConfigObject,
    public readonly timestamp: Date,
    public readonly source: string
  ) {}
  
  get<T>(key: string): Result<T>;
  getWithDefault<T>(key: string, defaultValue: T): T;
  has(key: string): boolean;
  keys(): string[];
  
  static merge(configs: ConfigData[]): ConfigData;
  static empty(): ConfigData;
}

export interface ConfigObject {
  [key: string]: ConfigValue;
}

export type ConfigValue = 
  | string 
  | number 
  | boolean 
  | ConfigValue[] 
  | ConfigObject 
  | null;
```

### Logging Types

```typescript
export enum LogLevel {
  DEBUG = 10,
  INFO = 20,
  WARN = 30,
  ERROR = 40,
  FATAL = 50
}

export class Logger {
  static async create(config: LoggerConfig): Promise<Result<Logger>>;
  
  debug(message: string, context?: LogContext): void;
  info(message: string, context?: LogContext): void;
  warn(message: string, context?: LogContext): void;
  error(message: string, error?: Error | QiError, context?: LogContext): void;
  fatal(message: string, error?: Error | QiError, context?: LogContext): void;
  
  isLevelEnabled(level: LogLevel): boolean;
  withContext(context: LogContext): Logger;
}

export type LogContext = Record<string, unknown>;
```

### Cache Types

```typescript
export class Cache {
  static async createMemory(config: CacheConfig): Promise<Result<Cache>>;
  static async createPersistent(path: string, config: CacheConfig): Promise<Result<Cache>>;
  
  async get<T>(key: string): Promise<Result<T>>;
  async set<T>(key: string, value: T, ttl?: TTL): Promise<Result<void>>;
  async has(key: string): Promise<boolean>;
  async remove(key: string): Promise<boolean>;
  async clear(): Promise<void>;
  async size(): Promise<number>;
  
  // Advanced operations
  async getOrSet<T>(
    key: string, 
    factory: () => Promise<T>, 
    ttl?: TTL
  ): Promise<Result<T>>;
  
  async getMany<T>(keys: string[]): Promise<Result<Map<string, T>>>;
  async setMany<T>(entries: Map<string, T>, ttl?: TTL): Promise<Result<void>>;
}

export class TTL {
  static seconds(n: number): TTL;
  static minutes(n: number): TTL;
  static hours(n: number): TTL;
  static days(n: number): TTL;
  static never(): TTL;
}
```

## Performance Characteristics

### Result<T> Operations
- **Creation**: O(1) - Zero allocation overhead for success cases
- **Mapping**: O(1) - Function application only
- **Chaining**: O(1) - No intermediate allocations
- **Memory**: ~24 bytes per Result instance

### Configuration Service
- **Direct key access**: O(1) - Hash map lookup
- **Nested key access**: O(depth) - Path traversal
- **Merging**: O(n) - Linear in total config size
- **Validation**: O(keys) - Linear in number of validation rules

### Logging Service  
- **Level checking**: O(1) - Integer comparison with early exit
- **Message formatting**: Lazy - Only computed if level enabled
- **Context merging**: O(context_size) - Object spread operation
- **Async I/O**: Non-blocking - Uses worker threads for file operations

### Cache Service
- **Get/Set operations**: O(1) average - Hash map with LRU tracking
- **Eviction**: O(1) for LRU - Doubly-linked list maintenance
- **TTL checking**: O(1) - Timestamp comparison
- **Batch operations**: O(n) - Linear in batch size

## Contract Compliance

This TypeScript implementation satisfies **ALL** behavioral contracts defined in the language-agnostic specifications:

- ✅ **Result<T> Monad Laws**: Left identity, right identity, associativity
- ✅ **Configuration Monoid Laws**: Associativity, identity, right-bias merge
- ✅ **Error Propagation Laws**: Context preservation, cause chaining
- ✅ **Performance Contracts**: All operations meet specified complexity bounds
- ✅ **Type Safety**: Compile-time verification of correctness

## Testing

```bash
# Run all tests
npm test

# Run specific test suites
npm run test:base          # Base component tests
npm run test:core          # Core component tests  
npm run test:integration   # Integration tests

# Property-based testing
npm run test:properties    # QuickCheck-style tests

# Performance benchmarks
npm run bench             # Performance regression tests

# Coverage reporting
npm run test:coverage     # Generate coverage report
```

## Dependencies

### Runtime Dependencies
- **TypeScript**: ^5.0.0 - Modern TypeScript features
- **@types/node**: ^20.0.0 - Node.js type definitions

### Development Dependencies
- **vitest**: ^1.0.0 - Fast unit testing framework
- **@typescript-eslint**: ESLint integration for TypeScript
- **prettier**: Code formatting
- **fast-check**: Property-based testing library
- **benchmark**: Performance testing

### Optional Dependencies
- **@opentelemetry/api**: For observability integration
- **pino**: High-performance logging backend
- **ioredis**: Redis cache backend

## Integration Examples

### Express.js Middleware

```typescript
import { Logger, ConfigService, QiError } from '@qi/qicore-foundation';
import express from 'express';

const app = express();

// Configuration-driven setup
const config = await ConfigService.fromFile('./config.json');
const logger = await Logger.create(config.get('logging').unwrap());

// Error handling middleware
app.use((err: Error, req: express.Request, res: express.Response, next: express.NextFunction) => {
  const qiError = err instanceof QiError ? err : QiError.fromError(err);
  
  logger.error('Request failed', qiError, {
    method: req.method,
    url: req.url,
    userAgent: req.get('User-Agent')
  });
  
  res.status(500).json({
    error: {
      code: qiError.code,
      message: qiError.message,
      timestamp: qiError.timestamp
    }
  });
});
```

### NestJS Integration

```typescript
import { Injectable, Module } from '@nestjs/common';
import { Logger, Cache, ConfigService } from '@qi/qicore-foundation';

@Injectable()
export class QiCoreService {
  constructor(
    private readonly logger: Logger,
    private readonly cache: Cache,
    private readonly config: ConfigService
  ) {}
  
  async getUserData(userId: string) {
    return this.cache.getOrSet(
      `user:${userId}`,
      async () => {
        this.logger.info('Fetching user from database', { userId });
        return await this.database.findUser(userId);
      },
      TTL.minutes(15)
    );
  }
}

@Module({
  providers: [
    {
      provide: Logger,
      useFactory: async (config: ConfigService) => {
        const logConfig = config.get('logging').unwrap();
        return Logger.create(logConfig).unwrap();
      },
      inject: [ConfigService]
    },
    QiCoreService
  ]
})
export class QiCoreModule {}
```

---

**QiCore Foundation TypeScript**: Production-ready implementation with mathematical rigor, type safety, and performance optimization for modern TypeScript applications.