# lib/core Architecture

The `lib/core` module provides **essential infrastructure services** built on top of `lib/base`, implementing practical tools that every application needs: configuration, logging, and caching.

## Overview

This module implements three core services using functional programming patterns from `lib/base`. All operations return `Result<T>` and follow the same error handling patterns established in the foundation.

## Module Structure

```
lib/core/src/
├── index.ts          # Main exports and service re-exports
├── config.ts         # Configuration management with validation
├── logger.ts         # Structured logging with events
└── cache.ts          # Multi-backend caching (Memory/Redis)
```

## Architecture Principles

### 1. Functional Foundation
- All operations return `Result<T, DomainError>`
- No exceptions in business logic
- Composable operations using `flatMap`, `map`, etc.

### 2. Domain-Specific Error Handling
- **ConfigError** - Configuration-specific failures
- **LoggerError** - Logging-specific failures  
- **CacheError** - Cache-specific failures

### 3. Builder Patterns
- Fluent APIs for complex configuration
- Method chaining with validation
- Immutable configuration objects

### 4. Event-Driven Architecture
- EventEmitter integration for observability
- Non-blocking event emission
- Typed event handlers

## Service Architecture

### 1. Configuration Service

**Purpose**: Multi-source configuration loading with validation

```
┌─────────────────────────────────────────────────────────────┐
│                    ConfigBuilder                            │
├─────────────────────────────────────────────────────────────┤
│ Sources: JSON │ YAML │ TOML │ Environment │ Objects          │
│ ↓ Merge (left-to-right precedence)                          │
│ ↓ Validate (Zod schemas)                                     │
│ ↓ Build → ValidatedConfig                                    │
└─────────────────────────────────────────────────────────────┘
```

**Key Features:**
- **Multiple Sources**: Files, environment variables, objects
- **Precedence Rules**: Later sources override earlier ones
- **Schema Validation**: Zod integration for type safety
- **Immutable Results**: Once built, configurations cannot change

**Anti-Pattern Elimination:**
- ✅ No direct file system exceptions → `fromAsyncTryCatch`
- ✅ No unsafe JSON parsing → Wrapped in Result<T>
- ✅ No silent failures → Explicit validation errors

### 2. Logging Service

**Purpose**: Structured logging with context accumulation

```
┌─────────────────────────────────────────────────────────────┐
│                      Logger                                 │
├─────────────────────────────────────────────────────────────┤
│ Pino Backend │ Custom Events │ Context Merging               │
│ ↓ Level Filtering                                            │
│ ↓ Structured Output (JSON/Pretty)                           │
│ ↓ Event Emission (log, error, level)                        │
└─────────────────────────────────────────────────────────────┘
```

**Key Features:**
- **Pino Integration**: High-performance logging backend
- **Context Accumulation**: Child loggers inherit parent context
- **Event System**: Custom events for monitoring
- **Level Management**: Dynamic level changes

**Anti-Pattern Elimination:**
- ✅ No unsafe `as any` casting → Proper EventEmitter typing
- ✅ No throwing in log operations → Event emission for errors
- ✅ No raw try/catch → Functional error handling

### 3. Caching Service

**Purpose**: Unified caching with multiple backends

```
┌─────────────────────────────────────────────────────────────┐
│                    ICache Interface                         │
├─────────────────────────────────────────────────────────────┤
│              MemoryCache  │  RedisCache                     │
├───────────────────────────┼─────────────────────────────────┤
│ • LRU Eviction           │ • Redis Pipeline Operations      │
│ • TTL Management         │ • ioredis Integration            │
│ • Event Emission         │ • Native TTL Support             │
│ • Statistics Tracking    │ • Batch Operations               │
└─────────────────────────────────────────────────────────────┘
```

**Key Features:**
- **Unified Interface**: Same API for memory and Redis
- **Performance Optimized**: O(1) operations, efficient pipelines
- **Event-Driven**: Hit/miss/eviction events
- **Batch Operations**: Multi-get/set/delete support

**Anti-Pattern Elimination:**
- ✅ No throwing for cache misses → Functional cache miss handling
- ✅ No pipeline exceptions → Wrapped in `fromAsyncTryCatch`
- ✅ No unsafe resource cleanup → Result<T> for all operations

## Domain Error Types

### ConfigError
```typescript
interface ConfigError extends QiError {
  readonly category: 'CONFIGURATION'
  readonly context: {
    readonly source?: ConfigSource
    readonly path?: string
    readonly validationErrors?: unknown[]
  }
}
```

**Common Scenarios:**
- File not found or unreadable
- Invalid JSON/YAML/TOML syntax
- Schema validation failures
- Environment variable parsing errors

### LoggerError
```typescript
interface LoggerError extends QiError {
  readonly category: 'LOGGER'
  readonly context: {
    readonly operation?: string
    readonly level?: LogLevel
    readonly loggerName?: string
  }
}
```

**Common Scenarios:**
- Invalid log level configuration
- Logger creation failures
- Event system errors (non-blocking)

### CacheError
```typescript
interface CacheError extends QiError {
  readonly category: 'RESOURCE'
  readonly context: {
    readonly operation?: string
    readonly key?: string
    readonly backend?: CacheBackend
    readonly cache?: string
  }
}
```

**Common Scenarios:**
- Cache capacity exceeded
- Redis connection failures
- Serialization/deserialization errors
- TTL parsing failures

## Integration Patterns

### 1. Service Composition

Services are designed to work together:

```typescript
// Configuration drives other services
const configResult = ConfigBuilder
  .fromYamlFile('./config.yaml')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .validateWith(appSchema)
  .build()

match(
  config => {
    // Create logger from config
    const loggerResult = createLogger({
      level: config.logging.level,
      name: config.app.name
    })
    
    // Create cache from config
    const cacheResult = createCache({
      backend: config.cache.backend,
      redis: config.redis
    })
    
    // All services use Result<T> patterns
    return { config, logger: loggerResult.value, cache: cacheResult.value }
  },
  error => {
    console.error('Failed to initialize services:', error.message)
    process.exit(1)
  },
  configResult
)
```

### 2. Error Propagation

Errors compose naturally:

```typescript
const initializeApp = async (): Promise<Result<AppServices, QiError>> => {
  const config = ConfigBuilder.fromEnv('APP_').build()
  
  return flatMapAsync(
    async (cfg) => {
      const logger = createLogger({ level: cfg.logLevel })
      return flatMap(
        (log) => {
          const cache = createCache({ backend: 'redis' })
          return map(
            (c) => ({ config: cfg, logger: log, cache: c }),
            cache
          )
        },
        logger
      )
    },
    config
  )
}
```

### 3. Event Integration

All services emit typed events:

```typescript
// Logger events
logger.on('log', (entry) => {
  // Custom log processing
})

logger.on('error', (error) => {
  // Handle logging errors
})

// Cache events
cache.on('hit', (key, value) => {
  // Track cache performance
})

cache.on('miss', (key) => {
  // Handle cache misses
})
```

## Performance Characteristics

### Configuration
- **Loading**: O(n) where n is configuration size
- **Merging**: O(m) where m is number of sources
- **Validation**: Depends on Zod schema complexity
- **Access**: O(1) after build

### Logging  
- **Core Logging**: O(1) via Pino
- **Event Emission**: O(k) where k is number of listeners
- **Context Merging**: O(c) where c is context size
- **Level Checking**: O(1) with Pino optimization

### Caching
- **Memory Cache**: O(1) for get/set, O(log n) for LRU eviction
- **Redis Cache**: O(1) for most operations, O(n) for pipeline batches
- **TTL Management**: O(1) with native Redis TTL
- **Event Emission**: O(k) where k is number of listeners

## Quality Assurance

### Testing Strategy
- **Unit Tests**: Each service thoroughly tested
- **Integration Tests**: Cross-service interactions tested
- **Property Tests**: Mathematical properties verified
- **Performance Tests**: O(n) complexity verified

### Error Handling Coverage
- **100% Result<T>** return types for fallible operations
- **Comprehensive error categories** for retry strategies
- **No silent failures** - all errors are explicit
- **Composable error handling** with functional patterns

### Type Safety
- **Generic constraints** prevent misuse
- **Domain-specific types** for each service
- **EventEmitter proper typing** (no `as any`)
- **Immutable data structures** throughout

## Usage Examples

### Configuration Management
```typescript
// Multi-source configuration with validation
const schema = z.object({
  database: z.object({
    host: z.string(),
    port: z.number().default(5432)
  }),
  api: z.object({
    url: z.string().url(),
    timeout: z.number().default(5000)
  })
})

const configResult = ConfigBuilder
  .fromJsonFile('./config.json')           // Base configuration
  .merge(ConfigBuilder.fromEnv('APP_'))    // Environment overrides
  .merge(ConfigBuilder.fromObject({        // Default values
    api: { timeout: 5000 },
    database: { port: 5432 }
  }))
  .validateWith(schema)                    // Schema validation
  .build()

match(
  config => {
    console.log('Database host:', config.database.host)
    console.log('API URL:', config.api.url)
  },
  error => {
    console.error('Configuration error:', error.message)
    if (error.context?.validationErrors) {
      console.error('Validation errors:', error.context.validationErrors)
    }
  },
  configResult
)
```

### Structured Logging
```typescript
// Create logger with configuration
const loggerResult = createLogger({
  level: 'info',
  pretty: process.env.NODE_ENV === 'development',
  name: 'my-app'
})

match(
  logger => {
    // Structured logging with context
    logger.info('Application started', undefined, {
      version: '1.0.0',
      environment: process.env.NODE_ENV
    })

    // Child logger with inherited context
    const requestLogger = logger.child({ requestId: 'req-123' })
    requestLogger.info('Processing request')

    // Error logging with QiError
    const error = validationError('Invalid user input')
    logger.error('Validation failed', error, { userId: 123 })
  },
  error => console.error('Logger creation failed:', error.message),
  loggerResult
)
```

### Multi-Backend Caching
```typescript
// Create Redis cache
const cacheResult = createCache({
  backend: 'redis',
  redis: {
    host: 'localhost',
    port: 6379
  },
  defaultTtl: 300
})

match(
  async (cache) => {
    // Set value with TTL
    const setResult = await cache.set('user:123', { name: 'Alice' }, 60)
    
    match(
      () => console.log('User cached'),
      error => console.error('Cache set failed:', error.message),
      setResult
    )

    // Get value
    const getResult = await cache.get<User>('user:123')
    
    match(
      user => console.log('Retrieved user:', user.name),
      error => {
        if (error.message.includes('Cache miss')) {
          console.log('User not in cache, fetching from database...')
        } else {
          console.error('Cache error:', error.message)
        }
      },
      getResult
    )

    // Batch operations
    const batchResult = await cache.mget<User>(['user:123', 'user:456'])
    
    match(
      users => console.log('Retrieved users:', Object.keys(users)),
      error => console.error('Batch get failed:', error.message),
      batchResult
    )
  },
  error => console.error('Cache creation failed:', error.message),
  cacheResult
)
```

## Anti-Pattern Elimination Results

The core module has been systematically cleaned:

✅ **Configuration Service:**
- Fixed cache miss handling to use proper Result<T> patterns
- Eliminated unsafe type assertions
- Wrapped all file operations in `fromAsyncTryCatch`

✅ **Logging Service:**
- Replaced `as any` casting with proper EventEmitter typing
- Event-based error handling instead of throwing
- Functional composition throughout

✅ **Caching Service:**
- Fixed cache miss error handling
- Eliminated pipeline throw statements (properly wrapped)
- Resource cleanup returns Result<T>

All services now follow functional programming principles and integrate seamlessly with the `lib/base` foundation.