# Haskell → TypeScript Migration Guide - QiCore Foundation

## Overview

This guide provides comprehensive migration strategies for adapting QiCore Foundation concepts from Haskell reference implementation to TypeScript production implementation. The migration preserves mathematical laws and behavioral contracts while embracing TypeScript-native patterns.

## Core Philosophy Differences

### Haskell: Pure Functional with Do-Notation
```haskell
-- Haskell monadic composition
processData :: Input -> Result Output
processData input = do
  validated <- validateInput input
  processed <- computeResult validated  
  saveResult processed
```

### TypeScript: Fluent API with Method Chaining
```typescript
// TypeScript fluent composition (MANDATORY pattern)
const processData = (input: Input): Result<Output> =>
  from(validateInput(input))
    .flatMap(validated => computeResult(validated))
    .flatMap(processed => saveResult(processed))
    .build()
```

**Key Insight**: Same mathematical laws, different ergonomic expression patterns.

## Type System Mapping

### Result<T> Type Representation

#### Haskell ADT (Algebraic Data Type)
```haskell
-- Haskell: Sum type with pattern matching
data Result a = Success a | Failure QiError

case result of
  Success value -> processValue value
  Failure error -> handleError error
```

#### TypeScript Discriminated Union
```typescript
// TypeScript: Discriminated union with exhaustive checking
type Result<T, E = QiError> = 
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }

// Pattern matching with TypeScript switch
const match = <T, E, R>(
  result: Result<T, E>,
  cases: {
    success: (value: T) => R
    failure: (error: E) => R
  }
): R => {
  switch (result.tag) {
    case 'success': return cases.success(result.value)
    case 'failure': return cases.failure(result.error)
  }
}
```

**Migration Strategy**: Convert Haskell ADTs to TypeScript discriminated unions with explicit tag fields for runtime type discrimination.

### QiError Structure Translation

#### Haskell Record Syntax
```haskell
-- Haskell: Record with named fields
data QiError = QiError
  { code :: Text
  , message :: Text
  , category :: ErrorCategory
  , context :: Map Text Value
  , cause :: Maybe QiError
  , timestamp :: UTCTime
  }
```

#### TypeScript Interface with Branded Types
```typescript
// TypeScript: Interface with branded types for type safety
interface QiError {
  readonly code: ErrorCode           // Branded string type
  readonly message: string
  readonly category: ErrorCategory   // String literal union
  readonly context: Record<string, unknown>
  readonly cause?: QiError          // Optional chaining
  readonly timestamp: Date          // Native Date object
}

// Branded types for compile-time safety
type ErrorCode = string & { readonly __brand: unique symbol }
type ErrorCategory = 'VALIDATION' | 'NETWORK' | 'SYSTEM' | 'BUSINESS'
```

**Migration Strategy**: Convert Haskell Maybe to TypeScript optional properties, use branded types for domain-specific strings.

## Monadic Operations Translation

### Function Composition Patterns

#### Haskell: Monadic Bind (>>=)
```haskell
-- Haskell: Sequential monadic composition
processOrder :: OrderId -> Result ProcessedOrder
processOrder orderId = 
  fetchOrder orderId >>= 
  validateOrder >>=
  processPayment >>=
  updateInventory
```

#### TypeScript: Fluent Chain with Builder Pattern
```typescript
// TypeScript: Fluent API with mandatory method chaining
const processOrder = (orderId: OrderId): Result<ProcessedOrder> =>
  from(fetchOrder(orderId))
    .flatMap(order => validateOrder(order))
    .flatMap(validOrder => processPayment(validOrder))
    .flatMap(paidOrder => updateInventory(paidOrder))
    .build()

// Alternative: Async fluent pattern for async operations
const processOrderAsync = async (orderId: OrderId): Promise<Result<ProcessedOrder>> =>
  from(await fetchOrder(orderId))
    .flatMap(order => validateOrder(order))
    .then(async builder => {
      const result = builder.build()
      if (result.tag === 'success') {
        return (await from(processPayment(result.value)))
          .flatMap(paid => updateInventory(paid))
          .build()
      }
      return result
    })
```

### Functor Operations (map)

#### Haskell: fmap with Infix Operator
```haskell
-- Haskell: Functor mapping with <$>
processValue :: Result Int -> Result String
processValue result = show <$> result

-- Or with explicit fmap
processValue' = fmap show
```

#### TypeScript: Fluent map with Type Inference
```typescript
// TypeScript: Method chaining with full type inference
const processValue = (result: Result<number>): Result<string> =>
  from(result)
    .map(value => value.toString())
    .build()

// Complex transformation pipeline
const transformData = (input: Result<RawData>): Result<DisplayData> =>
  from(input)
    .map(raw => normalizeData(raw))
    .map(normalized => enrichData(normalized))
    .map(enriched => formatForDisplay(enriched))
    .build()
```

## Error Handling Migration

### Error Creation Patterns

#### Haskell: Smart Constructors
```haskell
-- Haskell: Smart constructor functions
validationError :: Text -> QiError
validationError msg = QiError
  { code = "VALIDATION_FAILED"
  , message = msg
  , category = Validation
  , context = mempty
  , cause = Nothing
  , timestamp = getCurrentTime
  }
```

#### TypeScript: Factory Functions with Default Values
```typescript
// TypeScript: Factory functions with sensible defaults
const createValidationError = (
  message: string, 
  context: Record<string, unknown> = {}
): QiError => ({
  code: 'VALIDATION_FAILED' as ErrorCode,
  message,
  category: 'VALIDATION',
  context,
  cause: undefined,
  timestamp: new Date()
})

// Builder pattern for complex error construction
const createError = (code: ErrorCode) => ({
  message: (msg: string) => ({
    category: (cat: ErrorCategory) => ({
      context: (ctx: Record<string, unknown> = {}) => ({
        cause: (causeError?: QiError) => ({
          build: (): QiError => ({
            code,
            message: msg,
            category: cat,
            context: ctx,
            cause: causeError,
            timestamp: new Date()
          })
        })
      })
    })
  })
})

// Usage: Fluent error creation
const error = createError('USER_NOT_FOUND' as ErrorCode)
  .message('User with given ID does not exist')
  .category('BUSINESS')
  .context({ userId: '12345' })
  .cause(databaseError)
  .build()
```

### Error Category Mapping

#### Haskell: Sum Type Enumeration
```haskell
-- Haskell: ADT enumeration
data ErrorCategory = Validation | Network | System | Business
  deriving (Show, Eq, Enum, Bounded)
```

#### TypeScript: String Literal Union with Utilities
```typescript
// TypeScript: String literal union for runtime and compile-time safety
export type ErrorCategory = 
  | 'VALIDATION'     // Input validation failures
  | 'NETWORK'        // Network connectivity issues
  | 'SYSTEM'         // System/infrastructure failures  
  | 'BUSINESS'       // Business logic violations

// Utility for exhaustive checking
export const ErrorCategories: ReadonlyArray<ErrorCategory> = [
  'VALIDATION',
  'NETWORK', 
  'SYSTEM',
  'BUSINESS'
] as const

// Type guard for runtime validation
export const isErrorCategory = (value: string): value is ErrorCategory =>
  ErrorCategories.includes(value as ErrorCategory)

// Retry strategy mapping (preserving Haskell behavior)
export const getRetryStrategy = (error: QiError): RetryStrategy => {
  switch (error.category) {
    case 'VALIDATION': return { shouldRetry: false, maxRetries: 0 }
    case 'NETWORK': return { shouldRetry: true, maxRetries: 3, backoffMs: 1000 }
    case 'SYSTEM': return { shouldRetry: true, maxRetries: 2, backoffMs: 5000 }
    case 'BUSINESS': return { shouldRetry: false, maxRetries: 0 }
  }
}
```

## Async Operations Migration

### Promise Integration Patterns

#### Haskell: IO Monad with Exception Handling
```haskell
-- Haskell: IO with exception safety
fetchUserSafely :: UserId -> IO (Result User)
fetchUserSafely userId = do
  result <- try (fetchUser userId)
  case result of
    Left ex -> pure $ Failure (systemError $ show ex)
    Right user -> pure $ Success user
```

#### TypeScript: async/await with Result Integration
```typescript
// TypeScript: Native async integration with Result pattern
const fetchUserSafely = async (userId: UserId): Promise<Result<User>> => {
  try {
    const user = await fetchUser(userId)
    return Ok(user)
  } catch (error) {
    return Err(createSystemError(`Failed to fetch user: ${error}`))
  }
}

// Fluent async pattern with builder
const processUserData = async (userId: UserId): Promise<Result<ProcessedUser>> => {
  const userResult = await fetchUserSafely(userId)
  
  return from(userResult)
    .flatMap(user => validateUser(user))
    .then(async builder => {
      const validated = builder.build()
      if (validated.tag === 'success') {
        try {
          const enriched = await enrichUserData(validated.value)
          return from(Ok(enriched))
            .map(data => processUserAnalytics(data))
            .build()
        } catch (error) {
          return Err(createNetworkError(`Enrichment failed: ${error}`))
        }
      }
      return validated
    })
}
```

### Async Utility Functions

#### Haskell: Traverse and Sequence
```haskell
-- Haskell: Traverse for async collection processing
processUsers :: [UserId] -> IO (Result [User])
processUsers userIds = do
  results <- traverse fetchUserSafely userIds
  pure $ sequence results
```

#### TypeScript: Promise.allSettled with Result Aggregation
```typescript
// TypeScript: Native Promise utilities with Result handling
const processUsers = async (userIds: UserId[]): Promise<Result<User[]>> => {
  const results = await Promise.allSettled(
    userIds.map(id => fetchUserSafely(id))
  )
  
  const users: User[] = []
  const errors: QiError[] = []
  
  for (const result of results) {
    if (result.status === 'fulfilled') {
      const userResult = result.value
      if (userResult.tag === 'success') {
        users.push(userResult.value)
      } else {
        errors.push(userResult.error)
      }
    } else {
      errors.push(createSystemError(`Promise rejected: ${result.reason}`))
    }
  }
  
  return errors.length === 0 
    ? Ok(users)
    : Err(createAggregateError('Multiple user fetch failures', errors))
}

// Utility: Result sequence implementation
const sequence = <T>(results: Result<T>[]): Result<T[]> => {
  const values: T[] = []
  for (const result of results) {
    if (result.tag === 'failure') {
      return result  // Early exit on first failure
    }
    values.push(result.value)
  }
  return Ok(values)
}

// Utility: Result traverse implementation  
const traverse = async <T, U>(
  items: T[],
  fn: (item: T) => Promise<Result<U>>
): Promise<Result<U[]>> => {
  const results = await Promise.all(items.map(fn))
  return sequence(results)
}
```

## Configuration Service Migration

### Type-Safe Configuration

#### Haskell: Configuration with Reader Monad
```haskell
-- Haskell: Reader monad for configuration
data AppConfig = AppConfig
  { dbUrl :: Text
  , logLevel :: LogLevel  
  , cacheSize :: Int
  }

type AppM = ReaderT AppConfig IO

runWithConfig :: AppConfig -> AppM a -> IO a
runWithConfig config action = runReaderT action config
```

#### TypeScript: Configuration with Type Validation
```typescript
// TypeScript: Configuration with runtime validation and type safety
interface AppConfig {
  readonly database: {
    readonly url: string
    readonly poolSize: number
    readonly timeoutMs: number
  }
  readonly logging: {
    readonly level: LogLevel
    readonly format: 'json' | 'text'
  }
  readonly cache: {
    readonly maxSize: number
    readonly ttlSeconds: number
  }
}

// Configuration validation with Result pattern
const validateConfig = (raw: unknown): Result<AppConfig> => {
  return from(parseConfigSchema(raw))
    .flatMap(parsed => validateDatabaseConfig(parsed.database))
    .flatMap(validDb => validateLoggingConfig(parsed.logging))
    .flatMap(validLog => validateCacheConfig(parsed.cache))
    .map(() => parsed as AppConfig)
    .build()
}

// Configuration loading with multiple sources
const loadConfig = async (): Promise<Result<AppConfig>> => {
  const sources = [
    () => loadFromFile('./config.json'),
    () => loadFromEnv(),
    () => loadDefaults()
  ]
  
  for (const source of sources) {
    try {
      const config = await source()
      const validated = validateConfig(config)
      if (validated.tag === 'success') {
        return validated
      }
    } catch (error) {
      // Try next source
      continue
    }
  }
  
  return Err(createSystemError('No valid configuration source found'))
}
```

## Logging Service Migration

### Structured Logging Translation

#### Haskell: Katip Structured Logging
```haskell
-- Haskell: Katip with structured logging
logInfo :: Text -> Object -> AppM ()
logInfo message context = do
  config <- ask
  liftIO $ katipAddContext context $ 
    logInfoS "app" message
```

#### TypeScript: Pino with OpenTelemetry Integration
```typescript
// TypeScript: Pino logger with OpenTelemetry trace correlation
class Logger {
  private readonly pino: Pino.Logger
  
  constructor(config: LoggingConfig) {
    this.pino = pino({
      level: config.level,
      transport: config.format === 'text' ? {
        target: 'pino-pretty'
      } : undefined
    })
  }
  
  info(message: string, context: Record<string, unknown> = {}): void {
    // Add OpenTelemetry trace context automatically
    const traceContext = this.getTraceContext()
    this.pino.info({ ...context, ...traceContext }, message)
  }
  
  error(error: QiError, context: Record<string, unknown> = {}): void {
    const errorContext = {
      error: {
        code: error.code,
        category: error.category,
        message: error.message,
        stack: error.cause ? this.formatErrorChain(error) : undefined
      },
      ...context,
      ...this.getTraceContext()
    }
    
    this.pino.error(errorContext, error.message)
  }
  
  private getTraceContext(): Record<string, unknown> {
    const span = trace.getActiveSpan()
    if (!span) return {}
    
    const spanContext = span.spanContext()
    return {
      traceId: spanContext.traceId,
      spanId: spanContext.spanId
    }
  }
  
  private formatErrorChain(error: QiError, depth = 0): string[] {
    const chain = [`${error.code}: ${error.message}`]
    if (error.cause && depth < 10) {
      chain.push(...this.formatErrorChain(error.cause, depth + 1))
    }
    return chain
  }
}
```

## Cache Service Migration

### STM to Async Patterns

#### Haskell: Software Transactional Memory
```haskell
-- Haskell: STM for thread-safe caching
type Cache k v = TVar (Map k (CacheEntry v))

data CacheEntry v = CacheEntry
  { value :: v
  , expiresAt :: UTCTime
  }

get :: Ord k => k -> Cache k v -> STM (Maybe v)
get key cache = do
  cacheMap <- readTVar cache
  now <- unsafeIOToSTM getCurrentTime
  case Map.lookup key cacheMap of
    Just entry | expiresAt entry > now -> pure $ Just (value entry)
    _ -> pure Nothing
```

#### TypeScript: Memory + Redis with Async Coordination
```typescript
// TypeScript: Two-tier caching with async coordination
class Cache<K extends string | number, V> {
  private readonly memory = new Map<K, CacheEntry<V>>()
  private readonly redis?: Redis
  private readonly config: CacheConfig
  
  constructor(config: CacheConfig, redis?: Redis) {
    this.config = config
    this.redis = redis
    
    // Cleanup expired entries periodically
    setInterval(() => this.cleanup(), config.cleanupIntervalMs)
  }
  
  async get(key: K): Promise<Result<V | null>> {
    try {
      // L1: Check memory cache first
      const memoryEntry = this.memory.get(key)
      if (memoryEntry && !this.isExpired(memoryEntry)) {
        return Ok(memoryEntry.value)
      }
      
      // L2: Check Redis cache
      if (this.redis) {
        const redisValue = await this.redis.get(this.keyString(key))
        if (redisValue) {
          const parsed = JSON.parse(redisValue) as CacheEntry<V>
          if (!this.isExpired(parsed)) {
            // Populate memory cache
            this.memory.set(key, parsed)
            return Ok(parsed.value)
          }
        }
      }
      
      return Ok(null)  // Cache miss
    } catch (error) {
      return Err(createSystemError(`Cache get failed: ${error}`))
    }
  }
  
  async set(key: K, value: V, ttlMs?: number): Promise<Result<void>> {
    try {
      const entry: CacheEntry<V> = {
        value,
        expiresAt: new Date(Date.now() + (ttlMs ?? this.config.defaultTtlMs))
      }
      
      // Set in memory cache
      this.memory.set(key, entry)
      
      // Set in Redis cache if available
      if (this.redis) {
        await this.redis.setex(
          this.keyString(key),
          Math.floor((ttlMs ?? this.config.defaultTtlMs) / 1000),
          JSON.stringify(entry)
        )
      }
      
      return Ok(undefined)
    } catch (error) {
      return Err(createSystemError(`Cache set failed: ${error}`))
    }
  }
  
  private isExpired(entry: CacheEntry<V>): boolean {
    return new Date() > entry.expiresAt
  }
  
  private keyString(key: K): string {
    return `${this.config.keyPrefix}:${key}`
  }
  
  private cleanup(): void {
    const now = new Date()
    for (const [key, entry] of this.memory.entries()) {
      if (this.isExpired(entry)) {
        this.memory.delete(key)
      }
    }
  }
}

interface CacheEntry<V> {
  readonly value: V
  readonly expiresAt: Date
}

interface CacheConfig {
  readonly defaultTtlMs: number
  readonly maxMemorySize: number
  readonly cleanupIntervalMs: number
  readonly keyPrefix: string
}
```

## Mathematical Law Preservation

### Ensuring Contract Compliance

The migration must preserve all mathematical laws across different syntactic expressions:

#### Functor Laws Translation
```typescript
// Haskell: fmap id = id
// TypeScript equivalent:
test.prop([fc.anything()])('Functor identity law', (value) => {
  const identity = <T>(x: T): T => x
  const result = from(Ok(value)).map(identity).build()
  expect(result).toEqual(Ok(value))
})

// Haskell: fmap (f . g) = fmap f . fmap g  
// TypeScript equivalent:
test.prop([fc.integer()])('Functor composition law', (value) => {
  const f = (x: number) => x * 2
  const g = (x: number) => x + 1
  const composed = (x: number) => f(g(x))
  
  const left = from(Ok(value)).map(composed).build()
  const right = from(Ok(value)).map(g).map(f).build()
  expect(left).toEqual(right)
})
```

#### Monad Laws Translation
```typescript
// Haskell: return a >>= f = f a
// TypeScript equivalent:  
test.prop([fc.integer()])('Monad left identity', (value) => {
  const f = (x: number) => Ok(x.toString())
  
  const left = from(Ok(value)).flatMap(f).build()
  const right = f(value)
  expect(left).toEqual(right)
})

// Haskell: m >>= return = m
// TypeScript equivalent:
test.prop([fc.anything()])('Monad right identity', (value) => {
  const result = Ok(value)
  const identity = from(result).flatMap(x => Ok(x)).build()
  expect(identity).toEqual(result)
})
```

## Performance Considerations

### V8 Optimization Patterns

#### Object Shape Consistency
```typescript
// Consistent object shapes for V8 hidden class optimization
interface ResultShape<T> {
  readonly tag: 'success' | 'failure'
  readonly value?: T
  readonly error?: QiError
}

// Always create objects with same property order
const createSuccess = <T>(value: T): Result<T> => ({
  tag: 'success' as const,
  value,
  error: undefined  // Explicit undefined for consistent shape
})

const createFailure = <T>(error: QiError): Result<T> => ({
  tag: 'failure' as const,
  value: undefined,  // Explicit undefined for consistent shape
  error
})
```

#### Memory Pool for High-Frequency Operations
```typescript
// Object pooling for frequently created Result instances
class ResultPool<T> {
  private readonly successPool: Array<{ tag: 'success'; value?: T; error?: QiError }> = []
  private readonly failurePool: Array<{ tag: 'failure'; value?: T; error?: QiError }> = []
  
  acquireSuccess(value: T): Result<T> {
    const result = this.successPool.pop() ?? { tag: 'success' as const, value: undefined, error: undefined }
    result.value = value
    result.error = undefined
    return result as Result<T>
  }
  
  acquireFailure(error: QiError): Result<T> {
    const result = this.failurePool.pop() ?? { tag: 'failure' as const, value: undefined, error: undefined }
    result.value = undefined
    result.error = error
    return result as Result<T>
  }
  
  release(result: Result<T>): void {
    if (result.tag === 'success') {
      this.successPool.push(result as any)
    } else {
      this.failurePool.push(result as any)
    }
  }
}
```

## Testing Strategy Migration

### Property-Based Testing Translation

#### QuickCheck to fast-check
```haskell
-- Haskell QuickCheck property
prop_functorComposition :: Int -> Bool
prop_functorComposition x = 
  let f = (*2)
      g = (+1)
      composed = f . g
      left = fmap composed (Success x)
      right = (fmap f . fmap g) (Success x)
  in left == right
```

```typescript
// TypeScript fast-check equivalent
import fc from 'fast-check'

test.prop([fc.integer()])(
  'Functor composition property', 
  (x) => {
    const f = (n: number) => n * 2
    const g = (n: number) => n + 1
    const composed = (n: number) => f(g(n))
    
    const left = from(Ok(x)).map(composed).build()
    const right = from(Ok(x)).map(g).map(f).build()
    
    expect(left).toEqual(right)
  }
)
```

## Common Migration Patterns

### 1. Maybe to Optional Properties
```haskell
-- Haskell Maybe
data User = User { userName :: Text, userEmail :: Maybe Text }
```
```typescript
// TypeScript optional properties
interface User {
  readonly userName: string
  readonly userEmail?: string  // Optional property
}
```

### 2. List to Array with Utilities
```haskell
-- Haskell list processing
processItems :: [Item] -> Result [ProcessedItem]
processItems = traverse processItem
```
```typescript
// TypeScript array processing
const processItems = async (items: Item[]): Promise<Result<ProcessedItem[]>> =>
  traverse(items, processItem)
```

### 3. Type Classes to Interfaces
```haskell
-- Haskell type class
class Serializable a where
  serialize :: a -> Text
  deserialize :: Text -> Maybe a
```
```typescript
// TypeScript interface  
interface Serializable<T> {
  serialize(value: T): string
  deserialize(text: string): Result<T>
}
```

## Migration Checklist

### Pre-Migration Assessment
- [ ] **Identify core mathematical laws** that must be preserved
- [ ] **Map Haskell types** to equivalent TypeScript discriminated unions
- [ ] **Plan async operation integration** with Promise/async-await patterns
- [ ] **Design fluent API patterns** for TypeScript-native ergonomics

### Implementation Phase
- [ ] **Create discriminated union types** for core ADTs
- [ ] **Implement fluent builder patterns** for all monadic operations
- [ ] **Add property-based tests** to verify mathematical law preservation
- [ ] **Integrate with TypeScript ecosystem** (Bun, Vitest, Biome)

### Validation Phase
- [ ] **Run cross-language consistency tests** against Haskell reference
- [ ] **Verify performance characteristics** meet or exceed requirements
- [ ] **Test async operation patterns** thoroughly
- [ ] **Validate error handling semantics** match Haskell behavior

### Production Readiness
- [ ] **Complete API documentation** with migration examples
- [ ] **Performance benchmarks** demonstrating V8 optimization
- [ ] **Integration examples** with common TypeScript libraries
- [ ] **Deployment patterns** for Node.js and Bun environments

---

**Document Status**: Complete ✅  
**Migration Strategy**: Behavioral Consistency with Native TypeScript Patterns  
**Implementation Phase**: Ready for v-0.3.2 development  
**Last Updated**: 2025-01-14