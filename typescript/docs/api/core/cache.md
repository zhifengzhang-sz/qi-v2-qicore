# Cache API Reference

The Cache module provides unified caching interface with multiple backends (memory and Redis) using functional patterns. It implements Result<T> for consistent error handling and provides high-performance operations with TTL management and LRU eviction.

## Core Interface

### ICache

Unified cache interface for both memory and Redis backends.

```typescript
interface ICache {
  // Core operations
  get<T>(key: string): Promise<Result<T, CacheError>>
  set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  delete(key: string): Promise<Result<boolean, CacheError>>
  has(key: string): Promise<Result<boolean, CacheError>>
  remove(key: string): Promise<Result<boolean, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  size(): Promise<Result<number, CacheError>>
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  
  // Batch operations
  mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  mdelete(keys: string[]): Promise<Result<number, CacheError>>
  
  // Advanced operations
  getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>>
  
  // Management
  getStats(): CacheStats
  close(): Promise<void>
}
```

## Core Classes

### MemoryCache

In-memory cache with LRU eviction and TTL support.

```typescript
class MemoryCache implements ICache {
  constructor(config: CacheConfig)
  
  async get<T>(key: string): Promise<Result<T, CacheError>>
  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  async delete(key: string): Promise<Result<boolean, CacheError>>
  async has(key: string): Promise<Result<boolean, CacheError>>
  async exists(key: string): Promise<Result<boolean, CacheError>>
  async remove(key: string): Promise<Result<boolean, CacheError>>
  async size(): Promise<Result<number, CacheError>>
  async clear(): Promise<Result<void, CacheError>>
  async keys(pattern?: string): Promise<Result<string[], CacheError>>
  async mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  async mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  async mdelete(keys: string[]): Promise<Result<number, CacheError>>
  async getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>>
  getStats(): CacheStats
  async close(): Promise<void>
}
```

### RedisCache

Redis-backed cache with connection pooling and pipeline operations.

```typescript
class RedisCache implements ICache {
  constructor(config: CacheConfig)
  
  async get<T>(key: string): Promise<Result<T, CacheError>>
  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  async delete(key: string): Promise<Result<boolean, CacheError>>
  async has(key: string): Promise<Result<boolean, CacheError>>
  async exists(key: string): Promise<Result<boolean, CacheError>>
  async remove(key: string): Promise<Result<boolean, CacheError>>
  async size(): Promise<Result<number, CacheError>>
  async clear(): Promise<Result<void, CacheError>>
  async keys(pattern?: string): Promise<Result<string[], CacheError>>
  async mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  async mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  async mdelete(keys: string[]): Promise<Result<number, CacheError>>
  async getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>>
  getStats(): CacheStats
  async close(): Promise<void>
  
  // Redis-specific methods
  async ttl(key: string): Promise<Result<number, CacheError>>
  async expire(key: string, seconds: number): Promise<Result<boolean, CacheError>>
}
```

## Type Definitions

```typescript
type CacheBackend = 'memory' | 'redis'

interface CacheEntry<T = unknown> {
  readonly value: T
  readonly createdAt: Date
  readonly expiresAt?: Date
  readonly accessCount: number
  readonly lastAccessed: Date
}

interface CacheConfig {
  readonly backend: CacheBackend
  readonly maxSize?: number
  readonly defaultTtl?: number
  readonly redis?: RedisOptions
  readonly name?: string
}

interface CacheStats {
  readonly hits: number
  readonly misses: number
  readonly sets: number
  readonly deletes: number
  readonly evictions: number
  readonly size: number
  readonly maxSize?: number
}

interface CacheEvents {
  hit: (key: string, value: unknown) => void
  miss: (key: string) => void
  set: (key: string, value: unknown, ttl?: number) => void
  delete: (key: string) => void
  evict: (key: string, reason: 'lru' | 'ttl' | 'manual') => void
  error: (error: CacheError) => void
  connect: () => void
  disconnect: () => void
}

type CacheError = QiError & {
  readonly category: 'RESOURCE'
  readonly context: {
    readonly operation?: string
    readonly key?: string
    readonly backend?: CacheBackend
    readonly cache?: string
  }
}
```

## Factory Functions

### createCache(config: CacheConfig): ICache

Creates a cache instance based on the configuration.

```typescript
// Memory cache
const cache = createCache({
  backend: 'memory',
  maxSize: 1000,
  defaultTtl: 300
})

// Redis cache
const cache = createCache({
  backend: 'redis',
  redis: {
    host: 'localhost',
    port: 6379
  }
})
```

### createMemoryCache(config: Omit<CacheConfig, 'backend'>): MemoryCache

Creates a memory cache instance.

```typescript
const cache = createMemoryCache({
  maxSize: 500,
  defaultTtl: 60
})
```

### createRedisCache(config: Omit<CacheConfig, 'backend'>): RedisCache

Creates a Redis cache instance.

```typescript
const cache = createRedisCache({
  redis: {
    host: 'redis.example.com',
    port: 6379,
    password: 'secret'
  }
})
```

### createPersistent(filePath: string, config: Omit<CacheConfig, 'backend' | 'redis'>): Result<RedisCache, CacheError>

Create persistent cache (Redis-backed) with file path configuration.

```typescript
const cacheResult = createPersistent('/tmp/cache', {
  defaultTtl: 3600
})

if (cacheResult.tag === 'success') {
  const cache = cacheResult.value
  // Use persistent cache
}
```

## Utility Functions

### cacheAside<T>(key: string, cache: ICache, loader: () => Promise<T>, ttl?: number): Promise<Result<T, CacheError>>

Implements cache-aside pattern with automatic loading.

```typescript
const user = await cacheAside(
  `user:${id}`,
  cache,
  () => fetchUserFromDatabase(id),
  300 // 5 minutes TTL
)
```

## Error Factory

### cacheError(message: string, context?: CacheError['context']): CacheError

Creates a cache-specific error.

```typescript
const error = cacheError('Cache operation failed', {
  operation: 'get',
  key: 'user:123',
  backend: 'redis'
})
```

## Usage Examples

### Basic Cache Operations

```typescript
import { createMemoryCache } from '@qi/core'

const cache = createMemoryCache({
  maxSize: 1000,
  defaultTtl: 300
})

// Set value with TTL
const setResult = await cache.set('user:123', { id: '123', name: 'John' }, 300)
if (setResult.tag === 'success') {
  console.log('Value cached successfully')
}

// Get value
const getResult = await cache.get<User>('user:123')
if (getResult.tag === 'success') {
  console.log('User:', getResult.value)
} else {
  console.log('Cache miss or error:', getResult.error.message)
}

// Check if key exists
const hasResult = await cache.has('user:123')
if (hasResult.tag === 'success' && hasResult.value) {
  console.log('Key exists in cache')
}

// Delete key
const deleteResult = await cache.delete('user:123')
if (deleteResult.tag === 'success') {
  console.log('Key deleted:', deleteResult.value) // true if existed, false otherwise
}
```

### Batch Operations

```typescript
const cache = createMemoryCache({ maxSize: 1000 })

// Set multiple values
const entries = {
  'score:user1': 100,
  'score:user2': 85,
  'score:user3': 92
}

const msetResult = await cache.mset(entries, 300)
if (msetResult.tag === 'success') {
  console.log('All scores cached')
}

// Get multiple values
const mgetResult = await cache.mget<number>(['score:user1', 'score:user2', 'score:user3'])
if (mgetResult.tag === 'success') {
  console.log('Scores:', mgetResult.value)
  // Output: { 'score:user1': 100, 'score:user2': 85, 'score:user3': 92 }
}

// Delete multiple keys
const mdeleteResult = await cache.mdelete(['score:user1', 'score:user2'])
if (mdeleteResult.tag === 'success') {
  console.log(`Deleted ${mdeleteResult.value} keys`)
}
```

### Cache-Aside Pattern

```typescript
import { createMemoryCache, cacheAside } from '@qi/core'

const cache = createMemoryCache<User>({ defaultTtl: 300 })

// Manual cache-aside implementation
async function getUser(id: string): Promise<Result<User, CacheError>> {
  // Try cache first
  const cached = await cache.get(`user:${id}`)
  if (cached.tag === 'success') {
    return cached
  }

  // Cache miss - load from database
  const user = await loadUserFromDatabase(id)
  
  // Cache the result
  await cache.set(`user:${id}`, user, 300)
  
  return success(user)
}

// Using utility function
const userResult = await cacheAside(
  `user:${id}`,
  cache,
  () => loadUserFromDatabase(id),
  300
)
```

### Redis Cache with Advanced Features

```typescript
import { createRedisCache } from '@qi/core'

const cache = createRedisCache({
  redis: {
    host: 'localhost',
    port: 6379,
    password: process.env.REDIS_PASSWORD,
    connectTimeout: 10000,
    commandTimeout: 5000,
    maxRetriesPerRequest: 3
  }
})

// Set value with TTL
await cache.set('session:abc123', sessionData, 3600)

// Check TTL (Redis-specific)
const ttlResult = await cache.ttl('session:abc123')
if (ttlResult.tag === 'success') {
  console.log(`Key expires in ${ttlResult.value} seconds`)
}

// Update TTL (Redis-specific)
const expireResult = await cache.expire('session:abc123', 7200)
if (expireResult.tag === 'success' && expireResult.value) {
  console.log('TTL updated successfully')
}

// Pattern-based key search
const keysResult = await cache.keys('session:*')
if (keysResult.tag === 'success') {
  console.log('Active sessions:', keysResult.value)
}

// Always close Redis connection
await cache.close()
```

### GetOrSet Pattern

```typescript
const cache = createMemoryCache<UserProfile>({ defaultTtl: 300 })

// Atomic get-or-compute operation
const profileResult = await cache.getOrSet(
  `profile:${userId}`,
  async () => {
    const profile = await loadUserProfile(userId)
    return success(profile)
  },
  600 // 10 minutes TTL
)

if (profileResult.tag === 'success') {
  console.log('Profile:', profileResult.value)
}
```

### Cache Statistics and Monitoring

```typescript
const cache = createMemoryCache<string>({ maxSize: 100 })

// Perform some operations
await cache.set('key1', 'value1')
await cache.set('key2', 'value2')
await cache.get('key1') // hit
await cache.get('key3') // miss

// Get performance statistics
const stats = cache.getStats()
console.log('Cache Statistics:')
console.log(`  Hits: ${stats.hits}`)
console.log(`  Misses: ${stats.misses}`)
console.log(`  Hit Rate: ${(stats.hits / (stats.hits + stats.misses) * 100).toFixed(2)}%`)
console.log(`  Sets: ${stats.sets}`)
console.log(`  Deletes: ${stats.deletes}`)
console.log(`  Evictions: ${stats.evictions}`)
console.log(`  Current Size: ${stats.size}`)
console.log(`  Max Size: ${stats.maxSize}`)
```

### Event Monitoring

```typescript
const cache = createMemoryCache<any>({ maxSize: 10 })

// Monitor cache events
cache.on('hit', (key, value) => {
  console.log(`Cache hit for key: ${key}`)
})

cache.on('miss', (key) => {
  console.log(`Cache miss for key: ${key}`)
})

cache.on('evict', (key, reason) => {
  console.log(`Key evicted: ${key}, reason: ${reason}`)
})

cache.on('error', (error) => {
  console.error('Cache error:', error.message)
})

// Perform operations to trigger events
await cache.set('test', 'value')
await cache.get('test') // triggers 'hit'
await cache.get('missing') // triggers 'miss'
```

### Memory Cache with LRU Eviction

```typescript
const cache = createMemoryCache<string>({
  maxSize: 3, // Small size to demonstrate LRU
  defaultTtl: 300
})

// Fill cache
await cache.set('key1', 'value1')
await cache.set('key2', 'value2')
await cache.set('key3', 'value3')

// Access key1 to make it recently used
await cache.get('key1')

// Add another item - should evict key2 (least recently used)
await cache.set('key4', 'value4')

// Check what's still in cache
const key1 = await cache.get('key1') // Should exist
const key2 = await cache.get('key2') // Should be evicted
const key3 = await cache.get('key3') // Should exist
const key4 = await cache.get('key4') // Should exist
```

### Error Handling with Result<T>

```typescript
import { match } from '@qi/base'

const cache = createMemoryCache<User>()

const result = await cache.get('user:123')

// Pattern matching for error handling
const user = match(
  (user: User) => {
    console.log('Found user:', user.name)
    return user
  },
  (error: CacheError) => {
    console.error('Cache error:', error.message)
    console.error('Operation:', error.context.operation)
    console.error('Key:', error.context.key)
    return null
  },
  result
)
```

### TypeScript Type Safety

```typescript
interface Product {
  id: string
  name: string
  price: number
  category: string
}

// Type-safe cache usage
const productCache = createMemoryCache<Product>({ defaultTtl: 600 })

// TypeScript ensures type safety
await productCache.set('product:123', {
  id: '123',
  name: 'Laptop',
  price: 999.99,
  category: 'Electronics'
})

const productResult = await productCache.get('product:123')
if (productResult.tag === 'success') {
  // productResult.value is typed as Product
  console.log(`Product: ${productResult.value.name} - $${productResult.value.price}`)
}
```

## Performance Characteristics

### Memory Cache
- **Get/Set Operations**: O(1) average case using JavaScript Map
- **LRU Eviction**: O(1) using access order tracking with array manipulation
- **Memory Usage**: Scales linearly with cache size, bounded by maxSize
- **TTL Cleanup**: Lazy expiration checking during operations (no background timers)

### Redis Cache
- **Network Latency**: All operations affected by network round-trip time
- **Connection Pooling**: ioredis provides efficient connection management
- **Batch Operations**: mget/mset use Redis pipelining for better performance
- **TTL Management**: Redis native TTL for automatic expiration

## Best Practices

1. **Backend Selection**: Use memory cache for speed, Redis for persistence/distribution
2. **TTL Strategy**: Set appropriate TTL based on data freshness requirements
3. **Batch Operations**: Use mget/mset/mdelete for multiple keys to reduce round trips
4. **Monitor Performance**: Track hit rates and cache utilization using getStats()
5. **Graceful Degradation**: Handle cache failures gracefully - cache should enhance, not break functionality
6. **Type Safety**: Use TypeScript generics for type-safe cache operations
7. **Resource Management**: Always close Redis connections in production
8. **Key Naming**: Use consistent, hierarchical key patterns (e.g., 'user:123', 'session:abc')
9. **Memory Limits**: Set maxSize for memory cache to prevent unbounded growth
10. **Error Handling**: Use Result<T> pattern for consistent error handling

## Integration with Result<T>

All cache operations return `Promise<Result<T, CacheError>>` for consistent error handling:

```typescript
import { flatMap, map } from '@qi/base'

const cache = createMemoryCache<string>()

// Compose cache operations with Result<T> combinators
const processUser = async (userId: string, userData: string) => {
  const setResult = await cache.set(`user:${userId}`, userData)
  
  return flatMap(
    async () => {
      const getResult = await cache.get(`user:${userId}`)
      return map(
        (data: string) => `Processed: ${data}`,
        getResult
      )
    },
    setResult
  )
}
```

## Contract Compliance

This implementation follows the QiCore Cache contracts with platform-specific adaptations:

- **Async Interface**: All operations return `Promise<Result<T>>` for Redis compatibility
- **LRU Semantics**: Memory cache implements least-recently-used eviction
- **TTL Semantics**: Both backends support time-to-live expiration
- **Performance Guarantees**: O(1) operations with bounded memory usage
- **Result<T> Integration**: Consistent error handling across all operations

The TypeScript implementation adapts the pure functional contracts to async patterns while maintaining all mathematical properties and behavioral requirements.