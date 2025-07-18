# Cache API Reference

The Cache module provides high-performance caching with memory and Redis backends, TTL support, and statistics.

## Core Interface

### ICache<T>

Common interface for all cache implementations.

```typescript
interface ICache {
  get<T>(key: string): Promise<Result<T, CacheError>>
  set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  delete(key: string): Promise<Result<boolean, CacheError>>
  exists(key: string): Promise<Result<boolean, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  
  // Batch operations
  mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  mdelete(keys: string[]): Promise<Result<number, CacheError>>
  
  // Statistics and management
  getStats(): CacheStats
  close(): Promise<void>
}
```

## Core Classes

### MemoryCache<T>

In-memory cache with LRU eviction and TTL support.

```typescript
class MemoryCache<T = unknown> implements ICache<T> {
  constructor(config: MemoryCacheConfig)
  
  // Implements all ICache methods
  get(key: string): Promise<Result<T, CacheError>>
  set(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  // ... other methods
}
```

### RedisCache<T>

Redis-backed cache with connection pooling and reliability features.

```typescript
class RedisCache<T = unknown> implements ICache<T> {
  constructor(config: RedisCacheConfig)
  
  // Implements all ICache methods
  get(key: string): Promise<Result<T, CacheError>>
  set(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  // ... other methods
  
  close(): Promise<Result<void, CacheError>>
}
```

## Type Definitions

```typescript
type CacheBackend = 'memory' | 'redis'

interface CacheEntry<T> {
  value: T
  timestamp: number
  ttl?: number
  hits: number
}

interface CacheConfig {
  backend: CacheBackend
  maxSize?: number
  defaultTtl?: number
  keyPrefix?: string
  redis?: {
    host: string
    port: number
    password?: string
    db?: number
    connectTimeout?: number
    commandTimeout?: number
    retryDelayOnFailover?: number
    maxRetriesPerRequest?: number
  }
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
  hit: (key: string) => void
  miss: (key: string) => void
  set: (key: string, ttl?: number) => void
  delete: (key: string) => void
  evict: (key: string, reason: 'ttl' | 'lru' | 'manual') => void
  error: (error: CacheError) => void
}

interface CacheError extends QiError {
  category: 'NETWORK' | 'SYSTEM' | 'CONFIGURATION'
  context: {
    backend?: CacheBackend
    key?: string
    operation?: string
    redisError?: string
  }
}
```

## Factory Functions

### createCache<T>(config: CacheConfig): Result<ICache<T>, CacheError>

Creates a cache instance based on the configuration.

```typescript
// Memory cache
const cache = createCache<User>({
  backend: 'memory',
  maxSize: 1000,
  defaultTtl: 300
})

// Redis cache
const cache = createCache<User>({
  backend: 'redis',
  redis: {
    host: 'localhost',
    port: 6379
  }
})
```

### createMemoryCache<T>(config?: MemoryCacheConfig): Result<MemoryCache<T>, CacheError>

Creates a memory cache instance.

```typescript
const cache = createMemoryCache<string>({
  maxSize: 500,
  defaultTtl: 60
})
```

### createRedisCache<T>(config: RedisCacheConfig): Promise<Result<RedisCache<T>, CacheError>>

Creates a Redis cache instance with connection validation.

```typescript
const cacheResult = await createRedisCache<User>({
  redis: {
    host: 'redis.example.com',
    port: 6379,
    password: 'secret'
  }
})
```

## Utility Functions

### cacheAside<T, K>(cache: ICache<T>, key: string, loader: () => Promise<T>, ttl?: number): Promise<Result<T, CacheError>>

Implements cache-aside pattern with automatic loading.

```typescript
const user = await cacheAside(
  cache,
  `user:${id}`,
  () => database.findUser(id),
  300 // 5 minutes TTL
)
```

## Error Factory

### cacheError(message: string, context?: Record<string, unknown>): CacheError

Creates a cache-specific error.

```typescript
const error = cacheError('Redis connection failed', {
  backend: 'redis',
  host: 'localhost',
  port: 6379
})
```

## Usage Examples

### Basic Cache Operations

```typescript
import { createCache } from '@qi/qicore-foundation'

const cacheResult = createCache<string>({
  backend: 'memory',
  maxSize: 1000
})

if (cacheResult.tag === 'success') {
  const cache = cacheResult.value
  
  // Set value with TTL
  await cache.set('user:123', 'John Doe', 300)
  
  // Get value
  const result = await cache.get('user:123')
  if (result.tag === 'success') {
    console.log('User:', result.value)
  }
  
  // Check if key exists
  const exists = await cache.exists('user:123')
  
  // Delete key
  await cache.delete('user:123')
}
```

### Batch Operations

```typescript
const cache = createMemoryCache<number>().value

// Set multiple values
await cache.mset({
  'score:user1': 100,
  'score:user2': 85,
  'score:user3': 92
}, 300)

// Get multiple values
const scores = await cache.mget(['score:user1', 'score:user2', 'score:user3'])
if (scores.tag === 'success') {
  console.log('Scores:', scores.value)
  // { 'score:user1': 100, 'score:user2': 85, 'score:user3': 92 }
}

// Delete multiple keys
await cache.mdelete(['score:user1', 'score:user2'])
```

### Cache-Aside Pattern

```typescript
async function getUser(id: string): Promise<Result<User, UserError>> {
  const cache = createMemoryCache<User>().value
  
  // Try cache first
  const cached = await cache.get(`user:${id}`)
  if (cached.tag === 'success') {
    return success(cached.value)
  }
  
  // Cache miss - load from database
  const userResult = await database.findUser(id)
  if (userResult.tag === 'success') {
    // Cache the result
    await cache.set(`user:${id}`, userResult.value, 300)
  }
  
  return userResult
}

// Or use the utility function
async function getUserWithUtility(id: string): Promise<Result<User, CacheError>> {
  const cache = createMemoryCache<User>().value
  
  return cacheAside(
    cache,
    `user:${id}`,
    () => database.findUser(id).then(r => r.tag === 'success' ? r.value : Promise.reject(r.error)),
    300
  )
}
```

### Redis Cache with Connection Handling

```typescript
const cacheResult = await createRedisCache<Product>({
  redis: {
    host: 'redis.example.com',
    port: 6379,
    password: process.env.REDIS_PASSWORD,
    connectTimeout: 10000,
    commandTimeout: 5000,
    maxRetriesPerRequest: 3
  }
})

if (cacheResult.tag === 'success') {
  const cache = cacheResult.value
  
  try {
    await cache.set('product:123', product, 600)
    
    const result = await cache.get('product:123')
    match(
      product => console.log('Product:', product),
      error => console.error('Cache error:', error.message),
      result
    )
  } finally {
    // Always close Redis connection
    await cache.close()
  }
}
```

### Cache Statistics and Monitoring

```typescript
const cache = createMemoryCache<string>().value

// Perform operations
await cache.set('key1', 'value1')
await cache.set('key2', 'value2')
await cache.get('key1') // hit
await cache.get('key3') // miss

// Get performance statistics
const stats = cache.getStats()
const hitRate = stats.hits / (stats.hits + stats.misses) * 100
console.log(`Hit rate: ${hitRate.toFixed(2)}%`)
console.log(`Total operations: ${stats.hits + stats.misses}`)
console.log(`Cache size: ${stats.size}`)
console.log(`Total sets: ${stats.sets}`)
console.log(`Total deletes: ${stats.deletes}`)
console.log(`Total evictions: ${stats.evictions}`)
```

### Advanced Redis Configuration

```typescript
const cache = await createRedisCache<SessionData>({
  keyPrefix: 'session:',
  defaultTtl: 3600, // 1 hour
  redis: {
    host: 'redis-cluster.example.com',
    port: 6379,
    password: process.env.REDIS_PASSWORD,
    db: 2, // Use database 2
    connectTimeout: 10000,
    commandTimeout: 5000,
    retryDelayOnFailover: 100,
    maxRetriesPerRequest: 3
  }
})

if (cache.tag === 'success') {
  const redisCache = cache.value
  
  // Keys will be prefixed automatically
  await redisCache.set('user123', sessionData) // Actual key: "session:user123"
}
```

### Memory Cache with LRU Eviction

```typescript
const cache = createMemoryCache<Buffer>({
  maxSize: 100, // Maximum 100 entries
  defaultTtl: 300 // 5 minutes default
}).value

// Fill cache beyond capacity
for (let i = 0; i < 150; i++) {
  await cache.set(`key${i}`, Buffer.from(`value${i}`))
}

// Only the last 100 entries will be kept (LRU eviction)
const size = await cache.size()
console.log(`Cache size: ${size}`) // 100
```

### Error Handling Patterns

```typescript
const cache = createMemoryCache<User>().value

const result = await cache.get('user:123')

match(
  user => {
    console.log('Found user:', user.name)
    return user
  },
  error => {
    if (error.category === 'NETWORK') {
      console.log('Network error, will retry')
      // Implement retry logic
    } else {
      console.log('Cache error:', error.message)
    }
    return null
  },
  result
)
```

### Typed Cache Usage

```typescript
interface UserProfile {
  id: string
  name: string
  email: string
  preferences: Record<string, unknown>
}

const userCache = createMemoryCache<UserProfile>().value

// TypeScript ensures type safety
await userCache.set('profile:123', {
  id: '123',
  name: 'John Doe',
  email: 'john@example.com',
  preferences: { theme: 'dark' }
})

const profile = await userCache.get('profile:123')
if (profile.tag === 'success') {
  // profile.value is typed as UserProfile
  console.log(`Welcome ${profile.value.name}`)
}
```

## Performance Considerations

### Memory Cache
- O(1) average case for get/set operations
- LRU eviction is O(1) due to doubly-linked list implementation
- Memory usage scales linearly with cache size
- TTL cleanup happens lazily during operations

### Redis Cache
- Network latency affects all operations
- Connection pooling reduces overhead
- Batch operations (mget/mset) are more efficient than multiple single operations
- Redis pipeline commands can be used for better performance

## Best Practices

1. **Choose appropriate backend**: Memory for speed, Redis for persistence/distribution
2. **Set reasonable TTLs**: Balance freshness with performance
3. **Use batch operations**: mget/mset for multiple keys
4. **Monitor cache stats**: Track hit rates and adjust accordingly
5. **Handle cache failures gracefully**: Cache should enhance, not break functionality
6. **Use typed caches**: Leverage TypeScript for type safety
7. **Close Redis connections**: Always close connections in production
8. **Use cache-aside pattern**: Let cache complement, not replace, your data layer
9. **Set cache size limits**: Prevent unbounded memory growth
10. **Use appropriate key naming**: Clear, consistent key patterns

## Integration with Result<T>

All cache operations return Result<T> for consistent error handling:

```typescript
const cache = createMemoryCache<string>().value

// Cache operations integrate with Result<T> patterns
const processWithCache = async (key: string, value: string) => {
  const setResult = await cache.set(key, value)
  
  return flatMap(
    async () => {
      const getResult = await cache.get(key)
      return getResult
    },
    setResult
  )
}
```