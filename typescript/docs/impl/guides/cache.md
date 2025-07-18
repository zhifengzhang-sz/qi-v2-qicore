# Cache Implementation Guide

## Overview

The Cache module provides a unified caching interface with multiple backends (memory and Redis) using functional patterns. It implements Result<T> for consistent error handling and provides high-performance operations with TTL management and LRU eviction.

## Design Approach

**Functional wrapper around Map with TTL support**

### Rationale
- Map provides O(1) operations as required by contracts
- Simple TTL implementation without background timers
- Lazy expiration checking on access
- Clear separation between memory and persistent variants

### Key Decisions
- Cache operations return Result<T> for consistency
- Manual expiration checks (no background timers)
- Redis-backed cache for production scalability  
- LRU through access time tracking
- Async-first interface for Redis compatibility

## Contract Adaptations

### Method Signatures
- **Contract Names**: Implements contract-specified names (`has`, `remove`, `size`)
- **Signature Adaptation**: All methods return `Promise<Result<T>>` vs contract's pure functions
- **Async Necessity**: Redis operations require async interface
- **Batch Operations**: Added `mget`, `mset`, `mdelete` for performance
- **Result Wrapping**: Consistent error handling across all operations

### Platform Adaptations

#### Pure Contract Specifications
```yaml
# Language-agnostic contracts specify:
has: "Key → Cache → Boolean"           # Pure, synchronous
remove: "Key → Cache → Boolean"        # Pure, synchronous  
size: "Cache → Integer"                # Pure, synchronous
```

#### TypeScript Implementation
```typescript
// Platform-adapted signatures:
has(key: string): Promise<Result<boolean, CacheError>>     // Async, wrapped
remove(key: string): Promise<Result<boolean, CacheError>>  // Async, wrapped
size(): Promise<Result<number, CacheError>>                // Async, wrapped
```

#### Rationale for Adaptations
1. **Redis Requirement**: Production caching requires Redis, which is inherently async
2. **Interface Consistency**: All cache methods use same `Promise<Result<T>>` pattern
3. **Error Handling**: Consistent Result<T> wrapping across all operations
4. **Type Safety**: TypeScript async/await patterns with compile-time error visibility

## Complete Cache Interface

```typescript
interface ICache {
  // Core contract methods (adapted)
  get<T>(key: string): Promise<Result<T, CacheError>>
  set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  has(key: string): Promise<Result<boolean, CacheError>>
  remove(key: string): Promise<Result<boolean, CacheError>>
  size(): Promise<Result<number, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  
  // TypeScript extensions
  delete(key: string): Promise<Result<boolean, CacheError>>  // Alias for remove
  exists(key: string): Promise<Result<boolean, CacheError>>  // Alias for has
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  
  // Performance extensions
  mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  mdelete(keys: string[]): Promise<Result<number, CacheError>>
  getOrSet<T>(key: string, factory: () => Promise<Result<T>>, ttl?: number): Promise<Result<T, CacheError>>
  
  // Redis-specific extensions
  ttl(key: string): Promise<Result<number, CacheError>>      // Get remaining TTL in seconds
  expire(key: string, seconds: number): Promise<Result<boolean, CacheError>>  // Set TTL on existing key
  
  // Management
  getStats(): CacheStats
  close(): Promise<void>
}
```

## Factory Functions

### Core Factories
- `createCache(config: CacheConfig): ICache` - Create cache based on backend configuration
- `createMemoryCache(config: Omit<CacheConfig, 'backend'>): MemoryCache` - Create memory cache
- `createRedisCache(config: Omit<CacheConfig, 'backend'>): RedisCache` - Create Redis cache
- `createPersistent(filePath: string, config): Result<RedisCache, CacheError>` - Create persistent cache

### Usage Examples

```typescript
// Memory cache - fast, local, temporary
const memoryCache = createMemoryCache({
  maxSize: 1000,
  defaultTtl: 300
})

// Redis cache - persistent, distributed, shared
const redisCache = createRedisCache({
  redis: {
    host: 'localhost',
    port: 6379
  }
})

// Persistent cache with file-based configuration
const persistentResult = createPersistent('/tmp/cache', {
  defaultTtl: 3600
})
```

## Backend Implementations

### Memory Cache
- **Storage**: JavaScript Map with metadata tracking
- **LRU Implementation**: Manual access order tracking with array
- **TTL Management**: Manual expiration checking on access
- **Performance**: O(1) operations, bounded memory usage
- **Use Cases**: Fast local caching, temporary data, development

### Redis Cache
- **Storage**: Redis database with native TTL support
- **Implementation**: 70% ioredis package, 30% custom wrapper
- **Performance**: Leverages Redis optimizations and pipelining
- **TTL Management**: Redis native TTL for automatic expiration
- **Use Cases**: Production caching, distributed systems, persistence

## Extended Methods

### Redis-Specific Methods

#### `ttl(key: string): Promise<Result<number, CacheError>>`
Get the remaining time-to-live (TTL) for a key in seconds.

**Usage:**
```typescript
const cache = createRedisCache({ redis: { host: 'localhost' } })
await cache.set('user:123', userData, 300) // 5 minutes TTL

const ttlResult = await cache.ttl('user:123')
if (ttlResult.tag === 'success') {
  console.log(`Key expires in ${ttlResult.value} seconds`)
}
```

**Returns:**
- Positive number: Remaining TTL in seconds
- 0: Key exists but has no TTL (permanent)
- -1: Key does not exist
- -2: Key exists but has no associated TTL

#### `expire(key: string, seconds: number): Promise<Result<boolean, CacheError>>`
Set or update the TTL for an existing key.

**Usage:**
```typescript
// Set TTL on existing key
await cache.set('session:abc', sessionData) // No initial TTL
await cache.expire('session:abc', 1800) // Set 30 minutes TTL

// Update TTL for existing key
await cache.expire('user:123', 600) // Extend to 10 minutes
```

**Returns:**
- `true`: TTL was successfully set
- `false`: Key does not exist

### TypeScript Convenience Methods

#### `exists(key: string): Promise<Result<boolean, CacheError>>`
Alias for the `has()` method providing alternative naming convention.

**Usage:**
```typescript
// Both methods are identical in behavior
const existsResult = await cache.exists('user:123')
const hasResult = await cache.has('user:123')

if (existsResult.tag === 'success' && existsResult.value) {
  console.log('Key exists in cache')
}
```

**Rationale:**
- Provides familiar naming for developers coming from Redis background
- Maintains consistency with Redis command naming
- Zero-cost abstraction (direct alias to `has` method)

### Advanced TTL Operations

#### Combined TTL Management
```typescript
// Check if key exists and get its TTL
const key = 'session:user123'
const existsResult = await cache.exists(key)

if (existsResult.tag === 'success' && existsResult.value) {
  const ttlResult = await cache.ttl(key)
  
  if (ttlResult.tag === 'success') {
    if (ttlResult.value < 300) { // Less than 5 minutes
      await cache.expire(key, 1800) // Extend to 30 minutes
      console.log('Session TTL extended')
    }
  }
}
```

#### Conditional TTL Setting
```typescript
// Set TTL only if key exists without one
const setConditionalTtl = async (key: string, ttl: number) => {
  const ttlResult = await cache.ttl(key)
  
  if (ttlResult.tag === 'success' && ttlResult.value === 0) {
    // Key exists but has no TTL
    await cache.expire(key, ttl)
  }
}
```

## Utility Functions

### Cache-Aside Pattern
```typescript
export const cacheAside = async <T>(
  key: string,
  cache: ICache,
  loader: () => Promise<T>,
  ttl?: number
): Promise<Result<T, CacheError>>
```

**Usage:**
```typescript
const userData = await cacheAside(
  `user:${userId}`,
  cache,
  () => fetchUserFromDatabase(userId),
  300 // 5 minutes TTL
)
```

## Performance Characteristics

### Memory Usage
- No background timers (check on access)
- Bounded memory through maxSize configuration
- LRU eviction through access time tracking
- Efficient object reuse and pooling

### Runtime Performance
- **O(1) Operations**: Cache get/set operations
- **Memory Efficient**: Object pooling for frequent allocations
- **V8 Optimized**: Consistent object shapes for JIT compilation
- **Async Friendly**: Non-blocking operations throughout

## Error Handling

### Error Types
```typescript
export type CacheError = QiError & {
  readonly category: 'RESOURCE'
  readonly context: {
    readonly operation?: string
    readonly key?: string
    readonly backend?: CacheBackend
    readonly cache?: string
  }
}
```

### Error Categories
- **RESOURCE**: All cache errors use RESOURCE category
- **Context**: Operation, key, backend, and cache name included
- **Graceful Degradation**: Cache failures don't break application flow

## Event System

### Supported Events
```typescript
export interface CacheEvents {
  hit: (key: string, value: unknown) => void
  miss: (key: string) => void
  set: (key: string, value: unknown, ttl?: number) => void
  delete: (key: string) => void
  evict: (key: string, reason: 'lru' | 'ttl' | 'manual') => void
  error: (error: CacheError) => void
  connect: () => void
  disconnect: () => void
}
```

### Usage
```typescript
cache.on('hit', (key, value) => {
  console.log(`Cache hit for key: ${key}`)
})

cache.on('miss', (key) => {
  console.log(`Cache miss for key: ${key}`)
})
```

## Statistics and Monitoring

### Cache Statistics
```typescript
export interface CacheStats {
  readonly hits: number
  readonly misses: number
  readonly sets: number
  readonly deletes: number
  readonly evictions: number
  readonly size: number
  readonly maxSize?: number
}
```

### Monitoring Usage
```typescript
const stats = cache.getStats()
console.log(`Hit rate: ${stats.hits / (stats.hits + stats.misses) * 100}%`)
console.log(`Cache utilization: ${stats.size} / ${stats.maxSize}`)
```

## Configuration

### Cache Configuration
```typescript
export interface CacheConfig {
  readonly backend: CacheBackend
  readonly maxSize?: number
  readonly defaultTtl?: number
  readonly redis?: RedisOptions
  readonly name?: string
}
```

### Redis Configuration
Supports all ioredis configuration options:
- Host, port, password
- Connection pooling
- Retry strategies
- Clustering support
- SSL/TLS configuration

## Testing and Validation

### Property-Based Testing
- All mathematical laws verified with fast-check
- Minimum 1000 iterations per property
- Contract compliance verification

### Performance Testing
- O(1) operations verified under load
- Memory usage bounds tested
- TTL expiration accuracy validated

## Integration Examples

### Express API Caching
```typescript
app.get('/api/user/:id', async (req, res) => {
  const result = await cache.get<User>(`user:${req.params.id}`)
  
  if (result.tag === 'success') {
    return res.json(result.value)
  }
  
  const user = await fetchUserFromDatabase(req.params.id)
  await cache.set(`user:${req.params.id}`, user, 300)
  res.json(user)
})
```

### Batch Operations
```typescript
// Efficient batch retrieval
const userIds = ['1', '2', '3']
const usersResult = await cache.mget<User>(userIds.map(id => `user:${id}`))

// Efficient batch storage
const userData = {
  'user:1': { id: '1', name: 'John' },
  'user:2': { id: '2', name: 'Jane' }
}
await cache.mset(userData, 300)
```

## Best Practices

### Key Naming
- Use consistent prefixes: `user:${id}`, `session:${token}`
- Include version information: `user:v1:${id}`
- Use meaningful separators: colons for hierarchy

### TTL Strategy
- Set appropriate TTL based on data freshness requirements
- Use shorter TTL for frequently changing data
- Consider cache warming for critical data

### Error Handling
- Always handle cache failures gracefully
- Don't let cache failures break application flow
- Log cache errors for monitoring

### Performance Optimization
- Use batch operations for multiple keys
- Monitor hit rates and adjust TTL accordingly
- Consider cache preloading for predictable access patterns

## Migration from Other Caching Solutions

### From node-cache
```typescript
// Before
const cache = new NodeCache({ stdTTL: 600 })
cache.set('key', value)
const result = cache.get('key')

// After
const cache = createMemoryCache({ defaultTtl: 600 })
await cache.set('key', value)
const result = await cache.get('key')
```

### From Redis directly
```typescript
// Before
await redis.set('key', JSON.stringify(value))
const result = JSON.parse(await redis.get('key'))

// After
const cache = createRedisCache({ redis: { host: 'localhost' } })
await cache.set('key', value)
const result = await cache.get('key')
```

## Troubleshooting

### Common Issues
1. **Memory leaks**: Ensure maxSize is configured for memory cache
2. **Redis connection errors**: Check Redis configuration and network
3. **TTL not working**: Verify TTL values are in seconds, not milliseconds
4. **Performance issues**: Use batch operations and monitor hit rates

### Debug Information
```typescript
// Enable debug logging
cache.on('error', (error) => {
  console.error('Cache error:', error)
})

// Monitor cache statistics
setInterval(() => {
  console.log('Cache stats:', cache.getStats())
}, 10000)
```