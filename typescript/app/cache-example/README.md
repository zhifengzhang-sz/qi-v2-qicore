# Cache Example

Demonstrates QiCore Foundation cache usage patterns with both memory and Redis backends.

## Features Demonstrated

- **Memory Cache**: Fast in-memory caching with TTL and LRU eviction
- **Redis Cache**: Persistent caching with Redis backend
- **Cache Patterns**: Cache-aside pattern for expensive operations
- **TTL Management**: Automatic expiration of cached values
- **Multi-operations**: Batch get/set operations
- **Error Handling**: Proper Result<T> patterns for cache operations
- **Statistics**: Cache hit/miss monitoring

## Running the Example

```bash
# Basic example (memory cache only)
bun run dev

# With Redis (start Redis first)
docker run -p 6379:6379 redis:alpine
bun run dev
```

## Key Patterns Shown

### 1. Cache-Aside Pattern
```typescript
async function getUserWithCache(id: string) {
  // Try cache first
  const cachedResult = await cache.get(`user:${id}`)
  if (cachedResult.tag === 'success') {
    return cachedResult.value
  }
  
  // Cache miss - fetch from source
  const user = await expensiveUserLookup(id)
  
  // Store in cache
  await cache.set(`user:${id}`, user, 60)
  
  return user
}
```

### 2. TTL Management
```typescript
// Set with specific TTL
await cache.set('temp:data', value, 60) // 60 seconds

// Check expiration
const result = await cache.get('temp:data')
match(
  data => console.log('Found:', data),
  error => console.log('Expired or not found'),
  result
)
```

### 3. Error Handling
```typescript
const result = await cache.get('key')
match(
  value => {
    // Handle success
    processValue(value)
  },
  error => {
    // Handle cache miss or error
    if (error.code === 'CACHE_MISS') {
      fetchFromSource()
    } else {
      handleCacheError(error)
    }
  },
  result
)
```

## Cache Backends

### Memory Cache
- Fast in-memory storage
- LRU eviction when full
- TTL support
- Process-bound (not shared)

### Redis Cache
- Persistent storage
- Shared across processes
- Advanced data structures
- Clustering support

## Performance Monitoring

Both caches provide statistics:
- Hits/misses
- Set/delete operations
- Current size
- Eviction count