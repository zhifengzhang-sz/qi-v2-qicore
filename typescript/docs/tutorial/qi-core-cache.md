# Tool: Cache

## What It Does

Cache provides high-performance caching with memory and Redis backends, TTL support, and statistics. All operations return Result<T> for consistent error handling.

**See [Complete API Documentation](../api/core/cache.md) for all available functions.**

## Why You Need This

When performance matters, caching expensive operations is essential. Cache integrates with Result<T> to handle cache failures gracefully without breaking your application.

```typescript
// Instead of hoping cache works:
let user
try {
  user = await cache.get('user:123')
} catch (error) {
  // What kind of error? How should I handle it?
  user = await database.getUser('123')
}

// Use explicit Result<T> handling:
const cachedUser = await cache.get('user:123')
const user = match(
  cached => cached,
  error => {
    logger.debug('Cache miss', undefined, { userId: '123', reason: error.message })
    return database.getUser('123')  // Fallback to database
  },
  cachedUser
)
```

## Basic Usage

### Import What You Need

```typescript
import { createCache, type ICache, type CacheError } from '@qi/core'
import { match } from '@qi/base'
```

### Create a Cache

```typescript
// Memory cache
const cacheResult = createCache<User>({
  backend: 'memory',
  maxSize: 1000,
  defaultTtl: 300  // 5 minutes
})

// Redis cache
const cacheResult = await createRedisCache<User>({
  redis: {
    host: 'localhost',
    port: 6379
  }
})

match(
  cache => {
    console.log('Cache created successfully')
  },
  error => {
    console.error('Cache creation failed:', error.message)
  },
  cacheResult
)
```

### Basic Operations

```typescript
const cache = createCache<string>({ backend: 'memory' }).value

// Set value with TTL
await cache.set('user:123', 'John Doe', 300)  // 5 minutes

// Get value
const result = await cache.get('user:123')
match(
  value => console.log('User:', value),
  error => console.log('Cache miss:', error.message),
  result
)

// Check if key exists
const exists = await cache.has('user:123')

// Delete key
await cache.delete('user:123')
```

## Cache-Aside Pattern

The most common caching pattern - check cache first, load from source on miss:

```typescript
async function getUser(id: string): Promise<User> {
  const cache = createCache<User>({ backend: 'memory' }).value
  
  // Try cache first
  const cached = await cache.get(`user:${id}`)
  if (cached.tag === 'success') {
    return cached.value
  }
  
  // Cache miss - load from database
  const user = await database.findUser(id)
  
  // Cache the result for next time
  await cache.set(`user:${id}`, user, 300)
  
  return user
}

// Or use the utility function
import { cacheAside } from '@qi/core'

async function getUserWithUtility(id: string): Promise<Result<User, CacheError>> {
  const cache = createCache<User>({ backend: 'memory' }).value
  
  return cacheAside(
    cache,
    `user:${id}`,
    () => database.findUser(id),
    300  // TTL in seconds
  )
}
```

## Batch Operations

Efficiently work with multiple keys at once:

```typescript
const cache = createCache<number>({ backend: 'memory' }).value

// Set multiple values
await cache.mset({
  'score:user1': 100,
  'score:user2': 85,
  'score:user3': 92
}, 300)

// Get multiple values
const scores = await cache.mget(['score:user1', 'score:user2', 'score:user3'])
match(
  values => {
    console.log('Scores:', values)
    // { 'score:user1': 100, 'score:user2': 85, 'score:user3': 92 }
  },
  error => console.log('Failed to get scores:', error.message),
  scores
)

// Delete multiple keys
await cache.mdelete(['score:user1', 'score:user2'])
```

## Redis Configuration

For production applications, Redis provides persistent, distributed caching:

```typescript
const cacheResult = await createRedisCache<Product>({
  redis: {
    host: 'redis.example.com',
    port: 6379,
    password: process.env.REDIS_PASSWORD,
    db: 2,  // Use database 2
    connectTimeout: 10000,
    commandTimeout: 5000,
    maxRetriesPerRequest: 3
  }
})

match(
  cache => {
    console.log('Redis cache ready')
  },
  error => {
    console.error('Redis connection failed:', error.message)
    // Fallback to memory cache or handle gracefully
  },
  cacheResult
)
```

## Performance Monitoring

Track cache performance with built-in statistics:

```typescript
const cache = createCache<string>({ backend: 'memory' }).value

// Perform operations
await cache.set('key1', 'value1')
await cache.set('key2', 'value2')
await cache.get('key1')  // hit
await cache.get('key3')  // miss

// Get performance statistics
const stats = cache.getStats()
console.log(`Hit rate: ${(stats.hitRate * 100).toFixed(2)}%`)
console.log(`Total operations: ${stats.hits + stats.misses}`)
console.log(`Cache size: ${stats.size}`)

// Reset statistics
cache.resetStats()
```

## Real Example

```typescript
import { createCache, createLogger, ConfigBuilder } from '@qi/core'
import { match, flatMap } from '@qi/base'

// Load configuration
const configResult = ConfigBuilder
  .fromYamlFile('./config.yaml')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .build()

match(
  config => {
    // Create logger
    const loggerResult = createLogger({
      level: config.get('logging.level', 'info'),
      name: config.get('app.name', 'app')
    })
    
    match(
      logger => {
        // Create cache based on configuration
        const cacheBackend = config.get('cache.backend', 'memory')
        
        if (cacheBackend === 'redis') {
          setupRedisCache(config, logger)
        } else {
          setupMemoryCache(config, logger)
        }
      },
      error => console.error('Logger creation failed:', error.message),
      loggerResult
    )
  },
  error => console.error('Configuration error:', error.message),
  configResult
)

async function setupRedisCache(config: Config, logger: Logger) {
  const cacheResult = await createRedisCache<any>({
    redis: {
      host: config.get('redis.host'),
      port: config.get('redis.port'),
      password: config.get('redis.password')
    }
  })
  
  match(
    cache => {
      logger.info('Redis cache initialized')
      startApplication(cache, logger)
    },
    error => {
      logger.error('Redis cache failed, falling back to memory', error)
      const memoryCache = createCache({ backend: 'memory' }).value
      startApplication(memoryCache, logger)
    },
    cacheResult
  )
}

function setupMemoryCache(config: Config, logger: Logger) {
  const cache = createCache<any>({
    backend: 'memory',
    maxSize: config.get('cache.maxSize', 1000)
  }).value
  
  logger.info('Memory cache initialized')
  startApplication(cache, logger)
}

async function startApplication(cache: ICache, logger: Logger) {
  // Example service using cache
  class UserService {
    constructor(private cache: ICache, private logger: Logger) {}
    
    async getUser(id: string): Promise<User | null> {
      const requestLogger = this.logger.child({ operation: 'getUser', userId: id })
      
      // Try cache first
      const cached = await this.cache.get(`user:${id}`)
      
      return match(
        user => {
          requestLogger.debug('Cache hit')
          return user
        },
        async error => {
          requestLogger.debug('Cache miss, loading from database')
          
          // Load from database
          const user = await database.findUser(id)
          if (user) {
            // Cache for 5 minutes
            await this.cache.set(`user:${id}`, user, 300)
            requestLogger.debug('User cached')
          }
          
          return user
        },
        cached
      )
    }
    
    async updateUser(id: string, updates: Partial<User>): Promise<Result<User, UserError>> {
      const requestLogger = this.logger.child({ operation: 'updateUser', userId: id })
      
      const result = await database.updateUser(id, updates)
      
      match(
        user => {
          // Invalidate cache on successful update
          this.cache.delete(`user:${id}`)
          requestLogger.info('User updated and cache invalidated')
        },
        error => {
          requestLogger.error('User update failed', error)
        },
        result
      )
      
      return result
    }
  }
  
  const userService = new UserService(cache, logger)
  
  // Monitor cache performance
  setInterval(() => {
    const stats = cache.getStats()
    logger.info('Cache performance', undefined, {
      hitRate: stats.hitRate,
      hits: stats.hits,
      misses: stats.misses,
      size: stats.size
    })
  }, 60000)  // Every minute
}
```

## Error Handling Strategies

```typescript
const cache = createCache<User>({ backend: 'memory' }).value

async function getCachedUser(id: string): Promise<User | null> {
  const result = await cache.get(`user:${id}`)
  
  return match(
    user => user,
    error => {
      // Handle different error types
      switch (error.category) {
        case 'NETWORK':
          logger.warn('Cache network error, will retry', error)
          // Could implement retry logic here
          return null
          
        case 'SYSTEM':
          logger.error('Cache system error', error)
          // Could fallback to alternative cache or disable caching
          return null
          
        default:
          logger.debug('Cache miss', undefined, { userId: id })
          return null
      }
    },
    result
  )
}
```

## Integration with Other Tools

Cache works seamlessly with Config and Logger:

```typescript
// Configure cache from Config
const cache = createCache({
  backend: config.get('cache.backend'),
  maxSize: config.get('cache.maxSize'),
  redis: config.has('redis') ? {
    host: config.get('redis.host'),
    port: config.get('redis.port')
  } : undefined
}).value

// Log cache operations
async function cachedOperation<T>(
  cache: ICache<T>,
  key: string, 
  loader: () => Promise<T>,
  ttl: number,
  logger: Logger
): Promise<T> {
  const opLogger = logger.child({ operation: 'cachedOperation', key })
  
  const cached = await cache.get(key)
  
  return match(
    value => {
      opLogger.debug('Cache hit')
      return value
    },
    async error => {
      opLogger.debug('Cache miss, loading')
      const value = await loader()
      await cache.set(key, value, ttl)
      opLogger.debug('Value cached')
      return value
    },
    cached
  )
}
```

## Key Benefits

1. **Multiple backends** - Memory for speed, Redis for persistence/distribution
2. **Graceful failure** - Cache failures don't break your application
3. **Performance monitoring** - Built-in statistics and hit rate tracking
4. **Type safety** - Generic cache with TypeScript support
5. **Result<T> integration** - Consistent error handling with other tools

## Working Example

Try the complete working example:

```bash
# Start Redis (optional)
docker run -p 6379:6379 redis:alpine

# Run cache example
cd typescript/app/cache-example
bun run dev
```

This shows memory and Redis caching with performance monitoring and cache-aside patterns.

## Next Steps

- [Config Tool](./qi-core-config.md) - Use config to set up cache backends
- [Logger Tool](./qi-core-logger.md) - Log cache performance and operations
- [Back to qi/base](./qi-base.md) - Review the framework patterns

You now understand all components of QiCore Foundation: the framework (qi/base) and the three tools (Config, Logger, Cache) that use it consistently.