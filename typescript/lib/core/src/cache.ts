/**
 * QiCore Foundation - Cache Module
 *
 * Unified caching interface with ioredis and memory backends using functional patterns.
 * Provides high-performance caching with TTL management, LRU eviction, and
 * Redis pipeline operations. Max-Min principle: 70% ioredis package, 30% custom logic.
 */

import {
  type Result,
  type QiError,
  success,
  failure,
  create as createError,
  fromAsyncTryCatch,
} from '@qi/base'
import { EventEmitter } from 'eventemitter3'
import { Redis, type RedisOptions } from 'ioredis'

// ============================================================================
// Core Types
// ============================================================================

/**
 * Cache backend types
 */
export type CacheBackend = 'memory' | 'redis'

/**
 * Cache entry with metadata
 */
export interface CacheEntry<T = unknown> {
  readonly value: T
  readonly createdAt: Date
  readonly expiresAt?: Date
  readonly accessCount: number
  readonly lastAccessed: Date
}

/**
 * Cache configuration
 */
export interface CacheConfig {
  readonly backend: CacheBackend
  readonly maxSize?: number
  readonly defaultTtl?: number
  readonly redis?: RedisOptions
  readonly name?: string
}

/**
 * Cache statistics
 */
export interface CacheStats {
  readonly hits: number
  readonly misses: number
  readonly sets: number
  readonly deletes: number
  readonly evictions: number
  readonly size: number
  readonly maxSize?: number
}

/**
 * Cache events
 */
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

// ============================================================================
// Error Types
// ============================================================================

/**
 * Cache-specific error types
 */
export type CacheError = QiError & {
  readonly category: 'RESOURCE'
  readonly context: {
    readonly operation?: string
    readonly key?: string
    readonly backend?: CacheBackend
    readonly cache?: string
  }
}

/**
 * Create cache error
 */
export const cacheError = (message: string, context: CacheError['context'] = {}): CacheError =>
  createError('CACHE_ERROR', message, 'RESOURCE', context) as CacheError

/**
 * Cache-specific async try-catch wrapper that returns CacheError
 */
const fromCacheAsyncTryCatch = async <T>(
  operation: () => Promise<T>,
  errorMapper: (error: unknown) => CacheError
): Promise<Result<T, CacheError>> => {
  try {
    const result = await operation()
    return success(result)
  } catch (error) {
    return failure(errorMapper(error))
  }
}

// ============================================================================
// Cache Interface
// ============================================================================

/**
 * Unified cache interface for both memory and Redis backends
 */
export interface ICache {
  get<T>(key: string): Promise<Result<T, CacheError>>
  set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  delete(key: string): Promise<Result<boolean, CacheError>>
  has(key: string): Promise<Result<boolean, CacheError>>
  remove(key: string): Promise<Result<boolean, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  size(): Promise<Result<number, CacheError>>
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>>
  mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>>
  mdelete(keys: string[]): Promise<Result<number, CacheError>>
  getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>>
  getStats(): CacheStats
  close(): Promise<Result<void, CacheError>>
}

// ============================================================================
// Memory Cache Implementation (30% custom logic)
// ============================================================================

/**
 * In-memory cache with LRU eviction
 */
export class MemoryCache implements ICache {
  private readonly entries = new Map<string, CacheEntry>()
  private readonly events: EventEmitter<CacheEvents>
  private readonly config: CacheConfig
  private readonly accessOrder: string[] = []
  private readonly stats: {
    hits: number
    misses: number
    sets: number
    deletes: number
    evictions: number
    size: number
    maxSize?: number
  }

  constructor(config: CacheConfig) {
    this.config = { ...config }
    this.events = new EventEmitter<CacheEvents>()
    this.stats = {
      hits: 0,
      misses: 0,
      sets: 0,
      deletes: 0,
      evictions: 0,
      size: 0,
      maxSize: config.maxSize,
    }
  }

  async get<T>(key: string): Promise<Result<T, CacheError>> {
    const entry = this.entries.get(key)

    if (!entry) {
      this.stats.misses++
      this.events.emit('miss', key)
      return failure(
        cacheError(`Cache miss for key: ${key}`, {
          operation: 'get',
          key,
          backend: 'memory',
        })
      )
    }

    // Check if expired
    if (entry.expiresAt && entry.expiresAt < new Date()) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      this.stats.evictions++
      this.events.emit('evict', key, 'ttl')
      return failure(
        cacheError(`Cache key expired: ${key}`, {
          operation: 'get',
          key,
          backend: 'memory',
        })
      )
    }

    // Update access info
    const updatedEntry: CacheEntry<T> = {
      value: entry.value as T,
      createdAt: entry.createdAt,
      expiresAt: entry.expiresAt,
      lastAccessed: new Date(),
      accessCount: entry.accessCount + 1,
    }
    this.entries.set(key, updatedEntry)
    this.updateAccessOrder(key, 'access')

    this.stats.hits++
    this.events.emit('hit', key, entry.value)

    return success(entry.value as T)
  }

  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>> {
    const now = new Date()
    const effectiveTtl = ttl ?? this.config.defaultTtl
    const expiresAt = effectiveTtl ? new Date(now.getTime() + effectiveTtl * 1000) : undefined

    const entry: CacheEntry<T> = {
      value,
      createdAt: now,
      expiresAt,
      accessCount: 0,
      lastAccessed: now,
    }

    const wasPresent = this.entries.has(key)
    this.entries.set(key, entry)

    if (!wasPresent) {
      this.stats.size++
      this.updateAccessOrder(key, 'add')

      // Check for eviction
      if (this.config.maxSize && this.stats.size > this.config.maxSize) {
        this.evictLRU()
      }
    } else {
      this.updateAccessOrder(key, 'access')
    }

    this.stats.sets++
    this.events.emit('set', key, value, ttl)

    return success(undefined)
  }

  async delete(key: string): Promise<Result<boolean, CacheError>> {
    const existed = this.entries.has(key)

    if (existed) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      this.stats.deletes++
      this.events.emit('delete', key)
    }

    return success(existed)
  }

  async has(key: string): Promise<Result<boolean, CacheError>> {
    const entry = this.entries.get(key)

    if (!entry) {
      return success(false)
    }

    // Check if expired
    if (entry.expiresAt && entry.expiresAt < new Date()) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      return success(false)
    }

    return success(true)
  }

  async exists(key: string): Promise<Result<boolean, CacheError>> {
    return this.has(key)
  }

  async remove(key: string): Promise<Result<boolean, CacheError>> {
    return this.delete(key)
  }

  async size(): Promise<Result<number, CacheError>> {
    // Clean up expired entries first
    const now = new Date()
    const expiredKeys: string[] = []

    for (const [key, entry] of this.entries.entries()) {
      if (entry.expiresAt && entry.expiresAt < now) {
        expiredKeys.push(key)
      }
    }

    for (const key of expiredKeys) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
    }

    return success(this.stats.size)
  }

  async clear(): Promise<Result<void, CacheError>> {
    this.stats.evictions += this.stats.size
    this.entries.clear()
    this.accessOrder.length = 0
    this.stats.size = 0
    return success(undefined)
  }

  async keys(pattern?: string): Promise<Result<string[], CacheError>> {
    const allKeys = Array.from(this.entries.keys())

    if (!pattern) {
      return success(allKeys)
    }

    // Simple glob pattern matching
    const regex = new RegExp(pattern.replace(/\*/g, '.*').replace(/\?/g, '.'))
    const matchedKeys = allKeys.filter((key) => regex.test(key))

    return success(matchedKeys)
  }

  async mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>> {
    const result: Record<string, T> = {}

    for (const key of keys) {
      const getResult = await this.get<T>(key)
      if (getResult.tag === 'success') {
        result[key] = getResult.value
      }
    }

    return success(result)
  }

  async mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>> {
    for (const [key, value] of Object.entries(entries)) {
      const setResult = await this.set(key, value, ttl)
      if (setResult.tag === 'failure') {
        return setResult
      }
    }

    return success(undefined)
  }

  async mdelete(keys: string[]): Promise<Result<number, CacheError>> {
    let deletedCount = 0

    for (const key of keys) {
      const deleteResult = await this.delete(key)
      if (deleteResult.tag === 'success' && deleteResult.value) {
        deletedCount++
      }
    }

    return success(deletedCount)
  }

  async getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>> {
    // Try to get existing value first
    const getResult = await this.get<T>(key)
    if (getResult.tag === 'success') {
      return getResult
    }

    // Key doesn't exist or expired, call factory function
    const factoryResult = await factory()
    if (factoryResult.tag === 'failure') {
      return factoryResult
    }

    // Cache the factory result
    const setResult = await this.set(key, factoryResult.value, ttl)
    if (setResult.tag === 'failure') {
      return setResult
    }

    return factoryResult
  }

  getStats(): CacheStats {
    return { ...this.stats }
  }

  async close(): Promise<Result<void, CacheError>> {
    return fromCacheAsyncTryCatch(
      async () => {
        this.entries.clear()
        this.events.removeAllListeners()
      },
      (error) =>
        cacheError('Failed to close memory cache', {
          operation: 'close',
          backend: 'memory',
          error: error instanceof Error ? error.message : String(error),
        })
    )
  }

  private updateAccessOrder(key: string, operation: 'add' | 'access' | 'remove'): void {
    const index = this.accessOrder.indexOf(key)

    if (operation === 'remove') {
      if (index > -1) {
        this.accessOrder.splice(index, 1)
      }
    } else {
      // For both 'add' and 'access', move to front
      if (index > -1) {
        this.accessOrder.splice(index, 1)
      }
      this.accessOrder.unshift(key)
    }
  }

  private evictLRU(): void {
    const lruKey = this.accessOrder.pop()
    if (lruKey) {
      this.entries.delete(lruKey)
      this.stats.size--
      this.stats.evictions++
      this.events.emit('evict', lruKey, 'lru')
    }
  }
}

// ============================================================================
// Redis Cache Implementation (70% ioredis package)
// ============================================================================

/**
 * Redis-backed cache leveraging ioredis capabilities
 * Uses Redis native TTL, memory management, and pipelining
 */
export class RedisCache implements ICache {
  private readonly redis: Redis
  private readonly events: EventEmitter<CacheEvents>
  private readonly config: CacheConfig
  private readonly stats: {
    hits: number
    misses: number
    sets: number
    deletes: number
    evictions: number
    size: number
    maxSize?: number
  }

  constructor(config: CacheConfig) {
    this.config = { ...config }
    this.events = new EventEmitter<CacheEvents>()
    this.stats = {
      hits: 0,
      misses: 0,
      sets: 0,
      deletes: 0,
      evictions: 0,
      size: 0,
    }

    // Initialize Redis client with ioredis
    this.redis = new Redis(config.redis || {})

    // Set up event listeners using ioredis events
    this.redis.on('connect', () => {
      this.events.emit('connect')
    })

    this.redis.on('close', () => {
      this.events.emit('disconnect')
    })

    this.redis.on('error', (error: Error) => {
      this.events.emit(
        'error',
        cacheError(`Redis error: ${error.message}`, {
          backend: 'redis',
          cache: this.config.name,
        })
      )
    })
  }

  async get<T>(key: string): Promise<Result<T, CacheError>> {
    const result = await fromAsyncTryCatch(
      async () => {
        // Use Redis GET command via ioredis
        const value = await this.redis.get(key)

        if (value === null) {
          return null // Return null to indicate cache miss
        }

        return JSON.parse(value) as T
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'get',
          key,
          backend: 'redis',
        })
    )

    // Handle cache miss outside of the try/catch wrapper
    if (result.tag === 'success' && result.value === null) {
      this.stats.misses++
      this.events.emit('miss', key)
      return failure(
        cacheError(`Cache miss for key: ${key}`, {
          operation: 'get',
          key,
          backend: 'redis',
        })
      )
    }

    // Handle success case
    if (result.tag === 'success') {
      this.stats.hits++
      this.events.emit('hit', key, result.value)
    }

    return result as Result<T, CacheError>
  }

  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        const serialized = JSON.stringify(value)
        const effectiveTtl = ttl ?? this.config.defaultTtl

        if (effectiveTtl) {
          // Use Redis SETEX for TTL via ioredis
          await this.redis.setex(key, effectiveTtl, serialized)
        } else {
          // Use Redis SET without TTL via ioredis
          await this.redis.set(key, serialized)
        }

        this.stats.sets++
        this.events.emit('set', key, value, ttl)
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'set',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<void, CacheError>>
  }

  async delete(key: string): Promise<Result<boolean, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis DEL command via ioredis
        const result = await this.redis.del(key)
        const existed = result > 0

        if (existed) {
          this.stats.deletes++
          this.events.emit('delete', key)
        }

        return existed
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'delete',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<boolean, CacheError>>
  }

  async has(key: string): Promise<Result<boolean, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis EXISTS command via ioredis
        const result = await this.redis.exists(key)
        return result === 1
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'has',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<boolean, CacheError>>
  }

  async exists(key: string): Promise<Result<boolean, CacheError>> {
    return this.has(key)
  }

  async remove(key: string): Promise<Result<boolean, CacheError>> {
    return this.delete(key)
  }

  async size(): Promise<Result<number, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis DBSIZE command to get total number of keys
        const size = await this.redis.dbsize()
        return size
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'size',
          backend: 'redis',
        })
    ) as Promise<Result<number, CacheError>>
  }

  async clear(): Promise<Result<void, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis FLUSHDB command via ioredis
        await this.redis.flushdb()
        this.stats.evictions += this.stats.size
        this.stats.size = 0
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'clear',
          backend: 'redis',
        })
    ) as Promise<Result<void, CacheError>>
  }

  async keys(pattern = '*'): Promise<Result<string[], CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis KEYS command via ioredis (with pattern support)
        const keys = await this.redis.keys(pattern)
        return keys
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'keys',
          backend: 'redis',
        })
    ) as Promise<Result<string[], CacheError>>
  }

  getStats(): CacheStats {
    return { ...this.stats }
  }

  async close(): Promise<Result<void, CacheError>> {
    return fromCacheAsyncTryCatch(
      async () => {
        await this.redis.quit()
        this.events.removeAllListeners()
      },
      (error) =>
        cacheError('Failed to close Redis cache', {
          operation: 'close',
          backend: 'redis',
          error: error instanceof Error ? error.message : String(error),
        })
    )
  }

  // Redis-specific methods leveraging ioredis capabilities

  async mget<T>(keys: string[]): Promise<Result<Record<string, T>, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis MGET via ioredis pipeline
        const pipeline = this.redis.pipeline()
        for (const key of keys) {
          pipeline.get(key)
        }
        const results = await pipeline.exec()

        if (!results) {
          throw new Error('Pipeline execution failed')
        }

        const result: Record<string, T> = {}

        results.forEach(([error, value], index) => {
          if (error) {
            throw error
          }
          if (value && keys[index]) {
            result[keys[index]] = JSON.parse(value as string) as T
          }
        })

        return result
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'mget',
          backend: 'redis',
        })
    ) as Promise<Result<Record<string, T>, CacheError>>
  }

  async mset<T>(entries: Record<string, T>, ttl?: number): Promise<Result<void, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        const pipeline = this.redis.pipeline()
        const effectiveTtl = ttl ?? this.config.defaultTtl

        for (const [key, value] of Object.entries(entries)) {
          const serialized = JSON.stringify(value)

          if (effectiveTtl) {
            pipeline.setex(key, effectiveTtl, serialized)
          } else {
            pipeline.set(key, serialized)
          }
        }

        const results = await pipeline.exec()

        if (!results) {
          throw new Error('Pipeline execution failed')
        }

        // Check if any command failed
        for (const [error] of results) {
          if (error) {
            throw error
          }
        }

        this.stats.sets += Object.keys(entries).length

        for (const [key, value] of Object.entries(entries)) {
          this.events.emit('set', key, value, ttl)
        }
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'mset',
          backend: 'redis',
        })
    ) as Promise<Result<void, CacheError>>
  }

  async mdelete(keys: string[]): Promise<Result<number, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis DEL command for multiple keys
        const result = await this.redis.del(...keys)

        this.stats.deletes += result

        for (const key of keys.slice(0, result)) {
          this.events.emit('delete', key)
        }

        return result
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'mdelete',
          backend: 'redis',
        })
    ) as Promise<Result<number, CacheError>>
  }

  async getOrSet<T>(
    key: string,
    factory: () => Promise<Result<T, CacheError>>,
    ttl?: number
  ): Promise<Result<T, CacheError>> {
    // Try to get existing value first
    const getResult = await this.get<T>(key)
    if (getResult.tag === 'success') {
      return getResult
    }

    // Key doesn't exist or expired, call factory function
    const factoryResult = await factory()
    if (factoryResult.tag === 'failure') {
      return factoryResult
    }

    // Cache the factory result
    const setResult = await this.set(key, factoryResult.value, ttl)
    if (setResult.tag === 'failure') {
      return setResult
    }

    return factoryResult
  }

  /**
   * Use Redis TTL command to get remaining time
   */
  async ttl(key: string): Promise<Result<number, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis TTL command via ioredis
        const seconds = await this.redis.ttl(key)
        return seconds
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'ttl',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<number, CacheError>>
  }

  /**
   * Use Redis EXPIRE command to set TTL on existing key
   */
  async expire(key: string, seconds: number): Promise<Result<boolean, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis EXPIRE command via ioredis
        const result = await this.redis.expire(key, seconds)
        return result === 1
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'expire',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<boolean, CacheError>>
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create cache instance based on configuration
 */
export const createCache = (config: CacheConfig): Result<ICache, CacheError> => {
  switch (config.backend) {
    case 'memory':
      return success(new MemoryCache(config))
    case 'redis':
      return success(new RedisCache(config))
    default:
      return failure(
        cacheError(`Unsupported cache backend: ${config.backend}`, {
          backend: config.backend,
          operation: 'create',
        })
      )
  }
}

/**
 * Create memory cache with configuration
 */
export const createMemoryCache = (config: Omit<CacheConfig, 'backend'>): MemoryCache => {
  return new MemoryCache({ ...config, backend: 'memory' })
}

/**
 * Create Redis cache with configuration
 */
export const createRedisCache = (config: Omit<CacheConfig, 'backend'>): RedisCache => {
  return new RedisCache({ ...config, backend: 'redis' })
}

/**
 * Create persistent cache (Redis-backed) with file path configuration
 */
export const createPersistent = (
  filePath: string,
  config: Omit<CacheConfig, 'backend' | 'redis'>
): Result<RedisCache, CacheError> => {
  try {
    // For persistent cache, we use Redis with configuration derived from file path
    const redisConfig: RedisOptions = {
      host: 'localhost',
      port: 6379,
      // Use file path as keyPrefix or database selector
      keyPrefix: `${filePath.replace(/[^a-zA-Z0-9]/g, '_')}:`,
      lazyConnect: true,
      maxRetriesPerRequest: 3,
    }

    const cache = new RedisCache({
      ...config,
      backend: 'redis',
      redis: redisConfig,
    })

    return success(cache)
  } catch (error) {
    return failure(
      cacheError(error instanceof Error ? error.message : String(error), {
        operation: 'createPersistent',
        backend: 'redis',
      })
    )
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Cache-aside pattern implementation
 */
export const cacheAside = async <T>(
  key: string,
  cache: ICache,
  loader: () => Promise<T>,
  ttl?: number
): Promise<Result<T, CacheError>> => {
  // Try to get from cache first
  const cached = await cache.get<T>(key)
  if (cached.tag === 'success') {
    return cached
  }

  // Load from source
  return fromAsyncTryCatch(
    async () => {
      const value = await loader()

      // Store in cache
      await cache.set(key, value, ttl)

      return value
    },
    (error) =>
      cacheError(`Cache-aside loader failed: ${error}`, {
        operation: 'cache-aside',
        key,
      })
  ) as Promise<Result<T, CacheError>>
}

// ============================================================================
// Re-exports
// ============================================================================

// Types are already exported above
