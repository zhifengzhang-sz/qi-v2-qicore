/**
 * QiCore Foundation - Cache Module
 *
 * Unified caching interface with ioredis and memory backends using functional patterns.
 * Provides high-performance caching with TTL management, LRU eviction, and
 * Redis pipeline operations.
 */

import { Err, Ok, type QiError, type Result, createError } from '@qi/base'
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
  createError({
    code: 'CACHE_ERROR',
    message,
    category: 'RESOURCE',
    context,
  }) as CacheError

// ============================================================================
// Cache Interface
// ============================================================================

/**
 * Unified cache interface
 */
export interface ICache {
  get<T>(key: string): Promise<Result<T, CacheError>>
  set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>>
  delete(key: string): Promise<Result<boolean, CacheError>>
  exists(key: string): Promise<Result<boolean, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  getStats(): CacheStats
  close(): Promise<void>
}

// ============================================================================
// Memory Cache Implementation
// ============================================================================

/**
 * In-memory cache with LRU eviction
 */
export class MemoryCache implements ICache {
  private readonly entries = new Map<string, CacheEntry>()
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
  private readonly accessOrder: string[] = []

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

    // Set up TTL cleanup interval
    setInterval(() => this.cleanupExpired(), 60000) // Check every minute
  }

  /**
   * Get value from cache
   */
  async get<T>(key: string): Promise<Result<T, CacheError>> {
    const entry = this.entries.get(key)

    if (!entry) {
      this.stats.misses++
      this.events.emit('miss', key)
      return Err(
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
      this.events.emit('miss', key)
      return Err(
        cacheError(`Cache entry expired for key: ${key}`, {
          operation: 'get',
          key,
          backend: 'memory',
        })
      )
    }

    // Update access metadata
    const updatedEntry: CacheEntry<T> = {
      value: entry.value as T,
      createdAt: entry.createdAt,
      expiresAt: entry.expiresAt,
      accessCount: entry.accessCount + 1,
      lastAccessed: new Date(),
    }

    this.entries.set(key, updatedEntry)
    this.updateAccessOrder(key, 'access')
    this.stats.hits++
    this.events.emit('hit', key, entry.value)

    return Ok(entry.value as T)
  }

  /**
   * Set value in cache
   */
  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>> {
    try {
      const now = new Date()
      const effectiveTtl = ttl ?? this.config.defaultTtl

      const entry: CacheEntry<T> = {
        value,
        createdAt: now,
        expiresAt: effectiveTtl ? new Date(now.getTime() + effectiveTtl * 1000) : undefined,
        accessCount: 0,
        lastAccessed: now,
      }

      // Check if we need to evict entries
      if (
        this.config.maxSize &&
        this.entries.size >= this.config.maxSize &&
        !this.entries.has(key)
      ) {
        this.evictLru()
      }

      const wasPresent = this.entries.has(key)
      this.entries.set(key, entry)
      this.updateAccessOrder(key, 'set')

      if (!wasPresent) {
        this.stats.size++
      }

      this.stats.sets++
      this.events.emit('set', key, value, ttl)

      return Ok(undefined)
    } catch (error) {
      return Err(
        cacheError(`Failed to set cache entry: ${error}`, {
          operation: 'set',
          key,
          backend: 'memory',
        })
      )
    }
  }

  /**
   * Delete value from cache
   */
  async delete(key: string): Promise<Result<boolean, CacheError>> {
    const existed = this.entries.delete(key)

    if (existed) {
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      this.stats.deletes++
      this.events.emit('delete', key)
    }

    return Ok(existed)
  }

  /**
   * Check if key exists
   */
  async exists(key: string): Promise<Result<boolean, CacheError>> {
    const entry = this.entries.get(key)

    if (!entry) {
      return Ok(false)
    }

    // Check if expired
    if (entry.expiresAt && entry.expiresAt < new Date()) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      this.stats.evictions++
      this.events.emit('evict', key, 'ttl')
      return Ok(false)
    }

    return Ok(true)
  }

  /**
   * Clear all entries
   */
  async clear(): Promise<Result<void, CacheError>> {
    const size = this.entries.size
    this.entries.clear()
    this.accessOrder.length = 0
    this.stats.size = 0
    this.stats.evictions += size

    return Ok(undefined)
  }

  /**
   * Get all keys matching pattern
   */
  async keys(pattern?: string): Promise<Result<string[], CacheError>> {
    const allKeys = Array.from(this.entries.keys())

    if (!pattern) {
      return Ok(allKeys)
    }

    // Simple pattern matching (supports * wildcard)
    const regex = new RegExp(pattern.replace(/\*/g, '.*'))
    const matchedKeys = allKeys.filter((key) => regex.test(key))

    return Ok(matchedKeys)
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    return { ...this.stats }
  }

  /**
   * Close cache and cleanup
   */
  async close(): Promise<void> {
    this.entries.clear()
    this.accessOrder.length = 0
    this.events.removeAllListeners()
  }

  /**
   * Add event listener
   */
  on<K extends keyof CacheEvents>(event: K, listener: CacheEvents[K]): this {
    this.events.on(event, listener as any)
    return this
  }

  /**
   * Remove event listener
   */
  off<K extends keyof CacheEvents>(event: K, listener: CacheEvents[K]): this {
    this.events.off(event, listener as any)
    return this
  }

  /**
   * Update access order for LRU eviction
   */
  private updateAccessOrder(key: string, operation: 'access' | 'set' | 'remove'): void {
    const index = this.accessOrder.indexOf(key)

    if (operation === 'remove') {
      if (index > -1) {
        this.accessOrder.splice(index, 1)
      }
    } else {
      // Move to end (most recently used)
      if (index > -1) {
        this.accessOrder.splice(index, 1)
      }
      this.accessOrder.push(key)
    }
  }

  /**
   * Evict least recently used entry
   */
  private evictLru(): void {
    if (this.accessOrder.length === 0) return

    const lruKey = this.accessOrder.shift()
    if (!lruKey) return
    this.entries.delete(lruKey)
    this.stats.size--
    this.stats.evictions++
    this.events.emit('evict', lruKey, 'lru')
  }

  /**
   * Clean up expired entries
   */
  private cleanupExpired(): void {
    const now = new Date()
    const expiredKeys: string[] = []

    for (const [key, entry] of this.entries) {
      if (entry.expiresAt && entry.expiresAt < now) {
        expiredKeys.push(key)
      }
    }

    for (const key of expiredKeys) {
      this.entries.delete(key)
      this.updateAccessOrder(key, 'remove')
      this.stats.size--
      this.stats.evictions++
      this.events.emit('evict', key, 'ttl')
    }
  }
}

// ============================================================================
// Redis Cache Implementation
// ============================================================================

/**
 * Redis-backed cache with pipeline operations
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

    // Initialize Redis client
    this.redis = new Redis(config.redis || {})

    // Set up event listeners
    this.redis.on('connect', () => {
      this.events.emit('connect')
    })

    this.redis.on('error', (error) => {
      this.events.emit(
        'error',
        cacheError(`Redis error: ${error}`, {
          backend: 'redis',
          cache: config.name,
        })
      )
    })
  }

  /**
   * Get value from Redis
   */
  async get<T>(key: string): Promise<Result<T, CacheError>> {
    try {
      const value = await this.redis.get(key)

      if (value === null) {
        this.stats.misses++
        this.events.emit('miss', key)
        return Err(
          cacheError(`Cache miss for key: ${key}`, {
            operation: 'get',
            key,
            backend: 'redis',
          })
        )
      }

      const parsed = JSON.parse(value) as T
      this.stats.hits++
      this.events.emit('hit', key, parsed)

      return Ok(parsed)
    } catch (error) {
      return Err(
        cacheError(`Failed to get from Redis: ${error}`, {
          operation: 'get',
          key,
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Set value in Redis
   */
  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void, CacheError>> {
    try {
      const serialized = JSON.stringify(value)
      const effectiveTtl = ttl ?? this.config.defaultTtl

      if (effectiveTtl) {
        await this.redis.setex(key, effectiveTtl, serialized)
      } else {
        await this.redis.set(key, serialized)
      }

      this.stats.sets++
      this.events.emit('set', key, value, ttl)

      return Ok(undefined)
    } catch (error) {
      return Err(
        cacheError(`Failed to set in Redis: ${error}`, {
          operation: 'set',
          key,
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Delete value from Redis
   */
  async delete(key: string): Promise<Result<boolean, CacheError>> {
    try {
      const result = await this.redis.del(key)
      const existed = result > 0

      if (existed) {
        this.stats.deletes++
        this.events.emit('delete', key)
      }

      return Ok(existed)
    } catch (error) {
      return Err(
        cacheError(`Failed to delete from Redis: ${error}`, {
          operation: 'delete',
          key,
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Check if key exists in Redis
   */
  async exists(key: string): Promise<Result<boolean, CacheError>> {
    try {
      const result = await this.redis.exists(key)
      return Ok(result > 0)
    } catch (error) {
      return Err(
        cacheError(`Failed to check existence in Redis: ${error}`, {
          operation: 'exists',
          key,
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Clear all entries (use with caution)
   */
  async clear(): Promise<Result<void, CacheError>> {
    try {
      await this.redis.flushdb()
      return Ok(undefined)
    } catch (error) {
      return Err(
        cacheError(`Failed to clear Redis: ${error}`, {
          operation: 'clear',
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Get keys matching pattern
   */
  async keys(pattern = '*'): Promise<Result<string[], CacheError>> {
    try {
      const keys = await this.redis.keys(pattern)
      return Ok(keys)
    } catch (error) {
      return Err(
        cacheError(`Failed to get keys from Redis: ${error}`, {
          operation: 'keys',
          backend: 'redis',
        })
      )
    }
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    return { ...this.stats }
  }

  /**
   * Close Redis connection
   */
  async close(): Promise<void> {
    await this.redis.quit()
    this.events.removeAllListeners()
  }

  /**
   * Add event listener
   */
  on<K extends keyof CacheEvents>(event: K, listener: CacheEvents[K]): this {
    this.events.on(event, listener as any)
    return this
  }

  /**
   * Remove event listener
   */
  off<K extends keyof CacheEvents>(event: K, listener: CacheEvents[K]): this {
    this.events.off(event, listener as any)
    return this
  }

  /**
   * Execute Redis pipeline for batch operations
   */
  async pipeline(
    operations: Array<(pipeline: any) => void>
  ): Promise<Result<unknown[], CacheError>> {
    try {
      const pipeline = this.redis.pipeline()

      for (const operation of operations) {
        operation(pipeline)
      }

      const results = await pipeline.exec()
      return Ok(results || [])
    } catch (error) {
      return Err(
        cacheError(`Pipeline operation failed: ${error}`, {
          operation: 'pipeline',
          backend: 'redis',
        })
      )
    }
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create cache instance with configuration
 */
export const createCache = (config: CacheConfig): Result<ICache, CacheError> => {
  try {
    switch (config.backend) {
      case 'memory':
        return Ok(new MemoryCache(config))
      case 'redis':
        return Ok(new RedisCache(config))
      default:
        return Err(
          cacheError(`Unsupported cache backend: ${config.backend}`, {
            backend: config.backend,
          })
        )
    }
  } catch (error) {
    return Err(
      cacheError(`Failed to create cache: ${error}`, {
        backend: config.backend,
      })
    )
  }
}

/**
 * Create memory cache with default configuration
 */
export const memoryCache = (maxSize = 1000, defaultTtl = 3600): ICache => {
  return new MemoryCache({
    backend: 'memory',
    maxSize,
    defaultTtl,
  })
}

/**
 * Create Redis cache with default configuration
 */
export const redisCache = (redis?: RedisOptions, defaultTtl = 3600): Result<ICache, CacheError> => {
  return createCache({
    backend: 'redis',
    redis,
    defaultTtl,
  })
}

// ============================================================================
// Cache Utilities
// ============================================================================

/**
 * Cache key builder utility
 */
export const cacheKey = (...parts: (string | number)[]): string => {
  return parts.join(':')
}

/**
 * Cache namespace utility
 */
export const namespaced = (namespace: string) => {
  return (...parts: (string | number)[]): string => {
    return cacheKey(namespace, ...parts)
  }
}

/**
 * Cache warming utility
 */
export const warmCache = async <T>(
  cache: ICache,
  keys: string[],
  loader: (key: string) => Promise<T>,
  ttl?: number
): Promise<Result<void, CacheError>> => {
  try {
    const promises = keys.map(async (key) => {
      const exists = await cache.exists(key)
      if (exists.tag === 'success' && !exists.value) {
        const value = await loader(key)
        await cache.set(key, value, ttl)
      }
    })

    await Promise.all(promises)
    return Ok(undefined)
  } catch (error) {
    return Err(
      cacheError(`Cache warming failed: ${error}`, {
        operation: 'warm',
      })
    )
  }
}

/**
 * Cache-aside pattern utility
 */
export const cacheAside = async <T>(
  cache: ICache,
  key: string,
  loader: () => Promise<T>,
  ttl?: number
): Promise<Result<T, CacheError>> => {
  // Try to get from cache first
  const cached = await cache.get<T>(key)
  if (cached.tag === 'success') {
    return cached
  }

  // Load from source
  try {
    const value = await loader()

    // Store in cache
    await cache.set(key, value, ttl)

    return Ok(value)
  } catch (error) {
    return Err(
      cacheError(`Cache-aside loader failed: ${error}`, {
        operation: 'cache-aside',
        key,
      })
    )
  }
}

// ============================================================================
// Common Cache Configurations
// ============================================================================

/**
 * Development cache configuration
 */
export const developmentConfig: CacheConfig = {
  backend: 'memory',
  maxSize: 100,
  defaultTtl: 300, // 5 minutes
  name: 'qicore-dev',
}

/**
 * Production cache configuration
 */
export const productionConfig: CacheConfig = {
  backend: 'redis',
  defaultTtl: 3600, // 1 hour
  name: 'qicore-prod',
  redis: {
    host: process.env.REDIS_HOST || 'localhost',
    port: Number.parseInt(process.env.REDIS_PORT || '6379'),
    password: process.env.REDIS_PASSWORD,
  } as any,
}

/**
 * Test cache configuration
 */
export const testConfig: CacheConfig = {
  backend: 'memory',
  maxSize: 10,
  defaultTtl: 60, // 1 minute
  name: 'qicore-test',
}

/**
 * Get cache configuration based on environment
 */
export const getEnvironmentConfig = (): CacheConfig => {
  const env = process.env.NODE_ENV || 'development'

  switch (env) {
    case 'production':
      return productionConfig
    case 'test':
      return testConfig
    default:
      return developmentConfig
  }
}
