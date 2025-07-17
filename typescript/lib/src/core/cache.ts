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
  exists(key: string): Promise<Result<boolean, CacheError>>
  clear(): Promise<Result<void, CacheError>>
  keys(pattern?: string): Promise<Result<string[], CacheError>>
  getStats(): CacheStats
  close(): Promise<void>
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

  async exists(key: string): Promise<Result<boolean, CacheError>> {
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

  getStats(): CacheStats {
    return { ...this.stats }
  }

  async close(): Promise<void> {
    this.entries.clear()
    this.events.removeAllListeners()
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
    return fromAsyncTryCatch(
      async () => {
        // Use Redis GET command via ioredis
        const value = await this.redis.get(key)

        if (value === null) {
          this.stats.misses++
          this.events.emit('miss', key)
          throw new Error(`Cache miss for key: ${key}`)
        }

        const parsed = JSON.parse(value) as T
        this.stats.hits++
        this.events.emit('hit', key, parsed)

        return parsed
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'get',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<T, CacheError>>
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

  async exists(key: string): Promise<Result<boolean, CacheError>> {
    return fromAsyncTryCatch(
      async () => {
        // Use Redis EXISTS command via ioredis
        const result = await this.redis.exists(key)
        return result === 1
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'exists',
          key,
          backend: 'redis',
        })
    ) as Promise<Result<boolean, CacheError>>
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

  async close(): Promise<void> {
    await this.redis.quit()
    this.events.removeAllListeners()
  }

  // Redis-specific methods leveraging ioredis capabilities

  /**
   * Use Redis pipeline for batch operations
   */
  async mget<T>(keys: string[]): Promise<Result<(T | null)[], CacheError>> {
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

        return results.map(([error, value]) => {
          if (error) {
            throw error
          }
          return value ? (JSON.parse(value as string) as T) : null
        })
      },
      (error) =>
        cacheError(error instanceof Error ? error.message : String(error), {
          operation: 'mget',
          backend: 'redis',
        })
    ) as Promise<Result<(T | null)[], CacheError>>
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
export const createCache = (config: CacheConfig): ICache => {
  switch (config.backend) {
    case 'memory':
      return new MemoryCache(config)
    case 'redis':
      return new RedisCache(config)
    default:
      throw new Error(`Unsupported cache backend: ${config.backend}`)
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
