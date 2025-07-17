/**
 * Cache Tests - Contract Compliance
 * Focus: Law, Interfaces, Behavior
 */

import { describe, it, expect, vi } from 'vitest'
import {
  MemoryCache,
  RedisCache,
  createCache,
  createMemoryCache,
  createRedisCache,
  cacheAside,
  cacheError,
  type CacheConfig,
} from '@qi/core'
import { isSuccess, isFailure } from '@qi/base'

describe('Cache Factory Operations', () => {
  it('createCache creates memory cache', () => {
    const config: CacheConfig = {
      backend: 'memory',
      maxSize: 100,
      defaultTtl: 60,
    }

    const cache = createCache(config)
    expect(cache).toBeInstanceOf(MemoryCache)
  })

  it('createCache creates redis cache', () => {
    const config: CacheConfig = {
      backend: 'redis',
      defaultTtl: 60,
      redis: { host: 'localhost', port: 6379 },
    }

    const cache = createCache(config)
    expect(cache).toBeInstanceOf(RedisCache)
  })

  it('createMemoryCache creates memory cache', () => {
    const cache = createMemoryCache({ maxSize: 100 })
    expect(cache).toBeInstanceOf(MemoryCache)
  })

  it('createRedisCache creates redis cache', () => {
    const cache = createRedisCache({ redis: { host: 'localhost' } })
    expect(cache).toBeInstanceOf(RedisCache)
  })
})

describe('Cache Interface Operations', () => {
  const cache = createMemoryCache({ maxSize: 100, defaultTtl: 60 })

  it('cache has all required methods', () => {
    expect(typeof cache.get).toBe('function')
    expect(typeof cache.set).toBe('function')
    expect(typeof cache.delete).toBe('function')
    expect(typeof cache.exists).toBe('function')
    expect(typeof cache.clear).toBe('function')
    expect(typeof cache.keys).toBe('function')
    expect(typeof cache.getStats).toBe('function')
    expect(typeof cache.close).toBe('function')
  })

  it('all operations return promises', () => {
    const getPromise = cache.get('test')
    const setPromise = cache.set('test', 'value')
    const deletePromise = cache.delete('test')
    const existsPromise = cache.exists('test')
    const clearPromise = cache.clear()
    const keysPromise = cache.keys()

    expect(getPromise).toBeInstanceOf(Promise)
    expect(setPromise).toBeInstanceOf(Promise)
    expect(deletePromise).toBeInstanceOf(Promise)
    expect(existsPromise).toBeInstanceOf(Promise)
    expect(clearPromise).toBeInstanceOf(Promise)
    expect(keysPromise).toBeInstanceOf(Promise)
  })

  it('getStats returns cache statistics', () => {
    const stats = cache.getStats()

    expect(typeof stats.hits).toBe('number')
    expect(typeof stats.misses).toBe('number')
    expect(typeof stats.sets).toBe('number')
    expect(typeof stats.deletes).toBe('number')
    expect(typeof stats.evictions).toBe('number')
    expect(typeof stats.size).toBe('number')
  })
})

describe('Cache Behavior Contract', () => {
  const cache = createMemoryCache({ maxSize: 3, defaultTtl: 1000 })

  it('set then get returns same value', async () => {
    const setResult = await cache.set('key1', 'value1')
    expect(isSuccess(setResult)).toBe(true)

    const getResult = await cache.get('key1')
    expect(isSuccess(getResult)).toBe(true)
    expect(getResult.tag === 'success' && getResult.value).toBe('value1')
  })

  it('get returns NOT_FOUND for missing key', async () => {
    const result = await cache.get('missing-key')
    expect(isFailure(result)).toBe(true)
  })

  it('exists returns true for existing key', async () => {
    await cache.set('key2', 'value2')
    const result = await cache.exists('key2')
    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe(true)
  })

  it('exists returns false for missing key', async () => {
    const result = await cache.exists('missing-key')
    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe(false)
  })

  it('delete removes key', async () => {
    await cache.set('key3', 'value3')
    const deleteResult = await cache.delete('key3')
    expect(isSuccess(deleteResult)).toBe(true)
    expect(deleteResult.tag === 'success' && deleteResult.value).toBe(true)

    const getResult = await cache.get('key3')
    expect(isFailure(getResult)).toBe(true)
  })

  it('delete returns false for missing key', async () => {
    const result = await cache.delete('missing-key')
    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe(false)
  })

  it('clear removes all entries', async () => {
    await cache.set('key4', 'value4')
    await cache.set('key5', 'value5')

    const clearResult = await cache.clear()
    expect(isSuccess(clearResult)).toBe(true)

    const stats = cache.getStats()
    expect(stats.size).toBe(0)
  })

  it('keys returns all keys', async () => {
    await cache.clear()
    await cache.set('key6', 'value6')
    await cache.set('key7', 'value7')

    const result = await cache.keys()
    expect(isSuccess(result)).toBe(true)
    if (result.tag === 'success') {
      expect(result.value).toContain('key6')
      expect(result.value).toContain('key7')
    }
  })

  it('keys with pattern filters keys', async () => {
    await cache.clear()
    await cache.set('user:123', 'user-value')
    await cache.set('post:456', 'post-value')

    const result = await cache.keys('user:*')
    expect(isSuccess(result)).toBe(true)
    if (result.tag === 'success') {
      expect(result.value).toContain('user:123')
      expect(result.value).not.toContain('post:456')
    }
  })
})

describe('Cache TTL Behavior', () => {
  const cache = createMemoryCache({ maxSize: 10, defaultTtl: 1 }) // 1 second TTL

  it('entry expires after TTL', async () => {
    await cache.set('expire-key', 'value', 1) // 1 second TTL

    const immediate = await cache.get('expire-key')
    expect(isSuccess(immediate)).toBe(true)

    // Wait for expiration
    await new Promise((resolve) => setTimeout(resolve, 1100))

    const expired = await cache.get('expire-key')
    expect(isFailure(expired)).toBe(true)
  })

  it('uses default TTL when not specified', async () => {
    await cache.set('default-ttl-key', 'value') // Uses default 1 second TTL

    const immediate = await cache.get('default-ttl-key')
    expect(isSuccess(immediate)).toBe(true)

    // Wait for expiration
    await new Promise((resolve) => setTimeout(resolve, 1100))

    const expired = await cache.get('default-ttl-key')
    expect(isFailure(expired)).toBe(true)
  })
})

describe('Cache LRU Behavior', () => {
  const cache = createMemoryCache({ maxSize: 2 }) // Only 2 entries

  it('evicts least recently used when full', async () => {
    await cache.set('lru1', 'value1')
    await cache.set('lru2', 'value2')
    await cache.set('lru3', 'value3') // Should evict lru1

    const result1 = await cache.get('lru1')
    const result2 = await cache.get('lru2')
    const result3 = await cache.get('lru3')

    expect(isFailure(result1)).toBe(true) // Evicted
    expect(isSuccess(result2)).toBe(true) // Still there
    expect(isSuccess(result3)).toBe(true) // Still there
  })

  it('get updates access order', async () => {
    await cache.clear()
    await cache.set('access1', 'value1')
    await cache.set('access2', 'value2')

    // Access first key to make it most recently used
    await cache.get('access1')

    // Add third key, should evict access2 (least recently used)
    await cache.set('access3', 'value3')

    const result1 = await cache.get('access1')
    const result2 = await cache.get('access2')
    const result3 = await cache.get('access3')

    expect(isSuccess(result1)).toBe(true) // Recently accessed
    expect(isFailure(result2)).toBe(true) // Evicted
    expect(isSuccess(result3)).toBe(true) // New entry
  })
})

describe('Cache Utility Functions', () => {
  const cache = createMemoryCache({ maxSize: 10 })

  it('cacheAside returns cached value if exists', async () => {
    await cache.set('aside-key', 'cached-value')

    const loader = vi.fn().mockResolvedValue('loaded-value')
    const result = await cacheAside('aside-key', cache, loader)

    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe('cached-value')
    expect(loader).not.toHaveBeenCalled()
  })

  it('cacheAside loads and caches if missing', async () => {
    const loader = vi.fn().mockResolvedValue('loaded-value')
    const result = await cacheAside('new-key', cache, loader)

    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe('loaded-value')
    expect(loader).toHaveBeenCalledOnce()

    // Should be cached now
    const cached = await cache.get('new-key')
    expect(isSuccess(cached)).toBe(true)
    expect(cached.tag === 'success' && cached.value).toBe('loaded-value')
  })
})

describe('Cache Error Factory', () => {
  it('cacheError creates cache-specific error', () => {
    const error = cacheError('Cache failed', {
      operation: 'get',
      key: 'test-key',
      backend: 'memory',
    })

    expect(error.category).toBe('RESOURCE')
    expect(error.message).toBe('Cache failed')
    expect(error.context.operation).toBe('get')
    expect(error.context.key).toBe('test-key')
    expect(error.context.backend).toBe('memory')
  })

  it('cacheError defaults context to empty', () => {
    const error = cacheError('Cache failed')

    expect(error.category).toBe('RESOURCE')
    expect(error.message).toBe('Cache failed')
    expect(error.context).toEqual({})
  })
})
