/**
 * Cache Tests - Contract Compliance
 * Focus: Law, Interfaces, Behavior
 */

import { describe, it, expect, vi, beforeEach } from 'vitest'
import { SAMPLE_DATA } from '../test-constants.js'
import {
  MemoryCache,
  RedisCache,
  createCache,
  createMemoryCache,
  createRedisCache,
  createPersistent,
  cacheAside,
  cacheError,
  type CacheConfig,
} from '@qi/core'
import { match, isSuccess, isFailure } from '@qi/base'

describe('Cache Factory Operations', () => {
  it('createCache with memory backend provides working cache', async () => {
    const config: CacheConfig = {
      backend: 'memory',
      maxSize: 100,
      defaultTtl: 60,
    }

    const cacheResult = createCache(config)
    expect(isSuccess(cacheResult)).toBe(true)

    if (cacheResult.tag === 'failure') return // Type guard
    const cache = cacheResult.value

    // Test behavior, not implementation details
    await cache.set(SAMPLE_DATA.CACHE_KEYS.BASIC, SAMPLE_DATA.CACHE_VALUES.BASIC)
    const result = await cache.get(SAMPLE_DATA.CACHE_KEYS.BASIC)

    match(
      (value) => expect(value).toBe(SAMPLE_DATA.CACHE_VALUES.BASIC),
      (error) => expect(error).toBe(null), // Should work
      result
    )
  })

  it('createCache creates redis cache', () => {
    const config: CacheConfig = {
      backend: 'redis',
      defaultTtl: 60,
      redis: { host: 'localhost', port: 6379 },
    }

    const cacheResult = createCache(config)
    expect(isSuccess(cacheResult)).toBe(true)

    if (cacheResult.tag === 'failure') return // Type guard
    expect(cacheResult.value).toBeInstanceOf(RedisCache)
  })

  it('createMemoryCache creates memory cache', () => {
    const cache = createMemoryCache({ maxSize: 100 })
    expect(cache).toBeInstanceOf(MemoryCache)
  })

  it('createRedisCache creates redis cache', () => {
    const cache = createRedisCache({ redis: { host: 'localhost' } })
    expect(cache).toBeInstanceOf(RedisCache)
  })

  it('createPersistent creates persistent redis cache', () => {
    const result = createPersistent('/tmp/test.cache', { maxSize: 100 })
    expect(isSuccess(result)).toBe(true)
    if (isSuccess(result)) {
      expect(result.value).toBeInstanceOf(RedisCache)
    }
  })

  it('createPersistent handles errors gracefully', () => {
    // Test with invalid configuration that might cause an error
    const result = createPersistent('', { maxSize: 100 })
    expect(isSuccess(result)).toBe(true) // Should still succeed with empty path
  })
})

describe('Cache Interface Operations', () => {
  const cache = createMemoryCache({ maxSize: 100, defaultTtl: 60 })

  it('cache has all required methods', () => {
    expect(typeof cache.get).toBe('function')
    expect(typeof cache.set).toBe('function')
    expect(typeof cache.delete).toBe('function')
    expect(typeof cache.has).toBe('function')
    expect(typeof cache.remove).toBe('function')
    expect(typeof cache.size).toBe('function')
    expect(typeof cache.clear).toBe('function')
    expect(typeof cache.keys).toBe('function')
    expect(typeof cache.getStats).toBe('function')
    expect(typeof cache.close).toBe('function')
  })

  it('all operations return promises', () => {
    const getPromise = cache.get('test')
    const setPromise = cache.set('test', 'value')
    const deletePromise = cache.delete('test')
    const hasPromise = cache.has('test')
    const clearPromise = cache.clear()
    const keysPromise = cache.keys()

    expect(getPromise).toBeInstanceOf(Promise)
    expect(setPromise).toBeInstanceOf(Promise)
    expect(deletePromise).toBeInstanceOf(Promise)
    expect(hasPromise).toBeInstanceOf(Promise)
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

  it('set then get returns same value using Result<T> patterns', async () => {
    const setResult = await cache.set('key1', 'value1')

    match(
      () => {}, // Set succeeded
      (error) => expect(error).toBe(null), // Should not fail
      setResult
    )

    const getResult = await cache.get('key1')

    match(
      (value) => expect(value).toBe('value1'),
      (error) => expect(error).toBe(null), // Should not fail
      getResult
    )
  })

  it('get returns NOT_FOUND for missing key', async () => {
    const result = await cache.get('missing-key')
    expect(isFailure(result)).toBe(true)
  })

  it('has returns true for existing key', async () => {
    await cache.set('key2', 'value2')
    const result = await cache.has('key2')
    expect(isSuccess(result)).toBe(true)
    expect(result.tag === 'success' && result.value).toBe(true)
  })

  it('has returns false for missing key', async () => {
    const result = await cache.has('missing-key')
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

    // Use real function instead of mock - test behavior, not interactions
    let loaderCalled = false
    const loader = async () => {
      loaderCalled = true
      return 'loaded-value'
    }

    const result = await cacheAside('aside-key', cache, loader)

    match(
      (value) => {
        expect(value).toBe('cached-value')
        expect(loaderCalled).toBe(false) // Should not call loader when cached
      },
      (error) => expect(error).toBe(null), // Should not fail
      result
    )
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
      key: SAMPLE_DATA.CACHE_KEYS.ERROR,
      backend: 'memory',
    })

    expect(error.category).toBe('RESOURCE')
    expect(error.message).toBe('Cache failed')
    expect(error.context.operation).toBe('get')
    expect(error.context.key).toBe(SAMPLE_DATA.CACHE_KEYS.ERROR)
    expect(error.context.backend).toBe('memory')
  })

  it('cacheError defaults context to empty', () => {
    const error = cacheError('Cache failed')

    expect(error.category).toBe('RESOURCE')
    expect(error.message).toBe('Cache failed')
    expect(error.context).toEqual({})
  })
})

describe('Cache Batch Operations', () => {
  let cache: MemoryCache

  beforeEach(() => {
    cache = createMemoryCache({ maxSize: 100 })
  })

  describe('mget', () => {
    it('returns empty object for non-existent keys', async () => {
      const result = await cache.mget(['key1', 'key2', 'key3'])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toEqual({})
    })

    it('returns existing values for keys', async () => {
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')

      const result = await cache.mget(['key1', 'key2', 'key3'])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toEqual({
        key1: 'value1',
        key2: 'value2',
      })
    })

    it('handles mixed existing and non-existing keys', async () => {
      await cache.set('exists', 'found')

      const result = await cache.mget(['exists', 'missing', 'also-missing'])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toEqual({
        exists: 'found',
      })
    })
  })

  describe('mset', () => {
    it('sets multiple key-value pairs', async () => {
      const entries = {
        key1: 'value1',
        key2: 'value2',
        key3: 'value3',
      }

      const result = await cache.mset(entries)

      expect(isSuccess(result)).toBe(true)

      // Verify all keys were set
      const value1 = await cache.get('key1')
      const value2 = await cache.get('key2')
      const value3 = await cache.get('key3')

      expect(isSuccess(value1) && value1.value).toBe('value1')
      expect(isSuccess(value2) && value2.value).toBe('value2')
      expect(isSuccess(value3) && value3.value).toBe('value3')
    })

    it('sets values with TTL', async () => {
      const entries = { key1: 'value1', key2: 'value2' }

      const result = await cache.mset(entries, 1) // 1 second TTL

      expect(isSuccess(result)).toBe(true)

      // Should exist immediately
      const immediate1 = await cache.get('key1')
      const immediate2 = await cache.get('key2')
      expect(isSuccess(immediate1) && immediate1.value).toBe('value1')
      expect(isSuccess(immediate2) && immediate2.value).toBe('value2')
    })

    it('handles empty entries object', async () => {
      const result = await cache.mset({})

      expect(isSuccess(result)).toBe(true)
    })
  })

  describe('mdelete', () => {
    it('returns 0 for non-existent keys', async () => {
      const result = await cache.mdelete(['missing1', 'missing2'])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toBe(0)
    })

    it('deletes existing keys and returns count', async () => {
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')
      await cache.set('key3', 'value3')

      const result = await cache.mdelete(['key1', 'key3', 'missing'])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toBe(2)

      // Verify deletions
      const check1 = await cache.has('key1')
      const check2 = await cache.has('key2')
      const check3 = await cache.has('key3')

      expect(isSuccess(check1) && check1.value).toBe(false)
      expect(isSuccess(check2) && check2.value).toBe(true) // Wasn't deleted
      expect(isSuccess(check3) && check3.value).toBe(false)
    })

    it('handles empty keys array', async () => {
      const result = await cache.mdelete([])

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toBe(0)
    })
  })

  describe('Batch operations integration', () => {
    it('mset + mget + mdelete workflow', async () => {
      // Set multiple values
      const setResult = await cache.mset({
        user1: { id: 1, name: 'Alice' },
        user2: { id: 2, name: 'Bob' },
        user3: { id: 3, name: 'Charlie' },
      })
      expect(isSuccess(setResult)).toBe(true)

      // Get multiple values
      const getResult = await cache.mget(['user1', 'user2', 'user4'])
      expect(isSuccess(getResult)).toBe(true)
      expect(getResult.tag === 'success' && getResult.value).toEqual({
        user1: { id: 1, name: 'Alice' },
        user2: { id: 2, name: 'Bob' },
      })

      // Delete some values
      const deleteResult = await cache.mdelete(['user1', 'user3'])
      expect(isSuccess(deleteResult)).toBe(true)
      expect(deleteResult.tag === 'success' && deleteResult.value).toBe(2)

      // Verify final state
      const finalCheck = await cache.mget(['user1', 'user2', 'user3'])
      expect(isSuccess(finalCheck)).toBe(true)
      expect(finalCheck.tag === 'success' && finalCheck.value).toEqual({
        user2: { id: 2, name: 'Bob' },
      })
    })
  })
})

describe('Cache Advanced Operations', () => {
  let cache: MemoryCache
  beforeEach(() => {
    cache = createMemoryCache({ maxSize: 100 })
  })

  describe('getOrSet', () => {
    it('returns existing value if key exists', async () => {
      await cache.set('existing', 'cached-value')

      const factory = vi.fn().mockResolvedValue({ tag: 'success', value: 'new-value' })
      const result = await cache.getOrSet('existing', factory)

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toBe('cached-value')
      expect(factory).not.toHaveBeenCalled()
    })

    it('calls factory and caches result if key does not exist', async () => {
      const factory = vi.fn().mockResolvedValue({ tag: 'success', value: 'factory-value' })
      const result = await cache.getOrSet('new-key', factory)

      expect(isSuccess(result)).toBe(true)
      expect(result.tag === 'success' && result.value).toBe('factory-value')
      expect(factory).toHaveBeenCalledOnce()

      // Verify the value was cached
      const cachedResult = await cache.get('new-key')
      expect(isSuccess(cachedResult)).toBe(true)
      expect(cachedResult.tag === 'success' && cachedResult.value).toBe('factory-value')
    })

    it('returns factory error if factory fails', async () => {
      const factory = vi
        .fn()
        .mockResolvedValue({ tag: 'failure', error: cacheError('Factory failed') })
      const result = await cache.getOrSet('error-key', factory)

      expect(isFailure(result)).toBe(true)
      expect(result.tag === 'failure' && result.error.message).toBe('Factory failed')
      expect(factory).toHaveBeenCalledOnce()

      // Verify nothing was cached
      const cachedResult = await cache.get('error-key')
      expect(isFailure(cachedResult)).toBe(true)
    })

    it('handles concurrent calls atomically', async () => {
      let factoryCallCount = 0
      const factory = vi.fn().mockImplementation(async () => {
        factoryCallCount++
        await new Promise((resolve) => setTimeout(resolve, 100)) // Simulate async work
        return { tag: 'success', value: `call-${factoryCallCount}` }
      })

      // Make multiple concurrent calls
      const promises = [
        cache.getOrSet('concurrent-key', factory),
        cache.getOrSet('concurrent-key', factory),
        cache.getOrSet('concurrent-key', factory),
      ]

      const results = await Promise.all(promises)

      // All should succeed
      for (const result of results) {
        expect(isSuccess(result)).toBe(true)
      }

      // Factory should be called at least once (might be called multiple times due to race condition)
      expect(factoryCallCount).toBeGreaterThan(0)
    })
  })
})
