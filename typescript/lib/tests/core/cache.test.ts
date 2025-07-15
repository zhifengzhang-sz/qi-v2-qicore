/**
 * Unit tests for Cache module
 */

import {
  type CacheConfig,
  MemoryCache,
  RedisCache,
  cacheAside,
  cacheDevelopmentConfig,
  cacheError,
  cacheKey,
  cacheProductionConfig,
  cacheTestConfig,
  createCache,
  getCacheEnvironmentConfig,
  memoryCache,
  namespaced,
  redisCache,
  warmCache,
} from '@qi/core'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

describe('Cache Module', () => {
  describe('MemoryCache', () => {
    let cache: MemoryCache

    beforeEach(() => {
      cache = new MemoryCache({
        backend: 'memory',
        maxSize: 3,
        defaultTtl: 60,
      })
    })

    afterEach(async () => {
      await cache.close()
    })

    test('stores and retrieves values', async () => {
      const result = await cache.set('key1', 'value1')
      expect(result.tag).toBe('success')

      const retrieved = await cache.get<string>('key1')
      expect(retrieved.tag).toBe('success')
      if (retrieved.tag === 'success') {
        expect(retrieved.value).toBe('value1')
      }
    })

    test('returns error for missing keys', async () => {
      const result = await cache.get('nonexistent')

      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('RESOURCE')
      }
    })

    test('supports TTL expiration', async () => {
      await cache.set('key1', 'value1', 0.1) // 100ms TTL

      // Should exist immediately
      const immediate = await cache.get('key1')
      expect(immediate.tag).toBe('success')

      // Wait for expiration
      await new Promise((resolve) => setTimeout(resolve, 150))

      const expired = await cache.get('key1')
      expect(expired.tag).toBe('failure')
    })

    test('deletes entries', async () => {
      await cache.set('key1', 'value1')

      const deleted = await cache.delete('key1')
      expect(deleted.tag).toBe('success')
      if (deleted.tag === 'success') {
        expect(deleted.value).toBe(true)
      }

      const missing = await cache.get('key1')
      expect(missing.tag).toBe('failure')
    })

    test('checks existence', async () => {
      await cache.set('key1', 'value1')

      const exists = await cache.exists('key1')
      expect(exists.tag).toBe('success')
      if (exists.tag === 'success') {
        expect(exists.value).toBe(true)
      }

      const notExists = await cache.exists('key2')
      expect(notExists.tag).toBe('success')
      if (notExists.tag === 'success') {
        expect(notExists.value).toBe(false)
      }
    })

    test('clears all entries', async () => {
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')

      const result = await cache.clear()
      expect(result.tag).toBe('success')

      const stats = cache.getStats()
      expect(stats.size).toBe(0)
    })

    test('gets keys with pattern matching', async () => {
      await cache.set('user:1', 'Alice')
      await cache.set('user:2', 'Bob')
      await cache.set('product:1', 'Widget')

      const allKeys = await cache.keys()
      expect(allKeys.tag).toBe('success')
      if (allKeys.tag === 'success') {
        expect(allKeys.value).toHaveLength(3)
      }

      const userKeys = await cache.keys('user:*')
      expect(userKeys.tag).toBe('success')
      if (userKeys.tag === 'success') {
        expect(userKeys.value).toHaveLength(2)
        expect(userKeys.value).toContain('user:1')
        expect(userKeys.value).toContain('user:2')
      }
    })

    test('implements LRU eviction', async () => {
      // Fill cache to capacity
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')
      await cache.set('key3', 'value3')

      // Access key1 to make it recently used
      await cache.get('key1')

      // Add another key, should evict key2 (least recently used)
      await cache.set('key4', 'value4')

      const key1 = await cache.get('key1') // Should exist
      const key2 = await cache.get('key2') // Should be evicted
      const key3 = await cache.get('key3') // Should exist
      const key4 = await cache.get('key4') // Should exist

      expect(key1.tag).toBe('success')
      expect(key2.tag).toBe('failure')
      expect(key3.tag).toBe('success')
      expect(key4.tag).toBe('success')
    })

    test('tracks statistics', async () => {
      const initialStats = cache.getStats()
      expect(initialStats.hits).toBe(0)
      expect(initialStats.misses).toBe(0)

      await cache.set('key1', 'value1')
      await cache.get('key1') // Hit
      await cache.get('key2') // Miss

      const stats = cache.getStats()
      expect(stats.hits).toBe(1)
      expect(stats.misses).toBe(1)
      expect(stats.sets).toBe(1)
      expect(stats.size).toBe(1)
    })

    test('emits cache events', async () => {
      const hitSpy = vi.fn()
      const missSpy = vi.fn()
      const setSpy = vi.fn()
      const evictSpy = vi.fn()

      cache.on('hit', hitSpy)
      cache.on('miss', missSpy)
      cache.on('set', setSpy)
      cache.on('evict', evictSpy)

      await cache.set('key1', 'value1')
      await cache.get('key1')
      await cache.get('key2')

      expect(setSpy).toHaveBeenCalledWith('key1', 'value1', undefined)
      expect(hitSpy).toHaveBeenCalledWith('key1', 'value1')
      expect(missSpy).toHaveBeenCalledWith('key2')
    })

    test('handles complex data types', async () => {
      const complexData = {
        user: { id: 123, name: 'Alice' },
        preferences: ['theme:dark', 'lang:en'],
        metadata: { lastLogin: new Date('2023-01-01') },
      }

      await cache.set('complex', complexData)
      const result = await cache.get('complex')

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value).toEqual(complexData)
      }
    })

    test('cleans up expired entries automatically', async () => {
      // This test would need to mock the cleanup interval
      // For now, we'll test the manual cleanup logic
      await cache.set('key1', 'value1', 0.1)

      // Wait for expiration
      await new Promise((resolve) => setTimeout(resolve, 150))

      // Access should trigger cleanup
      await cache.get('key1')

      const stats = cache.getStats()
      expect(stats.evictions).toBeGreaterThan(0)
    })
  })

  describe('RedisCache', () => {
    // Note: These tests would require a Redis instance for full integration testing
    // For unit tests, we'll focus on the interface and error handling

    test('creates RedisCache with configuration', () => {
      const config: CacheConfig = {
        backend: 'redis',
        redis: { host: 'localhost', port: 6379 },
      }

      expect(() => new RedisCache(config)).not.toThrow()
    })

    test.skip('handles Redis connection errors', async () => {
      // Skip this test as it requires Redis connection
      // In a real environment with Redis available, this would test connection failures
    })
  })

  describe('Factory Functions', () => {
    test('createCache creates memory cache', () => {
      const config: CacheConfig = { backend: 'memory', maxSize: 100 }
      const result = createCache(config)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value).toBeInstanceOf(MemoryCache)
      }
    })

    test('createCache creates Redis cache', () => {
      const config: CacheConfig = { backend: 'redis' }
      const result = createCache(config)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value).toBeInstanceOf(RedisCache)
      }
    })

    test('createCache handles invalid backend', () => {
      const config = { backend: 'invalid' as any } as CacheConfig
      const result = createCache(config)

      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('RESOURCE')
      }
    })

    test('memoryCache creates memory cache with defaults', () => {
      const cache = memoryCache(50, 120)

      expect(cache).toBeInstanceOf(MemoryCache)
      expect(cache.getStats().maxSize).toBe(50)
    })

    test('redisCache creates Redis cache', () => {
      const result = redisCache({ host: 'localhost' }, 300)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value).toBeInstanceOf(RedisCache)
      }
    })
  })

  describe('Utility Functions', () => {
    test('cacheKey builds cache keys', () => {
      const key = cacheKey('user', 123, 'profile')
      expect(key).toBe('user:123:profile')
    })

    test('namespaced creates namespaced key builder', () => {
      const userKey = namespaced('user')

      expect(userKey(123, 'profile')).toBe('user:123:profile')
      expect(userKey('alice', 'settings')).toBe('user:alice:settings')
    })

    test('warmCache preloads cache entries', async () => {
      const cache = memoryCache(10, 60)
      const loader = vi.fn().mockImplementation(async (key: string) => `value-${key}`)

      const keys = ['key1', 'key2', 'key3']
      const result = await warmCache(cache, keys, loader, 30)

      expect(result.tag).toBe('success')
      expect(loader).toHaveBeenCalledTimes(3)

      // Check that values were cached
      const cached = await cache.get('key1')
      expect(cached.tag).toBe('success')
      if (cached.tag === 'success') {
        expect(cached.value).toBe('value-key1')
      }

      await cache.close()
    })

    test('warmCache skips existing entries', async () => {
      const cache = memoryCache(10, 60)
      const loader = vi.fn().mockImplementation(async (key: string) => `value-${key}`)

      // Pre-populate one key
      await cache.set('key1', 'existing-value')

      const keys = ['key1', 'key2']
      await warmCache(cache, keys, loader, 30)

      // Should only load key2
      expect(loader).toHaveBeenCalledTimes(1)
      expect(loader).toHaveBeenCalledWith('key2')

      await cache.close()
    })

    test('cacheAside implements cache-aside pattern', async () => {
      const cache = memoryCache(10, 60)
      const loader = vi.fn().mockResolvedValue('loaded-value')

      // First call should load from source
      const result1 = await cacheAside(cache, 'key1', loader, 30)
      expect(result1.tag).toBe('success')
      if (result1.tag === 'success') {
        expect(result1.value).toBe('loaded-value')
      }
      expect(loader).toHaveBeenCalledTimes(1)

      // Second call should use cache
      const result2 = await cacheAside(cache, 'key1', loader, 30)
      expect(result2.tag).toBe('success')
      if (result2.tag === 'success') {
        expect(result2.value).toBe('loaded-value')
      }
      expect(loader).toHaveBeenCalledTimes(1) // Still only called once

      await cache.close()
    })

    test('cacheAside handles loader errors', async () => {
      const cache = memoryCache(10, 60)
      const loader = vi.fn().mockRejectedValue(new Error('Load failed'))

      const result = await cacheAside(cache, 'key1', loader, 30)

      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('RESOURCE')
      }

      await cache.close()
    })
  })

  describe('Error Handling', () => {
    test('cacheError creates proper error structure', () => {
      const error = cacheError('Test cache error', {
        operation: 'get',
        key: 'test-key',
        backend: 'memory',
        cache: 'test-cache',
      })

      expect(error.category).toBe('RESOURCE')
      expect(error.message).toBe('Test cache error')
      expect(error.context.operation).toBe('get')
      expect(error.context.key).toBe('test-key')
      expect(error.context.backend).toBe('memory')
      expect(error.context.cache).toBe('test-cache')
    })

    test('MemoryCache handles operation errors gracefully', async () => {
      const cache = new MemoryCache({ backend: 'memory' })

      // Test with complex data
      const complexData = { nested: { deep: { value: 'test' } } }
      const result = await cache.set('key1', complexData)
      expect(result.tag).toBe('success')

      // Should handle gracefully
      const retrieved = await cache.get('key1')
      expect(retrieved.tag).toBe('success')
      if (retrieved.tag === 'success') {
        expect(retrieved.value).toEqual(complexData)
      }

      await cache.close()
    })
  })

  describe('Environment Configurations', () => {
    test('cacheDevelopmentConfig has correct settings', () => {
      expect(cacheDevelopmentConfig.backend).toBe('memory')
      expect(cacheDevelopmentConfig.maxSize).toBe(100)
      expect(cacheDevelopmentConfig.defaultTtl).toBe(300)
      expect(cacheDevelopmentConfig.name).toBe('qicore-dev')
    })

    test('cacheProductionConfig has correct settings', () => {
      expect(cacheProductionConfig.backend).toBe('redis')
      expect(cacheProductionConfig.defaultTtl).toBe(3600)
      expect(cacheProductionConfig.name).toBe('qicore-prod')
      expect(cacheProductionConfig.redis).toBeDefined()
    })

    test('cacheTestConfig has correct settings', () => {
      expect(cacheTestConfig.backend).toBe('memory')
      expect(cacheTestConfig.maxSize).toBe(10)
      expect(cacheTestConfig.defaultTtl).toBe(60)
      expect(cacheTestConfig.name).toBe('qicore-test')
    })

    test('getCacheEnvironmentConfig returns correct config based on NODE_ENV', () => {
      const originalEnv = process.env.NODE_ENV

      try {
        // Test development (default)
        delete process.env.NODE_ENV
        const devConfig = getCacheEnvironmentConfig()
        expect(devConfig).toEqual(cacheDevelopmentConfig)

        // Test production
        process.env.NODE_ENV = 'production'
        const prodConfig = getCacheEnvironmentConfig()
        expect(prodConfig).toEqual(cacheProductionConfig)

        // Test test
        process.env.NODE_ENV = 'test'
        const testEnvConfig = getCacheEnvironmentConfig()
        expect(testEnvConfig).toEqual(cacheTestConfig)
      } finally {
        // Restore original environment
        if (originalEnv !== undefined) {
          process.env.NODE_ENV = originalEnv
        } else {
          delete process.env.NODE_ENV
        }
      }
    })
  })

  describe('Event System', () => {
    test('cache emits appropriate events', async () => {
      const cache = new MemoryCache({ backend: 'memory', maxSize: 2 })

      const hitSpy = vi.fn()
      const missSpy = vi.fn()
      const setSpy = vi.fn()
      const deleteSpy = vi.fn()
      const evictSpy = vi.fn()

      cache.on('hit', hitSpy)
      cache.on('miss', missSpy)
      cache.on('set', setSpy)
      cache.on('delete', deleteSpy)
      cache.on('evict', evictSpy)

      // Test set event
      await cache.set('key1', 'value1', 60)
      expect(setSpy).toHaveBeenCalledWith('key1', 'value1', 60)

      // Test hit event
      await cache.get('key1')
      expect(hitSpy).toHaveBeenCalledWith('key1', 'value1')

      // Test miss event
      await cache.get('nonexistent')
      expect(missSpy).toHaveBeenCalledWith('nonexistent')

      // Test delete event
      await cache.delete('key1')
      expect(deleteSpy).toHaveBeenCalledWith('key1')

      // Test evict event (fill cache and trigger LRU)
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')
      await cache.set('key3', 'value3') // Should evict key1

      expect(evictSpy).toHaveBeenCalledWith('key1', 'lru')

      await cache.close()
    })

    test('supports event listener management', async () => {
      const cache = new MemoryCache({ backend: 'memory' })
      const spy = vi.fn()

      // Add listener
      cache.on('set', spy)
      await cache.set('key1', 'value1')
      expect(spy).toHaveBeenCalledOnce()

      // Remove listener
      cache.off('set', spy)
      await cache.set('key2', 'value2')
      expect(spy).toHaveBeenCalledOnce() // Still only called once

      await cache.close()
    })
  })

  describe('Integration', () => {
    test('cache works with different data types', async () => {
      const cache = memoryCache(10, 60)

      // Test different data types
      await cache.set('string', 'hello')
      await cache.set('number', 42)
      await cache.set('boolean', true)
      await cache.set('array', [1, 2, 3])
      await cache.set('object', { name: 'test' })
      await cache.set('null', null)
      await cache.set('undefined', undefined)

      const string = await cache.get<string>('string')
      const number = await cache.get<number>('number')
      const boolean = await cache.get<boolean>('boolean')
      const array = await cache.get<number[]>('array')
      const object = await cache.get<{ name: string }>('object')
      const nullValue = await cache.get('null')
      const undefinedValue = await cache.get('undefined')

      expect(string.tag).toBe('success')
      expect(number.tag).toBe('success')
      expect(boolean.tag).toBe('success')
      expect(array.tag).toBe('success')
      expect(object.tag).toBe('success')
      expect(nullValue.tag).toBe('success')
      expect(undefinedValue.tag).toBe('success')

      if (string.tag === 'success') expect(string.value).toBe('hello')
      if (number.tag === 'success') expect(number.value).toBe(42)
      if (boolean.tag === 'success') expect(boolean.value).toBe(true)
      if (array.tag === 'success') expect(array.value).toEqual([1, 2, 3])
      if (object.tag === 'success') expect(object.value).toEqual({ name: 'test' })
      if (nullValue.tag === 'success') expect(nullValue.value).toBe(null)
      if (undefinedValue.tag === 'success') expect(undefinedValue.value).toBe(undefined)

      await cache.close()
    })

    test('cache statistics track operations correctly', async () => {
      const cache = memoryCache(5, 60)

      // Perform various operations
      await cache.set('key1', 'value1')
      await cache.set('key2', 'value2')
      await cache.get('key1') // hit
      await cache.get('key1') // hit
      await cache.get('key3') // miss
      await cache.delete('key2')

      const stats = cache.getStats()

      expect(stats.sets).toBe(2)
      expect(stats.hits).toBe(2)
      expect(stats.misses).toBe(1)
      expect(stats.deletes).toBe(1)
      expect(stats.size).toBe(1) // Only key1 remains

      await cache.close()
    })

    test('cache works with cache-aside pattern in real scenario', async () => {
      const cache = memoryCache(10, 60)

      // Simulate database
      const database = new Map([
        ['user:1', { id: 1, name: 'Alice' }],
        ['user:2', { id: 2, name: 'Bob' }],
      ])

      const getUserFromDb = async (key: string) => {
        const user = database.get(key)
        if (!user) throw new Error('User not found')
        return user
      }

      // First access - should load from "database"
      const user1 = await cacheAside(cache, 'user:1', () => getUserFromDb('user:1'))
      expect(user1.tag).toBe('success')

      // Second access - should use cache
      const user1Cached = await cacheAside(cache, 'user:1', () => getUserFromDb('user:1'))
      expect(user1Cached.tag).toBe('success')

      // Verify cache contains the data
      const cachedDirectly = await cache.get('user:1')
      expect(cachedDirectly.tag).toBe('success')

      await cache.close()
    })
  })
})
