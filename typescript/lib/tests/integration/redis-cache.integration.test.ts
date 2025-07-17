/**
 * Redis Cache Integration Tests
 * Tests actual Redis connectivity and functionality
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { createRedisCache, type ICache } from '@qi/core'
import { isSuccess, isFailure } from '@qi/base'

describe('Redis Cache Integration', () => {
  let cache: ICache
  const testKey = 'integration-test-key'
  const testValue = 'integration-test-value'

  beforeAll(async () => {
    // Create Redis cache with connection to running Redis instance
    cache = createRedisCache({
      redis: {
        host: 'localhost',
        port: 6379,
        lazyConnect: true,
      },
    })

    // Wait a moment for connection
    await new Promise((resolve) => setTimeout(resolve, 100))
  })

  afterAll(async () => {
    // Clean up test data
    if (cache) {
      await cache.delete(testKey)
      await cache.clear()
      await cache.close()
    }
  })

  it('connects to Redis successfully', async () => {
    // This will fail if Redis is not running or accessible
    const result = await cache.set(testKey, testValue)
    expect(isSuccess(result)).toBe(true)
  })

  it('sets and gets values from Redis', async () => {
    const setResult = await cache.set(`${testKey}-get`, testValue)
    expect(isSuccess(setResult)).toBe(true)

    const getResult = await cache.get(`${testKey}-get`)
    expect(isSuccess(getResult)).toBe(true)
    if (getResult.tag === 'success') {
      expect(getResult.value).toBe(testValue)
    }
  })

  it('handles TTL expiration with Redis', async () => {
    const shortTtlKey = `${testKey}-ttl`

    // Set with 1 second TTL
    const setResult = await cache.set(shortTtlKey, testValue, 1)
    expect(isSuccess(setResult)).toBe(true)

    // Should exist immediately
    const immediateResult = await cache.get(shortTtlKey)
    expect(isSuccess(immediateResult)).toBe(true)

    // Wait for expiration
    await new Promise((resolve) => setTimeout(resolve, 1100))

    // Should be expired
    const expiredResult = await cache.get(shortTtlKey)
    expect(isFailure(expiredResult)).toBe(true)
  })

  it('deletes keys from Redis', async () => {
    const deleteKey = `${testKey}-delete`

    await cache.set(deleteKey, testValue)
    const deleteResult = await cache.delete(deleteKey)
    expect(isSuccess(deleteResult)).toBe(true)

    const getResult = await cache.get(deleteKey)
    expect(isFailure(getResult)).toBe(true)
  })

  it('checks key existence in Redis', async () => {
    const existsKey = `${testKey}-exists`

    // Should not exist initially
    const notExistsResult = await cache.exists(existsKey)
    expect(isSuccess(notExistsResult)).toBe(true)
    if (notExistsResult.tag === 'success') {
      expect(notExistsResult.value).toBe(false)
    }

    // Set the key
    await cache.set(existsKey, testValue)

    // Should exist now
    const existsResult = await cache.exists(existsKey)
    expect(isSuccess(existsResult)).toBe(true)
    if (existsResult.tag === 'success') {
      expect(existsResult.value).toBe(true)
    }
  })

  it('lists keys with pattern matching in Redis', async () => {
    const patternKey1 = 'integration:test:1'
    const patternKey2 = 'integration:test:2'
    const otherKey = 'other:key'

    await cache.set(patternKey1, testValue)
    await cache.set(patternKey2, testValue)
    await cache.set(otherKey, testValue)

    const keysResult = await cache.keys('integration:*')
    expect(isSuccess(keysResult)).toBe(true)

    if (keysResult.tag === 'success') {
      expect(keysResult.value).toContain(patternKey1)
      expect(keysResult.value).toContain(patternKey2)
      expect(keysResult.value).not.toContain(otherKey)
    }

    // Clean up
    await cache.delete(patternKey1)
    await cache.delete(patternKey2)
    await cache.delete(otherKey)
  })

  it('handles Redis connection errors gracefully', async () => {
    // Create cache with invalid port to test error handling
    const badCache = createRedisCache({
      redis: {
        host: 'localhost',
        port: 9999, // Invalid port
        connectTimeout: 500,
        commandTimeout: 500,
        lazyConnect: true,
        maxRetriesPerRequest: 1,
      },
    })

    // Should fail gracefully
    const result = await badCache.set('test', 'value')
    expect(isFailure(result)).toBe(true)

    await badCache.close()
  }, 5000)

  it('retrieves cache statistics', () => {
    const stats = cache.getStats()

    expect(typeof stats.hits).toBe('number')
    expect(typeof stats.misses).toBe('number')
    expect(typeof stats.sets).toBe('number')
    expect(typeof stats.deletes).toBe('number')
    expect(typeof stats.evictions).toBe('number')
    expect(typeof stats.size).toBe('number')

    // Redis cache should have some operations recorded
    expect(stats.sets).toBeGreaterThan(0)
  })
})
