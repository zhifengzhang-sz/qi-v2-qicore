#!/usr/bin/env node

import { createMemoryCache, createRedisCache } from '@qi/core'
import { createLogger } from '@qi/core'
import { match } from '@qi/base'

async function main() {
  const loggerResult = createLogger({ level: 'info', pretty: true })
  if (loggerResult.tag === 'failure') throw new Error('Logger failed')
  const logger = loggerResult.value

  console.log('🚀 QiCore Foundation - Cache Examples')
  console.log('=====================================\n')

  // === Memory Cache Example ===
  logger.info('=== Memory Cache Example ===')

  const memoryCache = createMemoryCache({
    maxSize: 100,
    defaultTtl: 60,
  })

  // Set some values
  await memoryCache.set('user:123', { id: '123', name: 'John', email: 'john@example.com' })
  await memoryCache.set('user:456', { id: '456', name: 'Jane', email: 'jane@example.com' }, 30) // 30 sec TTL
  await memoryCache.set('config:app', { theme: 'dark', language: 'en' })

  // Get values
  const user123Result = await memoryCache.get('user:123')
  const user456Result = await memoryCache.get('user:456')
  const configResult = await memoryCache.get('config:app')

  match(
    (user) => logger.info('Found user 123:', { user }),
    (error) => logger.error('User 123 not found:', undefined, { error: error.message }),
    user123Result
  )

  match(
    (user) => logger.info('Found user 456:', { user }),
    (error) => logger.error('User 456 not found:', undefined, { error: error.message }),
    user456Result
  )

  match(
    (config) => logger.info('Found config:', { config }),
    (error) => logger.error('Config not found:', undefined, { error: error.message }),
    configResult
  )

  // Check existence
  const existsResult = await memoryCache.exists('user:123')
  match(
    (exists) => logger.info('User 123 exists:', { exists }),
    (error) => logger.error('Existence check failed:', undefined, { error: error.message }),
    existsResult
  )

  // List keys
  const keysResult = await memoryCache.keys('user:*')
  match(
    (keys) => logger.info('User keys:', { keys }),
    (error) => logger.error('Keys listing failed:', undefined, { error: error.message }),
    keysResult
  )

  // Show stats
  const memoryStats = memoryCache.getStats()
  logger.info('Memory cache stats:', { memoryStats })

  console.log()

  // === Redis Cache Example ===
  logger.info('=== Redis Cache Example ===')

  try {
    const redisCache = createRedisCache({
      redis: {
        host: 'localhost',
        port: 6379,
        lazyConnect: true,
      },
    })

    // Set values in Redis
    await redisCache.set('session:abc123', {
      userId: '123',
      role: 'admin',
      expires: Date.now() + 3600000,
    })
    await redisCache.set('session:def456', {
      userId: '456',
      role: 'user',
      expires: Date.now() + 3600000,
    })

    // Get values from Redis
    const sessionResult = await redisCache.get('session:abc123')
    match(
      (session) => logger.info('Found session:', { session }),
      (error) => logger.error('Session not found:', undefined, { error: error.message }),
      sessionResult
    )

    // Multi-get operation
    const multiResult = await redisCache.mget(['session:abc123', 'session:def456'])
    match(
      (sessions) => logger.info('Multi-get results:', { sessions }),
      (error) => logger.error('Multi-get failed:', undefined, { error: error.message }),
      multiResult
    )

    // List Redis keys
    const redisKeysResult = await redisCache.keys('session:*')
    match(
      (keys) => logger.info('Redis session keys:', { keys }),
      (error) => logger.error('Redis keys listing failed:', undefined, { error: error.message }),
      redisKeysResult
    )

    // Show Redis stats
    const redisStats = redisCache.getStats()
    logger.info('Redis cache stats:', { redisStats })

    // Clean up
    await redisCache.delete('session:abc123')
    await redisCache.delete('session:def456')
    await redisCache.close()
  } catch (error) {
    logger.warn('Redis not available - skipping Redis examples:', {
      error: error instanceof Error ? error.message : 'Unknown error',
      hint: 'Start Redis with: docker run -p 6379:6379 redis:alpine',
    })
  }

  console.log()

  // === TTL Example ===
  logger.info('=== TTL (Time-To-Live) Example ===')

  // Set with short TTL
  await memoryCache.set('temp:data', { message: 'This will expire soon' }, 2)

  // Check immediately
  const tempResult1 = await memoryCache.get('temp:data')
  match(
    (data) => logger.info('Temp data (immediate):', { data }),
    (error) => logger.error('Temp data not found:', undefined, { error: error.message }),
    tempResult1
  )

  // Wait 3 seconds and check again
  logger.info('Waiting 3 seconds for TTL expiration...')
  await new Promise((resolve) => setTimeout(resolve, 3000))

  const tempResult2 = await memoryCache.get('temp:data')
  match(
    (data) => logger.info('Temp data (after 3s):', { data }),
    (error) => logger.info('Temp data expired as expected:', { message: error.message }),
    tempResult2
  )

  console.log()

  // === Cache Pattern Example ===
  logger.info('=== Cache Pattern Example ===')

  // Simulate expensive operation
  async function expensiveUserLookup(id: string) {
    logger.info('Performing expensive user lookup...', { id })
    await new Promise((resolve) => setTimeout(resolve, 1000)) // Simulate delay
    return { id, name: `User ${id}`, email: `user${id}@example.com`, computed: Date.now() }
  }

  // Cache-aside pattern
  async function getUserWithCache(id: string) {
    const cacheKey = `user:${id}`

    // Try cache first
    const cachedResult = await memoryCache.get(cacheKey)
    if (cachedResult.tag === 'success') {
      logger.info('Cache HIT:', { id, source: 'cache' })
      return cachedResult.value
    }

    // Cache miss - fetch from "database"
    logger.info('Cache MISS:', { id, source: 'database' })
    const user = await expensiveUserLookup(id)

    // Store in cache for next time
    await memoryCache.set(cacheKey, user, 60) // 60 second TTL

    return user
  }

  // Demonstrate cache pattern
  const start1 = Date.now()
  const user1 = await getUserWithCache('789')
  const time1 = Date.now() - start1
  logger.info('First lookup (cache miss):', { user: user1, timeMs: time1 })

  const start2 = Date.now()
  const user2 = await getUserWithCache('789')
  const time2 = Date.now() - start2
  logger.info('Second lookup (cache hit):', { user: user2, timeMs: time2 })

  // Final stats
  const finalStats = memoryCache.getStats()
  logger.info('Final cache stats:', { finalStats })

  console.log()
  logger.info('✨ Cache examples completed!')
  logger.info('💡 Try running with Redis: docker run -p 6379:6379 redis:alpine')
}

main().catch(console.error)
