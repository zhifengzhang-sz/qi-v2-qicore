/**
 * Integration tests for Core modules
 * Tests how Config, Logger, and Cache work together
 */

import { mkdir, unlink, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import {
  AppConfigSchema,
  // Config
  ConfigBuilder,
} from '@qi/core'
import {
  type ICache,
  type Logger,
  type LoggerConfig,
  // Logger
  createLogger,
  formatQiError,
} from '@qi/core'
import {
  type CacheBackend,
  type CacheConfig,
  cacheAside,
  // Cache
  createCache,
} from '@qi/core'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { z } from 'zod'

// Test configuration schemas
const LoggingConfigSchema = z.object({
  level: z.enum(['debug', 'info', 'warn', 'error']),
  pretty: z.boolean(),
})

const CacheConfigSchema = z.object({
  backend: z.enum(['memory', 'redis']),
  maxSize: z.number().optional(),
  defaultTtl: z.number().optional(),
})

describe('Core Module Integration', () => {
  let tempDir: string
  let testFiles: string[] = []

  beforeEach(async () => {
    tempDir = join(tmpdir(), 'qi-integration-test')
    await mkdir(tempDir, { recursive: true })
    testFiles = []
  })

  afterEach(async () => {
    // Clean up test files
    for (const file of testFiles) {
      try {
        await unlink(file)
      } catch {}
    }
  })

  const createTestFile = async (filename: string, content: string): Promise<string> => {
    const filepath = join(tempDir, filename)
    await writeFile(filepath, content)
    testFiles.push(filepath)
    return filepath
  }

  describe('Configuration-driven Service Initialization', () => {
    test('initializes logger from configuration', async () => {
      const appConfig = {
        app: {
          name: 'test-app',
          version: '1.0.0',
          environment: 'test' as const,
        },
        logging: {
          level: 'info' as const,
          pretty: false,
        },
      }

      const config = ConfigBuilder.fromObject(appConfig).validateWith(AppConfigSchema).build()

      expect(config.tag).toBe('success')
      if (config.tag === 'success') {
        const loggingConfig = config.value.get('logging')
        expect(loggingConfig.tag).toBe('success')

        if (loggingConfig.tag === 'success') {
          const parsedLogging = LoggingConfigSchema.safeParse(loggingConfig.value)
          if (!parsedLogging.success) {
            throw new Error('Invalid logging config')
          }
          const loggerConfig: LoggerConfig = {
            level: parsedLogging.data.level,
            pretty: parsedLogging.data.pretty,
            name: appConfig.app.name,
          }

          const loggerResult = createLogger(loggerConfig)
          expect(loggerResult.tag).toBe('success')

          if (loggerResult.tag === 'success') {
            expect(loggerResult.value.getLevel()).toBe('info')
            expect(loggerResult.value.getConfig().name).toBe('test-app')
          }
        }
      }
    })

    test('initializes cache from configuration', async () => {
      const cacheConfigData = {
        cache: {
          backend: 'memory' as const,
          maxSize: 100,
          defaultTtl: 300,
        },
      }

      const config = ConfigBuilder.fromObject(cacheConfigData).buildUnsafe()
      const cacheConfigResult = config.get('cache')

      expect(cacheConfigResult.tag).toBe('success')
      if (cacheConfigResult.tag === 'success') {
        const parsedCache = CacheConfigSchema.safeParse(cacheConfigResult.value)
        if (!parsedCache.success) {
          throw new Error('Invalid cache config')
        }
        const cacheConfig: CacheConfig = {
          backend: parsedCache.data.backend,
          maxSize: parsedCache.data.maxSize,
          defaultTtl: parsedCache.data.defaultTtl,
        }

        const cacheResult = createCache(cacheConfig)
        expect(cacheResult.tag).toBe('success')

        if (cacheResult.tag === 'success') {
          const stats = cacheResult.value.getStats()
          expect(stats.maxSize).toBe(100)
        }
      }
    })

    test('loads configuration from file and initializes services', async () => {
      const configData = {
        app: {
          name: 'file-config-app',
          version: '2.0.0',
          environment: 'development',
          port: 4000,
        },
        logging: {
          level: 'debug',
          pretty: true,
        },
        cache: {
          backend: 'memory',
          maxSize: 50,
          defaultTtl: 600,
        },
      }

      const configFile = await createTestFile('app-config.json', JSON.stringify(configData))
      const configResult = await ConfigBuilder.fromJsonFile(configFile)

      expect(configResult.tag).toBe('success')
      if (configResult.tag === 'success') {
        const config = configResult.value
          .validateWith(
            AppConfigSchema.extend({
              cache: z.object({
                backend: z.literal('memory'),
                maxSize: z.number(),
                defaultTtl: z.number(),
              }),
            })
          )
          .build()

        expect(config.tag).toBe('success')
        if (config.tag === 'success') {
          // Initialize logger
          const loggingConfigResult = config.value.get('logging')
          const loggerResult =
            loggingConfigResult.tag === 'success'
              ? (() => {
                  const parsedLogging = LoggingConfigSchema.safeParse(loggingConfigResult.value)
                  if (!parsedLogging.success) {
                    throw new Error('Invalid logging config')
                  }
                  return createLogger({
                    level: parsedLogging.data.level,
                    pretty: parsedLogging.data.pretty,
                    name: configData.app.name,
                  })
                })()
              : null

          // Initialize cache
          const cacheConfigResult = config.value.get('cache')
          const cacheResult =
            cacheConfigResult.tag === 'success'
              ? (() => {
                  const parsedCache = CacheConfigSchema.safeParse(cacheConfigResult.value)
                  if (!parsedCache.success) {
                    throw new Error('Invalid cache config')
                  }
                  return createCache({
                    backend: parsedCache.data.backend,
                    maxSize: parsedCache.data.maxSize,
                    defaultTtl: parsedCache.data.defaultTtl,
                  })
                })()
              : null

          expect(loggerResult?.tag).toBe('success')
          expect(cacheResult?.tag).toBe('success')

          if (loggerResult?.tag === 'success') {
            expect(loggerResult.value.getLevel()).toBe('debug')
          }

          if (cacheResult?.tag === 'success') {
            const stats = cacheResult.value.getStats()
            expect(stats.maxSize).toBe(50)
          }
        }
      }
    })
  })

  describe('Error Handling Integration', () => {
    test('logs configuration errors using logger', async () => {
      const logger = createLogger({ level: 'error', name: 'config-test' })
      expect(logger.tag).toBe('success')

      if (logger.tag === 'success') {
        const logSpy = vi.fn()
        logger.value.on('log', logSpy)

        // Create an invalid configuration
        const invalidConfig = {
          app: {
            name: 'test',
            version: '1.0.0',
            port: 'invalid-port', // Should be number
          },
        }

        const configResult = ConfigBuilder.fromObject(invalidConfig)
          .validateWith(AppConfigSchema)
          .build()

        expect(configResult.tag).toBe('failure')
        if (configResult.tag === 'failure') {
          const formattedError = formatQiError(configResult.error)
          logger.value.error(
            'Configuration validation failed',
            configResult.error as unknown as Error,
            formattedError
          )

          expect(logSpy).toHaveBeenCalled()
          const logEntry = logSpy.mock.calls[0][0]
          expect(logEntry.level).toBe('error')
          expect(logEntry.message).toBe('Configuration validation failed')
          expect(logEntry.error).toBe(configResult.error)
        }
      }
    })

    test('caches configuration after validation', async () => {
      const cache = createCache({ backend: 'memory', maxSize: 10 })
      expect(cache.tag).toBe('success')

      if (cache.tag === 'success') {
        const configData = {
          app: { name: 'cached-app', version: '1.0.0' },
          database: {
            host: 'localhost',
            port: 5432,
            name: 'testdb',
            user: 'admin',
            password: 'secret',
          },
        }

        // Cache configuration loader
        const loadConfig = async (): Promise<typeof configData> => {
          // Simulate expensive configuration loading
          await new Promise((resolve) => setTimeout(resolve, 10))
          return configData
        }

        // First load - should call loader
        const result1 = await cacheAside(cache.value, 'app-config', loadConfig, 60)
        expect(result1.tag).toBe('success')

        // Second load - should use cache
        const result2 = await cacheAside(cache.value, 'app-config', loadConfig, 60)
        expect(result2.tag).toBe('success')

        // Verify both results are the same
        if (result1.tag === 'success' && result2.tag === 'success') {
          expect(result1.value).toEqual(result2.value)
        }

        // Verify cache statistics
        const stats = cache.value.getStats()
        expect(stats.hits).toBe(1) // Second call was a cache hit
        expect(stats.sets).toBe(1) // Only set once
      }
    })
  })

  describe('Service Composition', () => {
    test('creates application with all services integrated', async () => {
      // Application configuration
      const appConfigData = {
        app: {
          name: 'integrated-app',
          version: '3.0.0',
          environment: 'test' as const,
          port: 3000,
        },
        logging: {
          level: 'info' as const,
          pretty: false,
        },
        cache: {
          backend: 'memory' as const,
          maxSize: 200,
          defaultTtl: 1800,
        },
      }

      // Load and validate configuration
      const config = ConfigBuilder.fromObject(appConfigData)
        .validateWith(
          AppConfigSchema.extend({
            cache: z.object({
              backend: z.literal('memory'),
              maxSize: z.number(),
              defaultTtl: z.number(),
            }),
          })
        )
        .build()

      expect(config.tag).toBe('success')
      if (config.tag !== 'success') return

      // Initialize services
      const services: {
        logger: Logger | null
        cache: ICache | null
      } = {
        logger: null,
        cache: null,
      }

      // Initialize logger
      const loggingConfig = config.value.get('logging')
      if (loggingConfig.tag === 'success') {
        const parsedLogging = LoggingConfigSchema.safeParse(loggingConfig.value)
        if (!parsedLogging.success) {
          throw new Error('Invalid logging config')
        }
        const loggerResult = createLogger({
          level: parsedLogging.data.level,
          pretty: parsedLogging.data.pretty,
          name: appConfigData.app.name,
        })

        if (loggerResult.tag === 'success') {
          services.logger = loggerResult.value
        }
      }

      // Initialize cache
      const cacheConfig = config.value.get('cache')
      if (cacheConfig.tag === 'success') {
        const parsedCache = CacheConfigSchema.safeParse(cacheConfig.value)
        if (!parsedCache.success) {
          throw new Error('Invalid cache config')
        }
        const cacheResult = createCache({
          backend: parsedCache.data.backend,
          maxSize: parsedCache.data.maxSize,
          defaultTtl: parsedCache.data.defaultTtl,
        })

        if (cacheResult.tag === 'success') {
          services.cache = cacheResult.value
        }
      }

      // Test integrated functionality
      expect(services.logger).toBeDefined()
      expect(services.cache).toBeDefined()

      if (!services.logger || !services.cache) {
        throw new Error('Services not initialized')
      }

      // Use logger to log cache operations
      const logSpy = vi.fn()
      services.logger.on('log', logSpy)

      // Perform cache operations with logging
      services.logger.info('Storing user data in cache')
      await services.cache.set('user:123', { name: 'Alice', role: 'admin' })

      services.logger.info('Retrieving user data from cache')
      const userData = await services.cache.get('user:123')

      expect(userData.tag).toBe('success')
      expect(logSpy).toHaveBeenCalledTimes(2)

      // Clean up
      await services.cache.close()
      await services.logger.close()
    })

    test('handles service initialization failures gracefully', async () => {
      const logger = createLogger({ level: 'error', name: 'failure-test' })
      expect(logger.tag).toBe('success')

      if (logger.tag === 'success') {
        const logSpy = vi.fn()
        logger.value.on('log', logSpy)

        // Try to create cache with invalid configuration
        const invalidCacheConfig: CacheConfig = {
          backend: 'invalid-backend' as unknown as CacheBackend,
        }

        const cacheResult = createCache(invalidCacheConfig)
        expect(cacheResult.tag).toBe('failure')

        if (cacheResult.tag === 'failure') {
          // Log the service initialization failure
          logger.value.error(
            'Failed to initialize cache service',
            cacheResult.error as unknown as Error
          )

          expect(logSpy).toHaveBeenCalled()
          const logEntry = logSpy.mock.calls[0][0]
          expect(logEntry.level).toBe('error')
          expect(logEntry.message).toBe('Failed to initialize cache service')
        }

        await logger.value.close()
      }
    })
  })

  describe('Configuration Merging and Environment Overrides', () => {
    test('merges configuration from multiple sources', async () => {
      // Base configuration file
      const baseConfig = {
        app: {
          name: 'multi-source-app',
          version: '1.0.0',
          environment: 'development',
        },
        logging: {
          level: 'debug',
        },
      }

      // Environment-specific overrides
      const envOverrides = {
        app: {
          environment: 'production',
        },
        logging: {
          level: 'error',
          pretty: false,
        },
        cache: {
          backend: 'memory',
          maxSize: 500,
        },
      }

      const baseConfigFile = await createTestFile('base.json', JSON.stringify(baseConfig))
      const baseConfigResult = await ConfigBuilder.fromJsonFile(baseConfigFile)

      expect(baseConfigResult.tag).toBe('success')
      if (baseConfigResult.tag === 'success') {
        const finalConfig = baseConfigResult.value.mergeObject(envOverrides).build()

        expect(finalConfig.tag).toBe('success')
        if (finalConfig.tag === 'success') {
          // Debug: log the merged config structure
          const _allData = finalConfig.value.getAll()

          // Verify merging worked correctly
          expect(finalConfig.value.get('app.name').tag).toBe('success')
          expect(finalConfig.value.get('app.environment').tag).toBe('success')
          expect(finalConfig.value.get('logging.level').tag).toBe('success')
          expect(finalConfig.value.get('cache.backend').tag).toBe('success')

          const appEnvResult = finalConfig.value.get('app.environment')
          if (appEnvResult.tag === 'success') {
            expect(appEnvResult.value).toBe('production')
          }
          const logLevelResult = finalConfig.value.get('logging.level')
          if (logLevelResult.tag === 'success') {
            expect(logLevelResult.value).toBe('error')
          }
        }
      }
    })

    test('uses environment variables to override configuration', () => {
      // Set environment variables
      process.env.MYAPP_LOG_LEVEL = 'warn'
      process.env.MYAPP_CACHE_SIZE = '1000'
      process.env.MYAPP_APP_PORT = '8080'

      const baseConfig = {
        app: { name: 'env-app', version: '1.0.0', port: 3000 },
        logging: { level: 'info' },
        cache: { maxSize: 100 },
      }

      // Load environment overrides
      const envConfig = ConfigBuilder.fromEnv('MYAPP')
      const mergedConfig = ConfigBuilder.fromObject(baseConfig)
        .merge(envConfig)
        .transform((data: Record<string, unknown>) => ({
          ...data,
          // Transform environment strings to appropriate types
          app: {
            ...(data.app || {}),
            port: data.app_port
              ? Number.parseInt(data.app_port as string)
              : ((data.app as Record<string, unknown>)?.port as number),
          },
          logging: {
            ...(data.logging || {}),
            level: data.log_level || ((data.logging as Record<string, unknown>)?.level as string),
          },
          cache: {
            ...(data.cache || {}),
            maxSize: data.cache_size
              ? Number.parseInt(data.cache_size as string)
              : ((data.cache as Record<string, unknown>)?.maxSize as number),
          },
        }))
        .buildUnsafe()

      expect(mergedConfig.getOr('app.port', 0)).toBe(8080)
      expect(mergedConfig.getOr('logging.level', 'info')).toBe('warn')
      expect(mergedConfig.getOr('cache.maxSize', 0)).toBe(1000)

      // Clean up environment variables
      process.env.MYAPP_LOG_LEVEL = undefined
      process.env.MYAPP_CACHE_SIZE = undefined
      process.env.MYAPP_APP_PORT = undefined
    })
  })

  describe('Real-world Usage Patterns', () => {
    test('implements request-scoped logging with cache', async () => {
      // Initialize services
      const logger = createLogger({ level: 'info', name: 'request-app' })
      const cache = createCache({ backend: 'memory', maxSize: 100 })

      expect(logger.tag).toBe('success')
      expect(cache.tag).toBe('success')

      if (logger.tag !== 'success' || cache.tag !== 'success') return

      // Simulate request processing
      const requestId = `req-${Date.now()}`
      const requestLogger = logger.value.child({ requestId })

      const logSpy = vi.fn()
      requestLogger.on('log', logSpy)

      // Log request start
      requestLogger.info('Processing user request', { userId: 'user-123', action: 'get-profile' })

      // Check cache first
      requestLogger.debug('Checking cache for user profile')
      const cachedProfile = await cache.value.get('profile:user-123')

      if (cachedProfile.tag === 'failure') {
        // Cache miss - load from "database"
        requestLogger.info('Cache miss, loading from database')

        const profileData = { id: 'user-123', name: 'Alice', email: 'alice@example.com' }

        // Store in cache
        await cache.value.set('profile:user-123', profileData, 300)
        requestLogger.debug('Profile cached for 5 minutes')

        // Log completion
        requestLogger.info('Request completed successfully', { fromCache: false })
      } else {
        // Cache hit
        requestLogger.info('Request completed successfully', { fromCache: true })
      }

      // Verify logging occurred
      expect(logSpy).toHaveBeenCalled()
      const logEntries = logSpy.mock.calls.map((call) => call[0])
      expect(logEntries.some((entry) => entry.context?.requestId === requestId)).toBe(true)

      // Clean up
      await cache.value.close()
      await logger.value.close()
    })

    test('handles configuration hot-reload simulation', async () => {
      let currentConfig: Record<string, unknown> = {
        app: { name: 'hot-reload-app', version: '1.0.0' },
        logging: { level: 'info' as const },
        cache: { maxSize: 100 },
      }

      // Initial setup
      let config = ConfigBuilder.fromObject(currentConfig).buildUnsafe()
      const logger = createLogger({
        level: config.getOr('logging.level', 'info'),
        name: config.getOr('app.name', 'app'),
      })

      expect(logger.tag).toBe('success')
      if (logger.tag !== 'success') return

      expect(logger.value.getLevel()).toBe('info')

      // Simulate configuration change
      currentConfig = {
        ...currentConfig,
        logging: { level: 'debug' as const },
      }

      // Reload configuration
      config = ConfigBuilder.fromObject(currentConfig).buildUnsafe()

      // Update logger level
      const newLevel = config.getOr('logging.level', 'info')
      logger.value.setLevel(newLevel)

      expect(logger.value.getLevel()).toBe('debug')

      await logger.value.close()
    })
  })
})
