/**
 * QiCore Foundation - Core Module Exports
 *
 * Infrastructure services for configuration, logging, and caching
 * with TypeScript-native patterns and Result<T> integration.
 */

// Configuration Module
export {
  // Core classes
  ConfigBuilder,
  Config,
  // Types
  type ConfigData,
  type ConfigSource,
  type ConfigOptions,
  type ConfigError,
  // Factory functions
  fromObject,
  fromJsonFile,
  fromYamlFile,
  fromTomlFile,
  fromEnv,
  empty,
  // Validation utilities
  validateConfig,
  safeParseConfig,
  // ValidatedConfig class
  ValidatedConfig,
  ConfigAccessError,
  // Error factory
  configError,
  // Common schemas
  AppConfigSchema,
  type AppConfig,
} from './config.js'

// Logger Module
export {
  // Core class
  Logger,
  // Types
  type LogLevel,
  type LogEntry,
  type LoggerConfig,
  type LoggerContext,
  type LoggerEvents,
  type LoggerError,
  // Factory functions
  createLogger,
  createFromEnv,
  // Utilities
  formatError,
  createRequestLogger,
  // Error factory
  loggerError,
  // Configurations
  developmentConfig as loggerDevelopmentConfig,
  productionConfig as loggerProductionConfig,
  testConfig as loggerTestConfig,
  getEnvironmentConfig as getLoggerEnvironmentConfig,
} from './logger.js'

// Cache Module
export {
  // Core classes
  MemoryCache,
  RedisCache,
  // Interface
  type ICache,
  // Types
  type CacheBackend,
  type CacheEntry,
  type CacheConfig,
  type CacheStats,
  type CacheEvents,
  type CacheError,
  // Factory functions
  createCache,
  createMemoryCache,
  createRedisCache,
  createPersistent,
  // Utilities
  cacheAside,
  // Error factory
  cacheError,
} from './cache.js'

// Note: Result and QiError are available from @qi/base
// They are not re-exported here to avoid naming conflicts when both modules are imported
