Here's the config.ts implementation following the fluent builder pattern:

```typescript
/**
 * QiCore Foundation Core - Configuration Implementation
 *
 * Fluent builder pattern for procedural configuration loading.
 * Supports multiple sources with immutable operations.
 */

import {
  type Result,
  type QiError,
  success,
  failure,
  create as createError,
  fromTryCatch
} from '@qi/base'
import { readFileSync, existsSync } from 'fs'

// ============================================================================
// Types
// ============================================================================

/**
 * Configuration data structure
 */
export interface ConfigData {
  readonly [key: string]: unknown
}

/**
 * Schema for validation (generic interface)
 */
export interface Schema {
  validate(data: ConfigData): { valid: boolean; errors?: string[] }
}

// ============================================================================
// ConfigBuilder - Fluent API
// ============================================================================

/**
 * Fluent configuration builder
 * Uses throw for fail-fast behavior as configs should fail early
 */
export class ConfigBuilder {
  private constructor(private readonly data: ConfigData) {}

  /**
   * Create configuration from object
   * Contract: validates object structure
   * Contract: preserves nested structure
   */
  static fromObject(obj: object): ConfigBuilder {
    try {
      // Deep clone and freeze for immutability
      const cloned = JSON.parse(JSON.stringify(obj))
      return new ConfigBuilder(deepFreeze(cloned))
    } catch (error) {
      throw new Error(`Invalid configuration object: ${error}`)
    }
  }

  /**
   * Load configuration from file
   * Contract: supports common configuration formats
   * Contract: auto-detects format from extension
   */
  static fromFile(filePath: string): ConfigBuilder {
    if (!existsSync(filePath)) {
      throw new Error(`Configuration file not found: ${filePath}`)
    }

    try {
      const content = readFileSync(filePath, 'utf-8')
      const data = parseConfigFile(filePath, content)
      return ConfigBuilder.fromObject(data)
    } catch (error) {
      throw new Error(`Failed to load configuration from ${filePath}: ${error}`)
    }
  }

  /**
   * Load configuration from environment variables
   * Contract: prefix filters environment variables
   * Contract: converts ENV_VAR_NAME to nested.var.name structure
   */
  static fromEnvironment(prefix: string = ''): ConfigBuilder {
    const config: ConfigData = {}
    const prefixUpper = prefix.toUpperCase()
    
    for (const [key, value] of Object.entries(process.env)) {
      if (!prefix || key.toUpperCase().startsWith(prefixUpper)) {
        const configKey = envKeyToConfigKey(key, prefixUpper)
        setNestedValue(config, configKey, parseEnvValue(value || ''))
      }
    }
    
    return new ConfigBuilder(deepFreeze(config))
  }

  /**
   * Create empty configuration
   * Contract: has(anyKey, empty()) == false
   */
  static empty(): ConfigBuilder {
    return new ConfigBuilder({})
  }

  /**
   * Merge multiple configurations
   * Contract: right-biased (later configs override earlier)
   * Contract: deep merge for nested objects
   * Contract: associative operation
   */
  merge(...others: ConfigBuilder[]): ConfigBuilder {
    const configs = [this.data, ...others.map(b => b.data)]
    const merged = configs.reduce((acc, config) => deepMerge(acc, config), {})
    return new ConfigBuilder(deepFreeze(merged))
  }

  /**
   * Validate configuration against schema
   * Contract: returns original config if valid
   * Contract: throws with detailed error if invalid
   */
  validate(schema: Schema): ConfigBuilder {
    const result = schema.validate(this.data)
    
    if (!result.valid) {
      const errors = result.errors || ['Validation failed']
      throw new Error(`Configuration validation failed: ${errors.join(', ')}`)
    }
    
    return this
  }

  /**
   * Get value by key path
   * Returns undefined if not found (does not throw)
   */
  get<T = unknown>(key: string): T | undefined {
    return getNestedValue(key, this.data) as T | undefined
  }

  /**
   * Get value or throw if not found
   * Use for required configuration values
   */
  getOrThrow<T = unknown>(key: string): T {
    const value = this.get<T>(key)
    if (value === undefined) {
      throw new Error(`Required configuration key not found: ${key}`)
    }
    return value
  }

  /**
   * Get value with default
   * Contract: returns default if key not found
   * Contract: returns actual value if key exists
   */
  getWithDefault<T>(key: string, defaultValue: T): T {
    const value = this.get<T>(key)
    return value !== undefined ? value : defaultValue
  }

  /**
   * Check if key exists
   * Contract: supports nested key paths
   */
  has(key: string): boolean {
    return this.get(key) !== undefined
  }

  /**
   * Build final configuration data
   * Returns immutable configuration object
   */
  build(): ConfigData {
    return this.data
  }

  /**
   * Convert to Result for error handling integration
   * Escape hatch when Result<T> is needed
   */
  toResult(): Result<ConfigData> {
    return success(this.data)
  }
}

// ============================================================================
// Functional API (for Result<T> integration)
// ============================================================================

/**
 * Create configuration from object (Result-based)
 */
export const fromObject = (obj: object): Result<ConfigData> => {
  return fromTryCatch(
    () => ConfigBuilder.fromObject(obj).build(),
    error => createError(
      'CONFIG_PARSE_ERROR',
      error instanceof Error ? error.message : String(error),
      'CONFIGURATION'
    )
  )
}

/**
 * Create configuration from file (Result-based)
 */
export const fromFile = (filePath: string): Result<ConfigData> => {
  return fromTryCatch(
    () => ConfigBuilder.fromFile(filePath).build(),
    error => createError(
      'CONFIG_LOAD_ERROR',
      error instanceof Error ? error.message : String(error),
      'CONFIGURATION',
      { filePath }
    )
  )
}

/**
 * Create configuration from environment (Result-based)
 */
export const fromEnvironment = (prefix: string = ''): Result<ConfigData> => {
  return fromTryCatch(
    () => ConfigBuilder.fromEnvironment(prefix).build(),
    error => createError(
      'CONFIG_ENV_ERROR',
      error instanceof Error ? error.message : String(error),
      'CONFIGURATION',
      { prefix }
    )
  )
}

/**
 * Get configuration value (Result-based)
 */
export const get = <T = unknown>(
  key: string,
  config: ConfigData
): Result<T> => {
  const value = getNestedValue(key, config)
  
  if (value === undefined) {
    return failure(
      createError(
        'KEY_NOT_FOUND',
        `Configuration key not found: ${key}`,
        'CONFIGURATION',
        { key }
      )
    )
  }
  
  return success(value as T)
}

/**
 * Get with default (pure function)
 */
export const getWithDefault = <T>(
  key: string,
  defaultValue: T,
  config: ConfigData
): T => {
  const value = getNestedValue(key, config)
  return value !== undefined ? (value as T) : defaultValue
}

/**
 * Check if key exists (pure function)
 */
export const has = (key: string, config: ConfigData): boolean => {
  return getNestedValue(key, config) !== undefined
}

/**
 * Merge configurations (Result-based)
 */
export const merge = (configs: ConfigData[]): Result<ConfigData> => {
  return fromTryCatch(
    () => {
      const merged = configs.reduce((acc, config) => deepMerge(acc, config), {})
      return deepFreeze(merged)
    },
    error => createError(
      'CONFIG_MERGE_ERROR',
      error instanceof Error ? error.message : String(error),
      'CONFIGURATION'
    )
  )
}

/**
 * Create empty configuration
 */
export const empty = (): ConfigData => ({})

/**
 * Validate configuration (Result-based)
 */
export const validate = (
  schema: Schema,
  config: ConfigData
): Result<ConfigData> => {
  const result = schema.validate(config)
  
  if (!result.valid) {
    return failure(
      createError(
        'CONFIG_VALIDATION_ERROR',
        `Validation failed: ${(result.errors || []).join(', ')}`,
        'CONFIGURATION',
        { errors: result.errors }
      )
    )
  }
  
  return success(config)
}

// ============================================================================
// Internal Functions
// ============================================================================

/**
 * Parse configuration file based on extension
 */
const parseConfigFile = (filePath: string, content: string): object => {
  const ext = filePath.toLowerCase().split('.').pop()
  
  switch (ext) {
    case 'json':
      return JSON.parse(content)
    
    case 'js':
    case 'mjs':
    case 'cjs':
      // For JS files, would need dynamic import
      throw new Error('JavaScript config files not supported in this implementation')
    
    default:
      // Try JSON by default
      try {
        return JSON.parse(content)
      } catch {
        throw new Error(`Unsupported config file format: ${ext}`)
      }
  }
}

/**
 * Convert environment variable key to config key
 * APP_DATABASE_HOST -> database.host
 */
const envKeyToConfigKey = (envKey: string, prefix: string): string => {
  let key = envKey
  
  if (prefix && key.toUpperCase().startsWith(prefix)) {
    key = key.slice(prefix.length)
  }
  
  // Remove leading underscore if present
  if (key.startsWith('_')) {
    key = key.slice(1)
  }
  
  // Convert to lowercase and replace underscores with dots
  return key.toLowerCase().replace(/_/g, '.')
}

/**
 * Parse environment variable value
 * Attempts basic type coercion
 */
const parseEnvValue = (value: string): unknown => {
  // Boolean
  if (value.toLowerCase() === 'true') return true
  if (value.toLowerCase() === 'false') return false
  
  // Number
  if (/^\d+$/.test(value)) return parseInt(value, 10)
  if (/^\d*\.\d+$/.test(value)) return parseFloat(value)
  
  // JSON (array or object)
  if (value.startsWith('[') || value.startsWith('{')) {
    try {
      return JSON.parse(value)
    } catch {
      // Not valid JSON, return as string
    }
  }
  
  // String
  return value
}

/**
 * Get nested value from object using dot notation
 */
const getNestedValue = (path: string, obj: ConfigData): unknown => {
  const keys = path.split('.')
  let current: unknown = obj
  
  for (const key of keys) {
    if (current === null || typeof current !== 'object') {
      return undefined
    }
    current = (current as Record<string, unknown>)[key]
  }
  
  return current
}

/**
 * Set nested value in object using dot notation
 */
const setNestedValue = (
  obj: ConfigData,
  path: string,
  value: unknown
): void => {
  const keys = path.split('.')
  let current = obj as Record<string, unknown>
  
  for (let i = 0; i < keys.length - 1; i++) {
    const key = keys[i]
    if (!(key in current) || typeof current[key] !== 'object') {
      current[key] = {}
    }
    current = current[key] as Record<string, unknown>
  }
  
  current[keys[keys.length - 1]] = value
}

/**
 * Deep merge two configuration objects
 * Right-biased: source overrides target
 */
const deepMerge = (target: ConfigData, source: ConfigData): ConfigData => {
  const result = { ...target }
  
  for (const key in source) {
    const sourceValue = source[key]
    const targetValue = result[key]
    
    if (
      sourceValue !== null &&
      typeof sourceValue === 'object' &&
      !Array.isArray(sourceValue) &&
      targetValue !== null &&
      typeof targetValue === 'object' &&
      !Array.isArray(targetValue)
    ) {
      // Both are objects, merge recursively
      result[key] = deepMerge(
        targetValue as ConfigData,
        sourceValue as ConfigData
      )
    } else {
      // Override with source value
      result[key] = sourceValue
    }
  }
  
  return result
}

/**
 * Deep freeze object for immutability
 */
const deepFreeze = <T extends object>(obj: T): T => {
  Object.freeze(obj)
  
  Object.values(obj).forEach(value => {
    if (value !== null && typeof value === 'object') {
      deepFreeze(value)
    }
  })
  
  return obj
}

// ============================================================================
// Re-exports
// ============================================================================

export type { ConfigData, Schema }
```

This implementation provides:

1. **Fluent Builder Pattern** - Natural chaining for procedural config loading
2. **Fail-Fast Behavior** - Throws on errors during configuration
3. **Multiple Sources** - File, environment, and object support
4. **Immutable Operations** - All operations return new instances
5. **Nested Key Support** - Dot notation for deep access
6. **Environment Parsing** - Automatic type coercion for env vars
7. **Monoid Operations** - Associative merge with identity
8. **Dual API** - Both builder (throws) and functional (Result<T>)

Usage examples:

```typescript
// Fluent builder pattern (throws on error)
const config = ConfigBuilder
  .fromFile('./config.json')
  .merge(
    ConfigBuilder.fromEnvironment('APP_'),
    ConfigBuilder.fromObject({ defaults: { port: 3000 } })
  )
  .validate(mySchema)
  .build()

// Access configuration
const dbHost = ConfigBuilder.fromObject(config).getOrThrow<string>('database.host')
const port = ConfigBuilder.fromObject(config).getWithDefault('server.port', 3000)

// Result-based API for error handling
const configResult = fromFile('./config.json')
const dbHostResult = flatMap(
  config => get<string>('database.host', config),
  configResult
)
```