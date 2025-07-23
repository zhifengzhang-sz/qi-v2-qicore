/**
 * QiCore Foundation - Configuration Module
 *
 * TypeScript-native configuration management with Zod schema validation and fluent API.
 * Supports multi-source loading (JSON, YAML, TOML, environment variables) with
 * runtime type safety and TypeScript type inference.
 * Max-Min principle: 70% packages (zod, yaml, smol-toml, dotenv), 30% custom logic.
 */

import { readFile } from 'node:fs/promises'
import { readFileSync } from 'node:fs'
import {
  type Result,
  type QiError,
  success,
  failure,
  create as createError,
} from '../base/index.js'
import { config as loadEnv } from 'dotenv'
import { parse as parseToml } from 'smol-toml'
import { parse as parseYaml } from 'yaml'
import { type ZodSchema, type ZodType, z } from 'zod'
import { convertJsonSchemaToZod } from 'zod-from-json-schema'

// ============================================================================
// Core Types
// ============================================================================

/**
 * Configuration data as plain object
 */
export type ConfigData = Record<string, unknown>

/**
 * Configuration source types
 */
export type ConfigSource = 'object' | 'json' | 'yaml' | 'toml' | 'env'

/**
 * Configuration loading options
 */
export interface ConfigOptions {
  /** Source type for parsing */
  source: ConfigSource
  /** Environment variable prefix for env source */
  envPrefix?: string
  /** Whether to validate against schema */
  validate?: boolean
  /** Schema for validation */
  schema?: ZodSchema<unknown>
}

/**
 * Configuration builder state
 */
interface ConfigBuilderState {
  readonly data: ConfigData
  readonly sources: ConfigSource[]
  readonly schema?: ZodSchema<unknown>
  readonly validated: boolean
}

// ============================================================================
// Error Types
// ============================================================================

/**
 * Configuration-specific error types
 */
export type ConfigError = QiError & {
  readonly category: 'CONFIGURATION'
  readonly context: {
    readonly source?: ConfigSource
    readonly path?: string
    readonly schema?: string
  }
}

/**
 * Create configuration error
 */
export const configError = (message: string, context: ConfigError['context'] = {}): ConfigError =>
  createError('CONFIG_ERROR', message, 'CONFIGURATION', context) as ConfigError

// ============================================================================
// Configuration Builder (Fluent API) - 30% custom logic
// ============================================================================

/**
 * Fluent configuration builder with method chaining
 *
 * Provides TypeScript-native ergonomics while maintaining Result<T> patterns
 */
export class ConfigBuilder {
  private constructor(private readonly state: ConfigBuilderState) {}

  /**
   * Create new configuration builder from object
   */
  static fromObject(data: ConfigData): ConfigBuilder {
    return new ConfigBuilder({
      data: { ...data },
      sources: ['object'],
      validated: false,
    })
  }

  /**
   * Create new configuration builder from JSON file
   */
  static fromJsonFile(path: string): Promise<Result<ConfigBuilder, ConfigError>> {
    return ConfigBuilder.fromFile(path, { source: 'json' })
  }

  /**
   * Create new configuration builder from YAML file (using yaml package)
   */
  static fromYamlFile(path: string): Promise<Result<ConfigBuilder, ConfigError>> {
    return ConfigBuilder.fromFile(path, { source: 'yaml' })
  }

  /**
   * Create new configuration builder from TOML file (using smol-toml package)
   */
  static fromTomlFile(path: string): Promise<Result<ConfigBuilder, ConfigError>> {
    return ConfigBuilder.fromFile(path, { source: 'toml' })
  }

  /**
   * Create new configuration builder from environment variables (using dotenv package)
   */
  static fromEnv(prefix = ''): ConfigBuilder {
    // Use dotenv package to load .env file
    loadEnv()
    const data: ConfigData = {}

    const prefixFilter = prefix ? `${prefix}_` : ''

    for (const [key, value] of Object.entries(process.env)) {
      if (!prefixFilter || key.startsWith(prefixFilter)) {
        const configKey = prefixFilter ? key.slice(prefixFilter.length) : key
        data[configKey.toLowerCase()] = value
      }
    }

    return new ConfigBuilder({
      data,
      sources: ['env'],
      validated: false,
    })
  }

  /**
   * Generic file loader with format detection (70% packages)
   */
  private static async fromFile(
    path: string,
    options: ConfigOptions
  ): Promise<Result<ConfigBuilder, ConfigError>> {
    try {
      const content = await readFile(path, 'utf-8')

      let data: ConfigData

      switch (options.source) {
        case 'json':
          data = JSON.parse(content)
          break
        case 'yaml':
          // Use yaml package for parsing
          data = parseYaml(content) as ConfigData
          break
        case 'toml':
          // Use smol-toml package for parsing
          data = parseToml(content) as ConfigData
          break
        default:
          return failure(
            configError(`Unsupported source: ${options.source}`, {
              source: options.source,
              path,
            })
          )
      }

      const builder = new ConfigBuilder({
        data,
        sources: [options.source],
        validated: false,
      })

      // Apply schema validation if provided (using zod)
      if (options.validate && options.schema) {
        const validatedResult = builder.validateWith(options.schema).build()
        if (validatedResult.tag === 'success') {
          return success(builder.validateWith(options.schema)) // Return builder for consistency
        }
        return validatedResult as Result<ConfigBuilder, ConfigError>
      }

      return success(builder)
    } catch (error) {
      return failure(
        configError(`Failed to load config from ${path}: ${error}`, {
          source: options.source,
          path,
        })
      )
    }
  }

  /**
   * Merge with another configuration (fluent)
   * Later configurations override earlier ones
   */
  merge(other: ConfigBuilder): ConfigBuilder {
    return new ConfigBuilder({
      data: this.deepMerge(this.state.data, other.state.data),
      sources: [...this.state.sources, ...other.state.sources],
      schema: this.state.schema || other.state.schema,
      validated: false, // Need to revalidate after merge
    })
  }

  /**
   * Merge with plain object (fluent)
   */
  mergeObject(data: ConfigData): ConfigBuilder {
    return this.merge(ConfigBuilder.fromObject(data))
  }

  /**
   * Set nested value using dot notation (fluent)
   */
  set(path: string, value: unknown): ConfigBuilder {
    const keys = path.split('.')
    const newData = { ...this.state.data }

    let current = newData
    for (let i = 0; i < keys.length - 1; i++) {
      const key = keys[i]
      if (!key) continue
      if (typeof current[key] !== 'object' || current[key] === null) {
        current[key] = {}
      }
      current = current[key] as Record<string, unknown>
    }

    const lastKey = keys[keys.length - 1]
    if (!lastKey) return this
    current[lastKey] = value

    return new ConfigBuilder({
      ...this.state,
      data: newData,
      validated: false,
    })
  }

  /**
   * Apply schema validation (fluent) - using zod package
   */
  validateWith<T>(schema: ZodSchema<T>): ConfigBuilder {
    return new ConfigBuilder({
      ...this.state,
      schema,
      validated: false,
    })
  }

  /**
   * Transform configuration data (fluent)
   */
  transform<T>(fn: (data: ConfigData) => T): ConfigBuilder {
    return new ConfigBuilder({
      ...this.state,
      data: fn(this.state.data) as ConfigData,
      validated: false,
    })
  }

  /**
   * Filter configuration keys (fluent)
   */
  filter(predicate: (key: string, value: unknown) => boolean): ConfigBuilder {
    const filtered = Object.fromEntries(
      Object.entries(this.state.data).filter(([key, value]) => predicate(key, value))
    )

    return new ConfigBuilder({
      ...this.state,
      data: filtered,
      validated: false,
    })
  }

  /**
   * Validate configuration using external JSON schema file (using zod-from-json-schema package)
   */
  validateWithSchemaFile(schemaPath: string): ConfigBuilder {
    try {
      const schemaContent = readFileSync(schemaPath, 'utf-8')
      const jsonSchema = JSON.parse(schemaContent)
      // Use zod-from-json-schema package to convert JSON schema to Zod
      const zodSchema = convertJsonSchemaToZod(jsonSchema) as unknown as ZodType

      return this.validateWith(zodSchema)
    } catch (error) {
      throw new Error(
        `Failed to load schema from ${schemaPath}: ${error instanceof Error ? error.message : 'Unknown error'}`
      )
    }
  }

  /**
   * Build final configuration with validation (using zod)
   */
  build(): Result<Config, ConfigError> {
    // Apply schema validation if configured (using zod package)
    if (this.state.schema) {
      const validation = this.state.schema.safeParse(this.state.data)

      if (!validation.success) {
        return failure(
          configError('Schema validation failed', {
            schema: this.state.schema.constructor.name,
          })
        )
      }

      return success(
        new Config({
          ...this.state,
          data: validation.data as ConfigData,
          validated: true,
        })
      )
    }

    return success(
      new Config({
        ...this.state,
        validated: false, // Only set to true if schema validation was successful
      })
    )
  }

  /**
   * Build ValidatedConfig with validation (throws on invalid config)
   */
  buildValidated(): Result<ValidatedConfig, ConfigError> {
    const buildResult = this.build()
    if (buildResult.tag === 'failure') {
      return buildResult
    }

    const config = buildResult.value
    if (!config.isValidated()) {
      return failure(
        configError('Configuration must be validated before creating ValidatedConfig', {})
      )
    }

    return success(new ValidatedConfig(config))
  }

  /**
   * Build with type assertion (unsafe)
   */
  buildUnsafe(): Config {
    return new Config({
      ...this.state,
      validated: false,
    })
  }

  /**
   * Get current data (for inspection)
   */
  getData(): ConfigData {
    return { ...this.state.data }
  }

  /**
   * Get sources (for inspection)
   */
  getSources(): ConfigSource[] {
    return [...this.state.sources]
  }

  /**
   * Deep merge two configuration objects
   */
  private deepMerge(target: ConfigData, source: ConfigData): ConfigData {
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
        result[key] = this.deepMerge(targetValue as ConfigData, sourceValue as ConfigData)
      } else {
        // Override with source value (primitives, arrays, or null)
        result[key] = sourceValue
      }
    }

    return result
  }
}

// ============================================================================
// Configuration Class (Immutable) - 30% custom logic
// ============================================================================

/**
 * Immutable configuration with type-safe access
 */
export class Config {
  constructor(private readonly state: ConfigBuilderState) {}

  /**
   * Get configuration value by path
   */
  get<T = unknown>(path: string): Result<T, ConfigError> {
    const keys = path.split('.')
    let current: unknown = this.state.data

    for (const key of keys) {
      if (typeof current !== 'object' || current === null || !(key in current)) {
        return failure(configError(`Configuration path not found: ${path}`, { path }))
      }
      current = (current as Record<string, unknown>)[key]
    }

    return success(current as T)
  }

  /**
   * Get configuration value with default
   */
  getOr<T>(path: string, defaultValue: T): T {
    const result = this.get<T>(path)
    return result.tag === 'success' ? result.value : defaultValue
  }

  /**
   * Check if path exists
   */
  has(path: string): boolean {
    return this.get(path).tag === 'success'
  }

  /**
   * Get all configuration data
   */
  getAll(): ConfigData {
    return { ...this.state.data }
  }

  /**
   * Get configuration sources
   */
  getSources(): ConfigSource[] {
    return [...this.state.sources]
  }

  /**
   * Check if configuration was validated
   */
  isValidated(): boolean {
    return this.state.validated
  }

  /**
   * Get schema if present
   */
  getSchema(): ZodSchema<unknown> | undefined {
    return this.state.schema
  }

  /**
   * Convert to builder for further modification
   */
  toBuilder(): ConfigBuilder {
    return ConfigBuilder.fromObject(this.state.data)
  }

  /**
   * Merge with another configuration (immutable)
   */
  merge(other: Config): Config {
    return this.toBuilder().merge(other.toBuilder()).buildUnsafe()
  }

  /**
   * Serialize to JSON
   */
  toJson(): string {
    return JSON.stringify(this.state.data, null, 2)
  }

  /**
   * Get as plain object
   */
  toObject(): ConfigData {
    return this.getAll()
  }
}

// ============================================================================
// ValidatedConfig Class - Advanced v-0.3.5 pattern
// ============================================================================

/**
 * Configuration error for invalid access to ValidatedConfig
 */
export class ConfigAccessError extends Error {
  constructor(
    message: string,
    public readonly path: string
  ) {
    super(message)
    this.name = 'ConfigAccessError'
  }
}

/**
 * Validated configuration with direct value access
 *
 * After successful schema validation, provides direct access to configuration values
 * without Result wrapper types. Throws ConfigAccessError for non-existent paths.
 */
export class ValidatedConfig {
  constructor(private readonly config: Config) {
    if (!config.isValidated()) {
      throw new Error('ValidatedConfig can only be created from validated Config instances')
    }
  }

  /**
   * Get configuration value by path (throws if path doesn't exist)
   *
   * Use this for required configuration values that must exist.
   * Throws ConfigAccessError if the path is not found.
   */
  get<T = unknown>(path: string): T {
    const result = this.config.get<T>(path)
    if (result.tag === 'failure') {
      throw new ConfigAccessError(`Configuration path not found: ${path}`, path)
    }
    return result.value
  }

  /**
   * Get configuration value by path (returns undefined if path doesn't exist)
   *
   * Use this for optional configuration values.
   */
  getOptional<T = unknown>(path: string): T | undefined {
    const result = this.config.get<T>(path)
    return result.tag === 'success' ? result.value : undefined
  }

  /**
   * Get configuration value with default fallback
   *
   * Use this when you want a safe fallback value.
   */
  getOr<T>(path: string, defaultValue: T): T {
    return this.config.getOr(path, defaultValue)
  }

  /**
   * Check if path exists in configuration
   */
  has(path: string): boolean {
    return this.config.has(path)
  }

  /**
   * Get all configuration data as plain object
   */
  getAll(): ConfigData {
    return this.config.getAll()
  }

  /**
   * Get configuration sources used to build this config
   */
  getSources(): ConfigSource[] {
    return this.config.getSources()
  }

  /**
   * Get the schema used for validation
   */
  getSchema(): ZodSchema<unknown> | undefined {
    return this.config.getSchema()
  }

  /**
   * Convert back to regular Config for Result-based access
   */
  toConfig(): Config {
    return this.config
  }

  /**
   * Merge with another ValidatedConfig (creates new ValidatedConfig)
   */
  merge(other: ValidatedConfig): ValidatedConfig {
    const mergedConfig = this.config.merge(other.config)
    return new ValidatedConfig(mergedConfig)
  }

  /**
   * Serialize to JSON string
   */
  toJson(): string {
    return this.config.toJson()
  }

  /**
   * Get as plain object
   */
  toObject(): ConfigData {
    return this.config.toObject()
  }
}

// ============================================================================
// Factory Functions - 30% custom logic
// ============================================================================

/**
 * Create configuration from object
 */
export const fromObject = (data: ConfigData): Config => ConfigBuilder.fromObject(data).buildUnsafe()

/**
 * Create configuration from JSON file
 */
export const fromJsonFile = (path: string): Promise<Result<Config, ConfigError>> =>
  ConfigBuilder.fromJsonFile(path).then((result) =>
    result.tag === 'success' ? result.value.build() : result
  )

/**
 * Create configuration from YAML file (using yaml package)
 */
export const fromYamlFile = (path: string): Promise<Result<Config, ConfigError>> =>
  ConfigBuilder.fromYamlFile(path).then((result) =>
    result.tag === 'success' ? result.value.build() : result
  )

/**
 * Create configuration from TOML file (using smol-toml package)
 */
export const fromTomlFile = (path: string): Promise<Result<Config, ConfigError>> =>
  ConfigBuilder.fromTomlFile(path).then((result) =>
    result.tag === 'success' ? result.value.build() : result
  )

/**
 * Create configuration from environment variables (using dotenv package)
 */
export const fromEnv = (prefix = ''): Config => ConfigBuilder.fromEnv(prefix).buildUnsafe()

/**
 * Create empty configuration
 */
export const empty = (): Config => ConfigBuilder.fromObject({}).buildUnsafe()

// ============================================================================
// Schema Validation Utilities (70% zod package)
// ============================================================================

/**
 * Validate configuration against Zod schema
 */
export const validateConfig = <T>(config: Config, schema: ZodSchema<T>): Result<T, ConfigError> => {
  const validation = schema.safeParse(config.getAll())

  if (!validation.success) {
    return failure(
      configError('Configuration validation failed', {
        schema: schema.constructor.name,
        context: {
          errors: validation.error.issues.map((err) => ({
            path: err.path.join('.'),
            message: err.message,
            code: err.code,
          })),
        },
      })
    )
  }

  return success(validation.data)
}

/**
 * Safe parse with detailed error information (using zod)
 */
export const safeParseConfig = <T>(
  data: ConfigData,
  schema: ZodSchema<T>
): Result<T, ConfigError> => {
  const validation = schema.safeParse(data)

  if (!validation.success) {
    return failure(
      configError('Schema parsing failed', {
        schema: schema.constructor.name,
        context: {
          errors: validation.error.issues.map((err) => ({
            path: err.path.join('.'),
            message: err.message,
            code: err.code,
          })),
        },
      })
    )
  }

  return success(validation.data)
}

// ============================================================================
// Common Schema Patterns (70% zod package)
// ============================================================================

/**
 * Common application configuration schema (using zod coercion and defaults)
 */
export const AppConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    version: z.string(),
    environment: z.enum(['development', 'production', 'test']).default('development'),
    port: z.coerce.number().default(3000),
    host: z.string().default('localhost'),
  }),
  database: z
    .object({
      host: z.string(),
      port: z.coerce.number().default(5432),
      name: z.string(),
      user: z.string(),
      password: z.string(),
    })
    .optional(),
  redis: z
    .object({
      host: z.string().default('localhost'),
      port: z.coerce.number().default(6379),
      password: z.string().optional(),
    })
    .optional(),
  logging: z
    .object({
      level: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
      pretty: z.coerce.boolean().default(false),
    })
    .optional(),
})

export type AppConfig = z.infer<typeof AppConfigSchema>
