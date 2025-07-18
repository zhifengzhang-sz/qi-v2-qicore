# Configuration API Reference

The Configuration module provides robust configuration management with validation, multiple sources, and type safety using Zod schemas and fluent API patterns.

## Type Definitions

```typescript
type ConfigData = Record<string, unknown>

type ConfigSource = 'object' | 'json' | 'yaml' | 'toml' | 'env'

interface ConfigOptions {
  source: ConfigSource
  envPrefix?: string
  validate?: boolean
  schema?: ZodSchema<unknown>
}

interface ConfigError extends QiError {
  readonly category: 'CONFIGURATION'
  readonly context: {
    readonly source?: ConfigSource
    readonly path?: string
    readonly schema?: string
  }
}
```

## Core Classes

### ConfigBuilder

Fluent API for building configuration from multiple sources with static factory methods.

```typescript
class ConfigBuilder {
  // Static factory methods
  static fromObject(data: ConfigData): ConfigBuilder
  static fromJsonFile(path: string): Promise<Result<ConfigBuilder, ConfigError>>
  static fromYamlFile(path: string): Promise<Result<ConfigBuilder, ConfigError>>
  static fromTomlFile(path: string): Promise<Result<ConfigBuilder, ConfigError>>
  static fromEnv(prefix?: string): ConfigBuilder
  
  // Instance methods for building
  merge(other: ConfigBuilder): ConfigBuilder
  mergeObject(data: ConfigData): ConfigBuilder
  set(path: string, value: unknown): ConfigBuilder
  validateWith<T>(schema: ZodSchema<T>): ConfigBuilder
  transform<T>(fn: (data: ConfigData) => T): ConfigBuilder
  filter(predicate: (key: string, value: unknown) => boolean): ConfigBuilder
  validateWithSchemaFile(schemaPath: string): ConfigBuilder
  
  // Build methods
  build(): Result<Config, ConfigError>
  buildValidated(): Result<ValidatedConfig, ConfigError>
  buildUnsafe(): Config
  
  // Inspection methods
  getData(): ConfigData
  getSources(): ConfigSource[]
}
```

### Config

Immutable configuration container with type-safe access.

```typescript
class Config {
  get<T>(path: string): Result<T, ConfigError>
  getOr<T>(path: string, defaultValue: T): T
  has(path: string): boolean
  getAll(): ConfigData
  isValidated(): boolean
  toObject(): ConfigData
  toJson(): string
  merge(other: Config): Config
  
  // Additional methods
  getSources(): ConfigSource[]
  getSchema(): ZodSchema<unknown> | undefined
  toBuilder(): ConfigBuilder
}
```

### ValidatedConfig

Type-safe configuration container that wraps Config with validated data access.

```typescript
class ValidatedConfig {
  // Access methods with type safety
  get<T>(path: string): T
  getOptional<T>(path: string): T | undefined
  getOr<T>(path: string, defaultValue: T): T
  has(path: string): boolean
  
  // Data access
  getAll(): ConfigData
  getSources(): ConfigSource[]
  getSchema(): ZodSchema<unknown> | undefined
  
  // Conversion methods
  toConfig(): Config
  merge(other: ValidatedConfig): ValidatedConfig
  toJson(): string
  toObject(): ConfigData
}
```

### ConfigAccessError

Specialized error for configuration access failures.

```typescript
class ConfigAccessError extends Error {
  readonly path: string
}
```

## Factory Functions

### fromObject(data: ConfigData): Config

Creates configuration directly from an object.

```typescript
const config = fromObject({
  app: {
    name: 'MyApp',
    port: 3000
  },
  database: {
    host: 'localhost',
    port: 5432
  }
})
```

### fromJsonFile(path: string): Promise<Result<Config, ConfigError>>

Creates configuration from a JSON file.

```typescript
const configResult = await fromJsonFile('./config.json')
if (configResult.tag === 'success') {
  const config = configResult.value
}
```

### fromYamlFile(path: string): Promise<Result<Config, ConfigError>>

Creates configuration from a YAML file.

```typescript
const configResult = await fromYamlFile('./config.yaml')
```

### fromTomlFile(path: string): Promise<Result<Config, ConfigError>>

Creates configuration from a TOML file.

```typescript
const configResult = await fromTomlFile('./config.toml')
```

### fromEnv(prefix?: string): Config

Creates configuration from environment variables.

```typescript
const config = fromEnv('APP') // Uses APP_ prefixed env vars
```

### empty(): Config

Creates an empty configuration.

```typescript
const config = empty()
```

### ConfigBuilder.getData(): ConfigData

Returns the current configuration data for inspection.

```typescript
const builder = ConfigBuilder.fromObject({ app: { name: 'test' } })
const currentData = builder.getData() // { app: { name: 'test' } }
```

### ConfigBuilder.getSources(): ConfigSource[]

Returns the list of configuration sources used to build this configuration.

```typescript
const builder = ConfigBuilder
  .fromObject({ default: true })
  .merge(ConfigBuilder.fromEnv('APP'))
const sources = builder.getSources() // ['object', 'env']
```

### Config.getSources(): ConfigSource[]

Returns the configuration sources used to build this configuration.

```typescript
const config = fromObject({ app: { name: 'test' } })
const sources = config.getSources() // ['object']
```

### Config.getSchema(): ZodSchema<unknown> | undefined

Returns the validation schema if present.

```typescript
const config = ConfigBuilder
  .fromObject({ app: { name: 'test' } })
  .validateWith(AppConfigSchema)
  .buildUnsafe()
const schema = config.getSchema() // Returns the AppConfigSchema
```

### Config.toBuilder(): ConfigBuilder

Converts the Config back to a ConfigBuilder for further modification.

```typescript
const config = fromObject({ app: { name: 'test' } })
const builder = config.toBuilder()
const modified = builder
  .set('app.version', '1.0.0')
  .build()
```

## Utility Functions

### configError(message: string, context?: ConfigError['context']): ConfigError

Creates a configuration-specific error.

```typescript
const error = configError('Invalid configuration', {
  source: 'json',
  path: './config.json'
})
```

### validateConfig<T>(config: Config, schema: ZodSchema<T>): Result<T, ConfigError>

Validates a configuration against a Zod schema.

```typescript
const UserConfigSchema = z.object({
  name: z.string(),
  email: z.string().email()
})

const result = validateConfig(config, UserConfigSchema)
if (result.tag === 'success') {
  const userData = result.value // Direct type-safe access
}
```

### safeParseConfig<T>(data: ConfigData, schema: ZodSchema<T>): Result<T, ConfigError>

Safely parses configuration data against a schema.

```typescript
const result = safeParseConfig(configData, UserConfigSchema)
```

## Predefined Schemas

### AppConfigSchema

Pre-defined schema for common application configuration.

```typescript
const AppConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    version: z.string(),
    environment: z.enum(['development', 'production', 'test']).default('development'),
    port: z.coerce.number().default(3000),
    host: z.string().default('localhost'),
  }),
  database: z.object({
    host: z.string(),
    port: z.coerce.number().default(5432),
    name: z.string(),
    user: z.string(),
    password: z.string(),
  }).optional(),
  redis: z.object({
    host: z.string().default('localhost'),
    port: z.coerce.number().default(6379),
    password: z.string().optional(),
  }).optional(),
  logging: z.object({
    level: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
    pretty: z.coerce.boolean().default(false),
  }).optional(),
})
```

## Usage Examples

### Basic Configuration Loading

```typescript
import { ConfigBuilder, fromObject, validateConfig } from '@qi/qicore-foundation/core'
import { z } from 'zod'

// Static method approach
const config = ConfigBuilder
  .fromObject({ port: 3000, host: 'localhost' })
  .merge(ConfigBuilder.fromEnv('APP'))
  .build()

if (config.tag === 'success') {
  const portResult = config.value.get<number>('port')
  if (portResult.tag === 'success') {
    console.log('Port:', portResult.value)
  }
}
```

### Multi-Source Configuration

```typescript
// Load from multiple sources
const builder = ConfigBuilder
  .fromObject({ 
    defaults: { timeout: 5000 } 
  })
  .mergeObject({
    app: { name: 'MyApp' }
  })

// Add environment variables
const envBuilder = ConfigBuilder.fromEnv('APP')
const combined = builder.merge(envBuilder)

const config = combined.build()
```

### Schema Validation

```typescript
const ConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    port: z.number().min(1000).max(65535)
  }),
  database: z.object({
    host: z.string(),
    port: z.number()
  })
})

// Build with validation
const configResult = ConfigBuilder
  .fromJsonFile('./config.json')
  .then(result => {
    if (result.tag === 'success') {
      return result.value
        .validateWith(ConfigSchema)
        .buildValidated()
    }
    return result
  })
```

### Configuration Merging and Transformation

```typescript
// Advanced builder operations
const config = ConfigBuilder
  .fromObject({ base: { value: 1 } })
  .set('app.name', 'MyApp')
  .filter((key, value) => !key.startsWith('temp'))
  .transform(data => ({
    ...data,
    computed: { timestamp: Date.now() }
  }))
  .build()
```

### File-Based Configuration

```typescript
// JSON configuration
const jsonConfig = await ConfigBuilder.fromJsonFile('./config.json')

// YAML configuration  
const yamlConfig = await ConfigBuilder.fromYamlFile('./config.yaml')

// TOML configuration
const tomlConfig = await ConfigBuilder.fromTomlFile('./config.toml')

// Combine different formats
if (jsonConfig.tag === 'success' && yamlConfig.tag === 'success') {
  const combined = jsonConfig.value
    .merge(yamlConfig.value)
    .build()
}
```

### Type-Safe Configuration Access

```typescript
interface AppConfig {
  server: {
    port: number
    host: string
  }
  database: {
    url: string
  }
}

const AppSchema: z.ZodSchema<AppConfig> = z.object({
  server: z.object({
    port: z.number(),
    host: z.string()
  }),
  database: z.object({
    url: z.string()
  })
})

const configResult = await ConfigBuilder.fromJsonFile('./app.json')
if (configResult.tag === 'success') {
  const validatedConfigResult = configResult.value
    .validateWith(AppSchema)
    .buildValidated()
  
  if (validatedConfigResult.tag === 'success') {
    const validatedConfig = validatedConfigResult.value
    const serverPort = validatedConfig.get<number>('server.port')
    const serverHost = validatedConfig.get<string>('server.host')
    console.log(`Server: ${serverHost}:${serverPort}`)
  }
}
```

### Error Handling

```typescript
import { match } from '@qi/qicore-foundation/base'

const configResult = await ConfigBuilder.fromJsonFile('./config.json')

match(
  (builder) => {
    const config = builder.build()
    match(
      (cfg) => console.log('Config loaded successfully'),
      (error) => console.error('Config build failed:', error.message),
      config
    )
  },
  (error) => console.error('Config file load failed:', error.message),
  configResult
)
```

### Environment-Specific Configuration

```typescript
// Load base config and override with environment-specific settings
const baseConfig = ConfigBuilder.fromObject({
  app: { name: 'MyApp', debug: false }
})

const envConfig = ConfigBuilder.fromEnv('APP')

// Environment file overlay
const envFile = process.env.NODE_ENV || 'development'
const envFileConfig = await ConfigBuilder.fromJsonFile(`./config.${envFile}.json`)

if (envFileConfig.tag === 'success') {
  const config = baseConfig
    .merge(envFileConfig.value)
    .merge(envConfig)
    .build()
}
```

## Best Practices

1. **Use static factory methods**: ConfigBuilder methods are static
2. **Validate early**: Use schema validation for type safety
3. **Merge strategically**: Later configurations override earlier ones
4. **Handle errors properly**: Use Result<T> pattern for error handling
5. **Environment separation**: Use different config files per environment
6. **Type safety**: Use TypeScript interfaces with Zod schemas
7. **Default values**: Use getOr() for safe access with defaults
8. **Immutable operations**: All operations return new instances

## Performance Considerations

- Configuration loading is I/O bound for file operations
- In-memory merging and transformation are fast
- Schema validation adds minimal overhead
- Use buildUnsafe() only when certain about data validity
- Cache validated configurations to avoid re-validation

## Integration with Result<T>

All potentially failing operations return Result<T> for consistent error handling:

```typescript
import { flatMap, map } from '@qi/base'

const processConfig = async (path: string) => {
  const configResult = await ConfigBuilder.fromJsonFile(path)
  
  return flatMap(
    (builder) => {
      const config = builder.build()
      return map(
        (cfg) => cfg.get<string>('app.name'),
        config
      )
    },
    configResult
  )
}
```