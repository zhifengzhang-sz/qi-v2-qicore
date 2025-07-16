# Configuration API Reference

The Configuration module provides robust configuration management with validation, multiple sources, and type safety.

## Core Classes

### ConfigBuilder

Fluent API for building configuration from multiple sources.

```typescript
class ConfigBuilder {
  static create(): ConfigBuilder
  fromObject(data: Record<string, unknown>): ConfigBuilder
  fromJsonFile(path: string): ConfigBuilder
  fromYamlFile(path: string): ConfigBuilder
  fromTomlFile(path: string): ConfigBuilder
  fromEnv(prefix?: string): ConfigBuilder
  validate<T>(schema: z.ZodSchema<T>): ConfigBuilder
  build(): Result<Config, ConfigError>
}
```

### Config

Immutable configuration container with type-safe access.

```typescript
class Config {
  get<T>(key: string, defaultValue?: T): T
  has(key: string): boolean
  keys(): string[]
  toObject(): Record<string, unknown>
  merge(other: Config): Config
  subset(prefix: string): Config
}
```

## Type Definitions

```typescript
type ConfigData = Record<string, unknown>

interface ConfigSource {
  type: 'object' | 'file' | 'env'
  data: ConfigData
  metadata?: Record<string, unknown>
}

interface ConfigOptions {
  envPrefix?: string
  fileEncoding?: string
  strict?: boolean
}

interface ConfigError extends QiError {
  category: 'CONFIGURATION'
}
```

## Factory Functions

### fromObject(data: Record<string, unknown>): Result<Config, ConfigError>

Creates configuration from an object.

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

### fromJsonFile(path: string): Result<Config, ConfigError>

Creates configuration from a JSON file.

```typescript
const config = fromJsonFile('./config.json')
```

### fromYamlFile(path: string): Result<Config, ConfigError>

Creates configuration from a YAML file.

```typescript
const config = fromYamlFile('./config.yaml')
```

### fromTomlFile(path: string): Result<Config, ConfigError>

Creates configuration from a TOML file.

```typescript
const config = fromTomlFile('./config.toml')
```

### fromEnv(prefix?: string): Result<Config, ConfigError>

Creates configuration from environment variables.

```typescript
// Load all environment variables
const config = fromEnv()

// Load with prefix (e.g., APP_PORT becomes port)
const config = fromEnv('APP_')
```

### empty(): Config

Creates an empty configuration.

```typescript
const config = empty()
```

## Validation

### validateConfig<T>(config: Config, schema: z.ZodSchema<T>): Result<T, ConfigError>

Validates configuration against a Zod schema.

```typescript
import { z } from 'zod'

const AppConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    port: z.number().min(1).max(65535),
    debug: z.boolean().default(false)
  }),
  database: z.object({
    host: z.string(),
    port: z.number(),
    name: z.string()
  })
})

const validated = validateConfig(config, AppConfigSchema)
```

### safeParseConfig<T>(config: Config, schema: z.ZodSchema<T>): Result<T, ConfigError>

Safely parses configuration with detailed error messages.

```typescript
const result = safeParseConfig(config, AppConfigSchema)
if (result.tag === 'failure') {
  console.log('Validation errors:', result.error.context?.errors)
}
```

## Built-in Schemas

### AppConfigSchema

Common application configuration schema.

```typescript
const AppConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    port: z.number().default(3000),
    debug: z.boolean().default(false),
    environment: z.enum(['development', 'production', 'test']).default('development')
  })
})

type AppConfig = z.infer<typeof AppConfigSchema>
```

## Error Handling

### configError(message: string, context?: Record<string, unknown>): ConfigError

Creates a configuration error.

```typescript
const error = configError('Configuration validation failed', {
  schema: 'AppConfig',
  errors: validationErrors
})
```

## Usage Examples

### Basic Configuration

```typescript
import { ConfigBuilder } from '@qi/qicore-foundation'

const configResult = ConfigBuilder.create()
  .fromJsonFile('./config.json')
  .fromEnv('APP_')
  .build()

if (configResult.tag === 'success') {
  const config = configResult.value
  
  const port = config.get('app.port', 3000)
  const dbHost = config.get('database.host')
  
  console.log(`Starting app on port ${port}`)
}
```

### Configuration with Validation

```typescript
import { ConfigBuilder, validateConfig } from '@qi/qicore-foundation'
import { z } from 'zod'

const ConfigSchema = z.object({
  server: z.object({
    port: z.number().min(1).max(65535),
    host: z.string().default('localhost')
  }),
  database: z.object({
    url: z.string().url(),
    maxConnections: z.number().default(10)
  })
})

const configResult = ConfigBuilder.create()
  .fromYamlFile('./config.yaml')
  .fromEnv()
  .validate(ConfigSchema)
  .build()

if (configResult.tag === 'success') {
  const config = configResult.value
  // config is now typed according to ConfigSchema
}
```

### Multiple Sources with Precedence

```typescript
// Later sources override earlier ones
const config = ConfigBuilder.create()
  .fromJsonFile('./config.json')        // Base configuration
  .fromYamlFile('./config.local.yaml')  // Local overrides
  .fromEnv('APP_')                      // Environment overrides
  .build()
```

### Environment-Specific Configuration

```typescript
const environment = process.env.NODE_ENV || 'development'

const config = ConfigBuilder.create()
  .fromJsonFile('./config.json')
  .fromJsonFile(`./config.${environment}.json`)
  .fromEnv()
  .build()
```

### Configuration Subsets

```typescript
const config = configResult.value
const dbConfig = config.subset('database')

// Access nested values without prefix
const host = dbConfig.get('host')
const port = dbConfig.get('port')
```

### Merging Configurations

```typescript
const baseConfig = fromObject({ a: 1, b: 2 })
const overrideConfig = fromObject({ b: 3, c: 4 })

const merged = baseConfig.merge(overrideConfig)
// Result: { a: 1, b: 3, c: 4 }
```

## Advanced Patterns

### Service Configuration

```typescript
class DatabaseService {
  private config: DatabaseConfig

  constructor(config: Config) {
    const dbConfigResult = validateConfig(config.subset('database'), DatabaseSchema)
    
    if (dbConfigResult.tag === 'failure') {
      throw new Error('Invalid database configuration')
    }
    
    this.config = dbConfigResult.value
  }

  async connect(): Promise<Connection> {
    return createConnection({
      host: this.config.host,
      port: this.config.port,
      database: this.config.name
    })
  }
}
```

### Configuration Watching

```typescript
import { watch } from 'fs'

class ConfigWatcher {
  private config: Config
  private callbacks: Array<(config: Config) => void> = []

  constructor(private configPath: string) {
    this.loadConfig()
    this.watchFile()
  }

  private loadConfig() {
    const result = ConfigBuilder.create()
      .fromJsonFile(this.configPath)
      .fromEnv()
      .build()

    if (result.tag === 'success') {
      this.config = result.value
      this.callbacks.forEach(callback => callback(this.config))
    }
  }

  private watchFile() {
    watch(this.configPath, () => {
      this.loadConfig()
    })
  }

  onChange(callback: (config: Config) => void) {
    this.callbacks.push(callback)
  }
}
```

### Configuration Providers

```typescript
interface ConfigProvider {
  load(): Promise<Result<ConfigData, ConfigError>>
  watch?(callback: (data: ConfigData) => void): void
}

class RemoteConfigProvider implements ConfigProvider {
  constructor(private endpoint: string) {}

  async load(): Promise<Result<ConfigData, ConfigError>> {
    try {
      const response = await fetch(this.endpoint)
      const data = await response.json()
      return Ok(data)
    } catch (error) {
      return Err(configError('Failed to load remote config', { endpoint: this.endpoint }))
    }
  }
}

// Usage
const provider = new RemoteConfigProvider('https://config.example.com/app')
const remoteData = await provider.load()

if (remoteData.tag === 'success') {
  const config = ConfigBuilder.create()
    .fromObject(remoteData.value)
    .fromEnv()
    .build()
}
```

## File Format Examples

### JSON Configuration

```json
{
  "app": {
    "name": "MyApp",
    "port": 3000,
    "debug": false
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "myapp"
  },
  "redis": {
    "host": "localhost",
    "port": 6379
  }
}
```

### YAML Configuration

```yaml
app:
  name: MyApp
  port: 3000
  debug: false

database:
  host: localhost
  port: 5432
  name: myapp

redis:
  host: localhost
  port: 6379
```

### TOML Configuration

```toml
[app]
name = "MyApp"
port = 3000
debug = false

[database]
host = "localhost"
port = 5432
name = "myapp"

[redis]
host = "localhost"
port = 6379
```

### Environment Variables

```bash
# With APP_ prefix
APP_APP_NAME=MyApp
APP_APP_PORT=3000
APP_APP_DEBUG=false
APP_DATABASE_HOST=localhost
APP_DATABASE_PORT=5432
APP_DATABASE_NAME=myapp
```

## Type Safety

### Schema Definition

```typescript
import { z } from 'zod'

const ServerConfigSchema = z.object({
  port: z.number().min(1).max(65535),
  host: z.string().default('localhost'),
  ssl: z.object({
    enabled: z.boolean().default(false),
    cert: z.string().optional(),
    key: z.string().optional()
  }).optional()
})

type ServerConfig = z.infer<typeof ServerConfigSchema>
```

### Type-Safe Configuration Access

```typescript
const config = validateConfig(rawConfig, ServerConfigSchema)

if (config.tag === 'success') {
  // TypeScript knows the exact type
  const port: number = config.value.port
  const host: string = config.value.host
  const sslEnabled: boolean = config.value.ssl?.enabled ?? false
}
```

## Best Practices

1. **Use multiple sources**: Combine files, environment variables, and defaults
2. **Validate early**: Use schemas to catch configuration errors early
3. **Provide defaults**: Use schema defaults for optional values
4. **Use environment-specific configs**: Separate configs for different environments
5. **Keep secrets separate**: Don't store secrets in configuration files
6. **Document schemas**: Use Zod's describe() for self-documenting schemas
7. **Use subsets**: Create focused configuration objects for services

## Performance Considerations

- Configuration objects are immutable and can be safely shared
- File loading is synchronous and should be done at startup
- Environment variable parsing is cached
- Validation is performed once during configuration building
- Configuration merging creates new objects without mutating originals

## Error Handling

Configuration errors include detailed context about what went wrong:

```typescript
const result = ConfigBuilder.create()
  .fromJsonFile('./missing-file.json')
  .build()

if (result.tag === 'failure') {
  console.log(result.error.code)     // 'CONFIG_FILE_NOT_FOUND'
  console.log(result.error.message)  // 'Configuration file not found'
  console.log(result.error.context)  // { path: './missing-file.json' }
}
```

Common error categories:
- File not found
- Parse errors (invalid JSON/YAML/TOML)
- Validation errors
- Environment variable parsing errors
- Schema validation failures