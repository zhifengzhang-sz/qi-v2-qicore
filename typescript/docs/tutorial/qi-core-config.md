# Tool: Config

## What It Does

Config provides a complete configuration management system with three main concepts:
- **Loaders**: Get configuration data from different sources
- **Merger**: Combine multiple configurations with clear precedence
- **Validator**: Ensure configuration correctness with schemas

**See [Complete API Documentation](../api/core/config.md) for all available functions.**

## Why You Need This

Every application needs to load configuration from multiple sources, merge them with clear precedence, and validate the result. Config solves this common problem with Result<T> error handling.

## Core Concepts

### 1. Loaders
Get configuration data from different sources:

```typescript
// Load from files
ConfigBuilder.fromJsonFile('./config.json')
ConfigBuilder.fromYamlFile('./config.yaml') 
ConfigBuilder.fromTomlFile('./config.toml')

// Load from environment variables
ConfigBuilder.fromEnv('APP_')  // APP_API_URL → api.url

// Load from objects (data from APIs, databases, etc.)
ConfigBuilder.fromObject({ api: { url: 'localhost' } })
```

### 2. Merger
Combine multiple sources with left-to-right precedence (later overrides earlier):

```typescript
const config = ConfigBuilder
  .fromJsonFile('./base.json')        // Base configuration
  .merge(ConfigBuilder.fromEnv('APP_'))     // Environment overrides
  .merge(ConfigBuilder.fromObject(defaults)) // Fallback defaults
```

### 3. Validator
Ensure configuration correctness with schemas:

```typescript
const config = ConfigBuilder
  .fromJsonFile('./config.json')
  .validateWith(configSchema)  // Zod schema validation
  .build()
```

```typescript
// Instead of scattered configuration loading:
let config = {}
try {
  config = JSON.parse(fs.readFileSync('./config.json'))
} catch {
  // Silent failure, use empty config
}
if (process.env.API_URL) config.apiUrl = process.env.API_URL
// No validation, hope for the best

// Use this (explicit, validated):
const configResult = ConfigBuilder
  .fromJsonFile('./config.json')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .validateWith(configSchema)
  .build()
```

## Solving Configuration Problems

### Problem: Remote Configuration
```typescript
// Get config from remote service
const remoteConfig = await fetch('/api/config').then(r => r.json())
const config = ConfigBuilder
  .fromObject(remoteConfig)              // Load from API response
  .merge(ConfigBuilder.fromEnv('APP_'))  // Allow env overrides
  .validateWith(configSchema)            // Ensure correctness
  .build()
```

### Problem: Database Configuration  
```typescript
// Get config from database
const dbConfig = await database.getAppConfig('user-service') 
const config = ConfigBuilder
  .fromObject(dbConfig.data)             // Load from database
  .merge(ConfigBuilder.fromEnv('APP_'))  // Environment overrides
  .build()
```

### Problem: Legacy System Integration
```typescript
// Get config from legacy system
const legacyConfig = legacySystem.getConfiguration()
const config = ConfigBuilder
  .fromObject(legacyConfig)              // Load legacy data
  .subset('newFeatures')                 // Extract relevant parts
  .validateWith(newSchema)               // Validate with new requirements  
  .build()
```

## Basic Usage

### Import What You Need

```typescript
import { ConfigBuilder, type Result, type ConfigError } from '@qi/core'
import { z } from 'zod'
```

### Load Configuration

```typescript
// Define what your config should look like
const configSchema = z.object({
  api: z.object({
    url: z.string().url(),
    timeout: z.number().default(5000)
  }),
  database: z.object({
    host: z.string(),
    port: z.number().default(5432)
  })
})

// Load from multiple sources with clear precedence
const configResult = ConfigBuilder
  .fromJsonFile('./config.json')        // Base configuration
  .merge(ConfigBuilder.fromEnv('APP_'))  // Environment overrides (APP_API_URL, etc.)
  .merge(ConfigBuilder.fromObject({      // Defaults
    api: { timeout: 5000 },
    database: { port: 5432 }
  }))
  .validateWith(configSchema)           // Ensure correctness
  .build()

// Handle the result
match(
  config => {
    // Type-safe access to validated configuration
    const apiUrl = config.get('api.url')
    const dbPort = config.get('database.port')
    console.log(`API: ${apiUrl}, DB Port: ${dbPort}`)
  },
  error => {
    console.error('Configuration error:', error.message)
    process.exit(1)
  },
  configResult
)
```

## Source Precedence

Sources are merged left-to-right, with later sources overriding earlier ones:

```typescript
const config = ConfigBuilder
  .fromJsonFile('./base.json')        // Lowest priority
  .merge(ConfigBuilder.fromYamlFile('./prod.yaml'))  // Overrides base
  .merge(ConfigBuilder.fromEnv('APP_'))              // Highest priority
  .build()
```

## Environment Variables

Environment variables are automatically nested based on naming:

```bash
# Environment variables
APP_API_URL=https://api.example.com
APP_API_TIMEOUT=10000
APP_DATABASE_HOST=localhost
APP_DATABASE_PORT=5432
```

```typescript
// Becomes this structure
{
  api: {
    url: "https://api.example.com",
    timeout: 10000
  },
  database: {
    host: "localhost", 
    port: 5432
  }
}
```

## Validation with Schemas

```typescript
import { z } from 'zod'

const serverSchema = z.object({
  port: z.number().min(1).max(65535),
  host: z.string().default('localhost'),
  ssl: z.object({
    enabled: z.boolean().default(false),
    cert: z.string().optional(),
    key: z.string().optional()
  }).optional()
})

const configResult = ConfigBuilder
  .fromYamlFile('./server.yaml')
  .merge(ConfigBuilder.fromEnv('SERVER_'))
  .validateWith(serverSchema)
  .build()

// If validation passes, config is fully typed
if (configResult.tag === 'success') {
  const config = configResult.value
  // TypeScript knows the exact shape
  const port: number = config.port
  const sslEnabled: boolean = config.ssl?.enabled ?? false
}
```

## Real Example

```typescript
import { ConfigBuilder, createLogger } from '@qi/core'
import { match } from '@qi/base'
import { z } from 'zod'

// Define application configuration schema
const appSchema = z.object({
  app: z.object({
    name: z.string(),
    port: z.number().default(3000),
    environment: z.enum(['development', 'production', 'test']).default('development')
  }),
  database: z.object({
    host: z.string(),
    port: z.number().default(5432),
    name: z.string(),
    ssl: z.boolean().default(false)
  }),
  logging: z.object({
    level: z.enum(['trace', 'debug', 'info', 'warn', 'error']).default('info'),
    pretty: z.boolean().default(false)
  })
})

// Load configuration
const configResult = ConfigBuilder
  .fromYamlFile('./config/base.yaml')
  .merge(ConfigBuilder.fromYamlFile(`./config/${process.env.NODE_ENV || 'development'}.yaml`))
  .merge(ConfigBuilder.fromEnv('APP_'))
  .validateWith(appSchema)
  .build()

match(
  config => {
    // Create logger with configuration
    const loggerResult = createLogger({
      level: config.logging.level,
      pretty: config.logging.pretty,
      name: config.app.name
    })
    
    match(
      logger => {
        logger.info('Application starting', undefined, {
          name: config.app.name,
          port: config.app.port,
          environment: config.app.environment
        })
        
        // Start your application with validated config
        startServer(config, logger)
      },
      error => {
        console.error('Logger creation failed:', error.message)
        process.exit(1)
      },
      loggerResult
    )
  },
  error => {
    console.error('Configuration error:', error.message)
    if (error.context?.validationErrors) {
      console.error('Validation errors:', error.context.validationErrors)
    }
    process.exit(1)
  },
  configResult
)
```

## File Format Examples

### YAML Configuration (config/base.yaml)
```yaml
app:
  name: MyApp
  port: 3000

database:
  host: localhost
  name: myapp

logging:
  level: info
```

### Environment Override (.env)
```bash
APP_APP_PORT=8080
APP_DATABASE_HOST=prod-db.example.com
APP_DATABASE_SSL=true
APP_LOGGING_LEVEL=warn
```

## Integration with Other Tools

Config integrates naturally with Logger and Cache:

```typescript
// Use config to create other services
match(
  config => {
    // Create logger from config
    const loggerResult = createLogger({
      level: config.logging.level,
      name: config.app.name
    })
    
    // Create cache from config  
    const cacheResult = createCache({
      backend: 'redis',
      redis: {
        host: config.redis.host,
        port: config.redis.port
      }
    })
    
    // All tools use the same Result<T> patterns
  },
  error => console.error('Config failed:', error.message),
  configResult
)
```

## Key Benefits

1. **Multiple sources** - Files, environment variables, and objects
2. **Clear precedence** - Later sources override earlier ones
3. **Type safety** - Validation ensures correctness at startup
4. **Explicit errors** - Know exactly what configuration is missing or invalid
5. **Consistent patterns** - Uses Result<T> like all other tools

## Working Example

Try the complete working example:

```bash
cd typescript/app/config-example
bun run dev
```

This shows Config + Logger integration with YAML loading, environment merging, and schema validation.

## Next Steps

- [Logger Tool](./qi-core-logger.md) - Use config to set up structured logging
- [Cache Tool](./qi-core-cache.md) - Use config to configure caching backends
- [Back to qi/base](./qi-base.md) - Review the framework patterns