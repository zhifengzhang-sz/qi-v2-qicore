# Configuration Management Example

This example demonstrates comprehensive configuration management using QiCore Foundation with schema validation, multiple sources, and environment-specific overrides.

## Features Demonstrated

- **Multiple Configuration Sources**: Files, environment variables, defaults
- **Schema Validation**: Type-safe configuration with Zod schemas
- **Environment-Specific Configs**: Development, staging, production
- **Hot Reloading**: Configuration watching and updates
- **Validation Error Handling**: Detailed error messages and recovery
- **Nested Configuration**: Complex nested structures with validation
- **Secret Management**: Secure handling of sensitive configuration

## Project Structure

```
config-example/
├── configs/
│   ├── base.yaml           # Base configuration
│   ├── development.yaml    # Development overrides
│   ├── production.yaml     # Production overrides
│   └── secrets.yaml        # Secret values (gitignored)
├── src/
│   ├── schemas/
│   │   ├── app.schema.ts   # Application schema
│   │   ├── db.schema.ts    # Database schema
│   │   └── auth.schema.ts  # Authentication schema
│   ├── config/
│   │   ├── loader.ts       # Configuration loader
│   │   ├── validator.ts    # Configuration validator
│   │   └── watcher.ts      # Configuration watcher
│   ├── services/
│   │   ├── database.ts     # Database service using config
│   │   ├── auth.ts         # Auth service using config
│   │   └── cache.ts        # Cache service using config
│   └── index.ts            # Main application
├── package.json
└── README.md
```

## Configuration Schema

The example uses comprehensive Zod schemas for type safety:

```typescript
// Complete application configuration schema
const AppConfigSchema = z.object({
  app: z.object({
    name: z.string(),
    version: z.string(),
    port: z.number().min(1).max(65535),
    host: z.string().default('localhost'),
    environment: z.enum(['development', 'staging', 'production']),
    debug: z.boolean().default(false),
    cors: z.object({
      enabled: z.boolean().default(true),
      origins: z.array(z.string()).default(['*']),
      methods: z.array(z.string()).default(['GET', 'POST', 'PUT', 'DELETE'])
    })
  }),
  database: z.object({
    host: z.string(),
    port: z.number(),
    name: z.string(),
    username: z.string(),
    password: z.string(),
    ssl: z.boolean().default(false),
    pool: z.object({
      min: z.number().default(2),
      max: z.number().default(10),
      idle: z.number().default(10000)
    })
  }),
  auth: z.object({
    jwt: z.object({
      secret: z.string().min(32),
      expiresIn: z.string().default('24h'),
      issuer: z.string(),
      audience: z.string()
    }),
    oauth: z.object({
      google: z.object({
        clientId: z.string(),
        clientSecret: z.string(),
        redirectUri: z.string()
      }).optional(),
      github: z.object({
        clientId: z.string(),
        clientSecret: z.string(),
        redirectUri: z.string()
      }).optional()
    })
  }),
  cache: z.object({
    backend: z.enum(['memory', 'redis']),
    ttl: z.number().default(3600),
    redis: z.object({
      host: z.string(),
      port: z.number(),
      password: z.string().optional(),
      db: z.number().default(0)
    }).optional()
  }),
  logging: z.object({
    level: z.enum(['trace', 'debug', 'info', 'warn', 'error']),
    format: z.enum(['json', 'pretty']).default('json'),
    output: z.enum(['console', 'file', 'both']).default('console'),
    file: z.object({
      path: z.string(),
      maxSize: z.string().default('10MB'),
      maxFiles: z.number().default(5)
    }).optional()
  })
})
```

## Running the Example

```bash
# Install dependencies
bun install

# Run with development config
bun run dev

# Run with production config
NODE_ENV=production bun run start

# Run with custom config file
CONFIG_FILE=./configs/staging.yaml bun run start

# Run with environment variables
DATABASE_HOST=localhost DATABASE_PORT=5432 bun run start

# Watch for config changes
bun run watch
```

## Key Learning Points

### 1. Multiple Configuration Sources
- Base configuration files (YAML/JSON)
- Environment-specific overrides
- Environment variables with prefixes
- Command-line arguments
- Default values in schemas

### 2. Schema Validation
- Type-safe configuration access
- Validation error handling
- Custom validation rules
- Nested schema composition
- Optional and required fields

### 3. Environment Management
- Environment-specific configurations
- Secret management patterns
- Configuration precedence rules
- Environment variable mapping

### 4. Hot Reloading
- File watching for configuration changes
- Safe configuration updates
- Service restart on config changes
- Error handling during updates

### 5. Service Integration
- Dependency injection with configuration
- Service-specific configuration subsets
- Configuration validation per service
- Runtime configuration updates

## Advanced Features

### Configuration Watching
```typescript
const watcher = new ConfigWatcher('./configs/')
watcher.onChange((newConfig) => {
  console.log('Configuration updated:', newConfig)
  // Restart services with new config
})
```

### Environment-Specific Loading
```typescript
const config = await loadConfig({
  environment: process.env.NODE_ENV || 'development',
  configDir: './configs/',
  secretsFile: './configs/secrets.yaml'
})
```

### Service Configuration Injection
```typescript
class DatabaseService {
  constructor(config: DatabaseConfig) {
    this.pool = createPool(config)
  }
}

const dbService = new DatabaseService(config.database)
```

This example provides a complete foundation for configuration management in production applications.