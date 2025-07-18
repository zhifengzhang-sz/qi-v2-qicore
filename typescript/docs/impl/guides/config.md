# Configuration Implementation Guide

## Overview

The Configuration module provides a fluent builder pattern for loading, merging, and validating configuration data from multiple sources. It implements monoid semantics for configuration merging and provides type-safe access to configuration values.

## Design Approach

**Fluent builder pattern with fail-fast validation**

### Rationale
- Configuration is inherently procedural (load → merge → validate → use)
- Builders provide better error messages with stack traces
- Fail-fast is appropriate for configuration
- Natural chaining matches mental model

### Key Decisions
- Throw on errors (configuration should fail fast)
- Immutable operations (each method returns new builder)
- Escape hatch to Result<T> when needed
- Support for multiple sources with precedence

## Core Interface

### ConfigBuilder Pattern
```typescript
interface ConfigBuilder {
  fromFile(path: string): ConfigBuilder
  fromObject(obj: Record<string, unknown>): ConfigBuilder
  fromEnvironment(prefix?: string): ConfigBuilder
  merge(other: ConfigBuilder): ConfigBuilder
  validate<T>(schema: Schema<T>): ConfigBuilder
  build(): ConfigData
  buildResult(): Result<ConfigData, ConfigError>
}
```

### Configuration Data
```typescript
interface ConfigData {
  get<T>(key: string): Result<T, ConfigError>
  getWithDefault<T>(key: string, defaultValue: T): T
  has(key: string): boolean
  keys(): string[]
  toObject(): Record<string, unknown>
}
```

## Factory Functions

### Core Factories
- `ConfigBuilder.empty()` - Create empty configuration builder
- `ConfigBuilder.fromFile(path: string)` - Load configuration from file
- `ConfigBuilder.fromObject(obj: Record<string, unknown>)` - Create from object
- `ConfigBuilder.fromEnvironment(prefix?: string)` - Load from environment variables

### Usage Examples

```typescript
// Simple file-based configuration
const config = ConfigBuilder
  .fromFile('./config.json')
  .build()

// Multi-source configuration with precedence
const config = ConfigBuilder
  .fromFile('./config/base.json')
  .merge(ConfigBuilder.fromFile('./config/production.json'))
  .merge(ConfigBuilder.fromEnvironment('APP_'))
  .validate(configSchema)
  .build()

// With error handling
const configResult = ConfigBuilder
  .fromFile('./config.json')
  .buildResult()

if (configResult.tag === 'success') {
  const config = configResult.value
  // Use configuration
}
```

## Contract Compliance

### Factory Operations
- `fromFile: FilePath → Result<ConfigData>` - ✅ Implemented
- `fromObject: Object → Result<ConfigData>` - ✅ Implemented  
- `fromEnvironment: Prefix? → Result<ConfigData>` - ✅ Implemented

### Query Operations
- `get: Key → ConfigData → Result<Value>` - ✅ Implemented
- `getWithDefault: Key → DefaultValue → ConfigData → Value` - ✅ Implemented
- `has: Key → ConfigData → Boolean` - ✅ Implemented

### Transformation Operations
- `merge: List<ConfigData> → Result<ConfigData>` - ✅ Implemented
- `empty: () → ConfigData` - ✅ Implemented
- `validate: Schema → ConfigData → Result<ConfigData>` - ✅ Implemented

## Supported Configuration Formats

### JSON Configuration
```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "ssl": true
  },
  "logging": {
    "level": "info"
  }
}
```

### YAML Configuration
```yaml
database:
  host: localhost
  port: 5432
  ssl: true
logging:
  level: info
```

### TOML Configuration
```toml
[database]
host = "localhost"
port = 5432
ssl = true

[logging]
level = "info"
```

### Environment Variables
```bash
# With prefix APP_
APP_DATABASE_HOST=localhost
APP_DATABASE_PORT=5432
APP_DATABASE_SSL=true
APP_LOGGING_LEVEL=info

# Converted to nested structure:
# database.host = "localhost"
# database.port = 5432
# database.ssl = true
# logging.level = "info"
```

## Configuration Merging

### Monoid Semantics
Configuration follows mathematical monoid laws:
- **Associativity**: `merge([a, merge([b, c])]) == merge([merge([a, b]), c])`
- **Left Identity**: `merge([empty, config]) == config`
- **Right Identity**: `merge([config, empty]) == config`

### Merge Behavior
- **Right-biased**: Later configurations override earlier ones
- **Deep merge**: Nested objects merged recursively
- **Array handling**: Arrays are replaced, not merged

### Examples
```typescript
// Base configuration
const base = {
  database: { host: 'localhost', port: 5432 },
  features: { auth: true }
}

// Override configuration
const override = {
  database: { port: 3306 },
  features: { cache: true }
}

// Merged result
const merged = {
  database: { host: 'localhost', port: 3306 },  // Deep merge
  features: { auth: true, cache: true }         // Deep merge
}
```

## Type Safety Features

### Template Literal Types
```typescript
// Type-safe key paths
type ConfigKey = 'database.host' | 'database.port' | 'logging.level'

const host = config.get<string>('database.host')
const port = config.get<number>('database.port')
```

### Branded Types
```typescript
// Prevent configuration key confusion
type DatabaseConfig = ConfigData & { readonly __brand: 'database' }
type LoggingConfig = ConfigData & { readonly __brand: 'logging' }
```

### Schema Validation
```typescript
import { z } from 'zod'

const configSchema = z.object({
  database: z.object({
    host: z.string(),
    port: z.number().int().positive(),
    ssl: z.boolean().default(false)
  }),
  logging: z.object({
    level: z.enum(['debug', 'info', 'warn', 'error'])
  })
})

const config = ConfigBuilder
  .fromFile('./config.json')
  .validate(configSchema)
  .build()
```

## Error Handling

### Error Types
```typescript
export type ConfigError = QiError & {
  readonly category: 'VALIDATION' | 'PARSING' | 'FILESYSTEM'
  readonly context: {
    readonly operation?: string
    readonly key?: string
    readonly source?: string
    readonly format?: string
  }
}
```

### Error Categories
- **VALIDATION**: Invalid configuration values or missing required fields
- **PARSING**: Invalid file format or syntax errors
- **FILESYSTEM**: File access issues or missing files

### Error Handling Examples
```typescript
// Handle different error types
const configResult = ConfigBuilder
  .fromFile('./config.json')
  .buildResult()

if (configResult.tag === 'failure') {
  const error = configResult.error
  
  switch (error.category) {
    case 'FILESYSTEM':
      console.error('Config file not found:', error.message)
      break
    case 'PARSING':
      console.error('Invalid config format:', error.message)
      break
    case 'VALIDATION':
      console.error('Invalid config values:', error.message)
      break
  }
}
```

## Performance Characteristics

### Loading Performance
- **File Reading**: Asynchronous file operations
- **Parsing**: Optimized JSON/YAML/TOML parsers
- **Merging**: O(n) complexity for configuration size

### Query Performance
- **Direct Key Access**: O(1) for simple keys
- **Nested Key Access**: O(depth) for nested paths
- **Default Values**: O(1) fallback handling

### Memory Usage
- **Immutable Data**: Structural sharing for nested objects
- **Lazy Evaluation**: Values parsed only when accessed
- **Caching**: Parsed values cached for repeated access

## Advanced Features

### Configuration Watchers
```typescript
// Watch for configuration changes
const watcher = config.watch((changes) => {
  console.log('Configuration changed:', changes)
})

// Stop watching
watcher.stop()
```

### Hot Reloading
```typescript
// Automatic configuration reloading
const reloader = config.enableHotReload()

// Configuration automatically updates when files change
```

### Configuration Validation
```typescript
// Runtime validation with detailed errors
const validateConfig = (config: ConfigData): ValidationResult => {
  const errors: ValidationError[] = []
  
  // Validate database configuration
  if (!config.has('database.host')) {
    errors.push({ key: 'database.host', message: 'Required field missing' })
  }
  
  // Validate port range
  const port = config.get<number>('database.port')
  if (port.tag === 'success' && (port.value < 1 || port.value > 65535)) {
    errors.push({ key: 'database.port', message: 'Port must be between 1 and 65535' })
  }
  
  return errors.length === 0 ? { valid: true } : { valid: false, errors }
}
```

## Integration Examples

### Express Application
```typescript
// Load configuration for Express app
const config = ConfigBuilder
  .fromFile('./config/default.json')
  .merge(ConfigBuilder.fromFile(`./config/${process.env.NODE_ENV}.json`))
  .merge(ConfigBuilder.fromEnvironment('APP_'))
  .validate(appConfigSchema)
  .build()

const app = express()
app.listen(config.get<number>('server.port').value)
```

### Database Connection
```typescript
// Database configuration
const dbConfig = config.get<DatabaseConfig>('database')
if (dbConfig.tag === 'success') {
  const connection = await createConnection({
    host: dbConfig.value.host,
    port: dbConfig.value.port,
    ssl: dbConfig.value.ssl
  })
}
```

### Feature Flags
```typescript
// Feature flag configuration
const features = config.get<Record<string, boolean>>('features')
if (features.tag === 'success') {
  if (features.value.newAuth) {
    // Enable new authentication system
  }
}
```

## Testing Configuration

### Unit Testing
```typescript
describe('Configuration', () => {
  it('should load from object', () => {
    const config = ConfigBuilder
      .fromObject({ key: 'value' })
      .build()
    
    const result = config.get<string>('key')
    expect(result.tag).toBe('success')
    expect(result.value).toBe('value')
  })
  
  it('should merge configurations', () => {
    const config = ConfigBuilder
      .fromObject({ a: 1, b: 2 })
      .merge(ConfigBuilder.fromObject({ b: 3, c: 4 }))
      .build()
    
    expect(config.get<number>('a').value).toBe(1)
    expect(config.get<number>('b').value).toBe(3) // Override
    expect(config.get<number>('c').value).toBe(4)
  })
})
```

### Integration Testing
```typescript
// Test configuration loading from files
const testConfig = ConfigBuilder
  .fromFile('./test/fixtures/config.json')
  .build()

// Test environment variable loading
process.env.TEST_KEY = 'test_value'
const envConfig = ConfigBuilder
  .fromEnvironment('TEST_')
  .build()
```

## Best Practices

### Configuration Organization
- **Separate by environment**: base.json, development.json, production.json
- **Use meaningful prefixes**: APP_, DATABASE_, CACHE_
- **Group related settings**: database.*, logging.*, features.*

### Security Considerations
- **Sensitive data**: Use environment variables for secrets
- **Validation**: Always validate configuration before use
- **Access control**: Restrict configuration file permissions

### Performance Optimization
- **Lazy loading**: Load configuration only when needed
- **Caching**: Cache parsed configuration values
- **Hot reloading**: Only reload when files actually change

## Migration Guide

### From dotenv
```typescript
// Before
require('dotenv').config()
const dbHost = process.env.DB_HOST

// After
const config = ConfigBuilder
  .fromEnvironment('DB_')
  .build()
const dbHost = config.get<string>('host').value
```

### From config package
```typescript
// Before
const config = require('config')
const dbHost = config.get('database.host')

// After
const config = ConfigBuilder
  .fromFile('./config/default.json')
  .merge(ConfigBuilder.fromFile(`./config/${process.env.NODE_ENV}.json`))
  .build()
const dbHost = config.get<string>('database.host').value
```

## Troubleshooting

### Common Issues
1. **File not found**: Check file paths and permissions
2. **Parse errors**: Validate JSON/YAML/TOML syntax
3. **Environment variables**: Verify variable names and prefixes
4. **Type mismatches**: Ensure configuration values match expected types

### Debug Information
```typescript
// Enable debug logging
const config = ConfigBuilder
  .fromFile('./config.json')
  .debug(true)
  .build()

// Log configuration sources
console.log('Configuration sources:', config.getSources())

// Log final configuration
console.log('Final configuration:', config.toObject())
```