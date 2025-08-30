# JSON Schema Integration with Zod v4

**Using JSON Schema files for dynamic validation in QiCore Foundation**

## Overview

QiCore Foundation's `@qi/core/config` module supports both static Zod schemas and dynamic JSON Schema files. This enables configuration validation from external schema files, making applications more flexible and maintainable.

## The validateWithSchemaFile Function

### Function Signature
```typescript
validateWithSchemaFile(schemaPath: string): ConfigBuilder
```

### Implementation Details
Located in `/lib/core/src/config.ts:597-618`:

```typescript
validateWithSchemaFile(schemaPath: string): ConfigBuilder {
  return new ConfigBuilder({
    ...this.options,
    schemaFile: schemaPath,
  })
}
```

The actual validation happens during the `build()` process, where the JSON Schema file is loaded and converted to a Zod schema.

## JSON Schema to Zod Conversion

### Dependencies
```json
{
  "dependencies": {
    "zod-from-json-schema": "^0.5.0",
    "zod": "^4.1.5"
  }
}
```

### Conversion Process
```typescript
import { zodFromJsonSchema } from 'zod-from-json-schema'
import { readFileSync } from 'fs'

// Load JSON Schema file
const jsonSchemaContent = JSON.parse(readFileSync(schemaPath, 'utf-8'))

// Convert to Zod schema
const zodSchema = zodFromJsonSchema(jsonSchemaContent)

// Use with validation (zodSchema is compatible with z.ZodType)
const result = validateConfig(config, zodSchema)
```

## JSON Schema File Examples

### Basic Configuration Schema
**config.schema.json:**
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Application Configuration",
  "description": "Configuration schema for the application",
  "properties": {
    "server": {
      "type": "object",
      "properties": {
        "port": {
          "type": "number",
          "default": 3000,
          "minimum": 1000,
          "maximum": 65535,
          "description": "Server port number"
        },
        "host": {
          "type": "string",
          "default": "localhost",
          "pattern": "^[a-zA-Z0-9.-]+$",
          "description": "Server hostname"
        }
      },
      "required": ["port"],
      "additionalProperties": false
    },
    "database": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string",
          "format": "uri",
          "description": "Database connection URL"
        },
        "ssl": {
          "type": "boolean",
          "default": false,
          "description": "Enable SSL connection"
        },
        "poolSize": {
          "type": "number",
          "default": 10,
          "minimum": 1,
          "maximum": 100
        }
      },
      "required": ["url"]
    }
  },
  "required": ["server", "database"],
  "additionalProperties": false
}
```

### Advanced Schema with Complex Types
**advanced-config.schema.json:**
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "logging": {
      "type": "object",
      "properties": {
        "level": {
          "type": "string",
          "enum": ["debug", "info", "warn", "error"],
          "default": "info"
        },
        "outputs": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "type": {
                "type": "string",
                "enum": ["console", "file", "syslog"]
              },
              "format": {
                "type": "string",
                "enum": ["json", "text", "pretty"],
                "default": "json"
              },
              "path": {
                "type": "string",
                "description": "File path (required for file output)"
              }
            },
            "required": ["type"],
            "if": {
              "properties": { "type": { "const": "file" } }
            },
            "then": {
              "required": ["path"]
            }
          },
          "minItems": 1,
          "default": [{"type": "console", "format": "pretty"}]
        }
      }
    },
    "features": {
      "type": "object",
      "properties": {
        "cache": {
          "type": "object",
          "properties": {
            "enabled": {
              "type": "boolean",
              "default": true
            },
            "backend": {
              "type": "string",
              "enum": ["memory", "redis"],
              "default": "memory"
            },
            "ttl": {
              "type": "number",
              "minimum": 1,
              "default": 3600,
              "description": "Cache TTL in seconds"
            }
          }
        }
      }
    }
  }
}
```

## Usage Patterns

### Basic File-Based Validation
```typescript
import { ConfigBuilder } from '@qi/core/config'

// Load and validate configuration from JSON schema file
const configResult = await ConfigBuilder
  .fromEnv('APP')
  .merge(ConfigBuilder.fromYamlFile('config.yaml'))
  .validateWithSchemaFile('./schemas/config.schema.json')
  .build()

await match(
  (config) => {
    console.log('Configuration loaded:', config.toObject())
    // config is now validated against the JSON schema
  },
  (error) => {
    console.error('Configuration validation failed:', error.message)
  },
  configResult
)
```

### Multiple Schema Files
```typescript
// Different schemas for different environments
const getSchemaPath = (env: string) => {
  switch (env) {
    case 'development':
      return './schemas/dev-config.schema.json'
    case 'production':
      return './schemas/prod-config.schema.json'
    case 'test':
      return './schemas/test-config.schema.json'
    default:
      return './schemas/default-config.schema.json'
  }
}

const configResult = await ConfigBuilder
  .fromEnv('APP')
  .validateWithSchemaFile(getSchemaPath(process.env.NODE_ENV || 'development'))
  .build()
```

### Schema Composition
```typescript
// Load base schema and extend it
const baseConfigResult = await ConfigBuilder
  .fromYamlFile('base-config.yaml')
  .validateWithSchemaFile('schemas/base.schema.json')
  .build()

// Then load feature-specific config
const fullConfigResult = await match(
  async (baseConfig) => ConfigBuilder
    .fromConfig(baseConfig)
    .merge(ConfigBuilder.fromEnv('FEATURES'))
    .validateWithSchemaFile('schemas/features.schema.json')
    .build(),
  (error) => Promise.resolve(failure(error)),
  baseConfigResult
)
```

## Schema File Organization

### Recommended Directory Structure
```
project/
├── schemas/
│   ├── common/
│   │   ├── server.schema.json
│   │   ├── database.schema.json
│   │   └── logging.schema.json
│   ├── environments/
│   │   ├── development.schema.json
│   │   ├── production.schema.json
│   │   └── test.schema.json
│   └── features/
│       ├── auth.schema.json
│       ├── cache.schema.json
│       └── monitoring.schema.json
├── config/
│   ├── development.yaml
│   ├── production.yaml
│   └── test.yaml
└── src/
    └── config.ts
```

### Schema References and Composition
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "server": {
      "$ref": "./common/server.schema.json"
    },
    "database": {
      "$ref": "./common/database.schema.json"
    },
    "features": {
      "type": "object",
      "properties": {
        "auth": {
          "$ref": "./features/auth.schema.json"
        },
        "cache": {
          "$ref": "./features/cache.schema.json"
        }
      }
    }
  }
}
```

## Error Handling and Validation

### Schema Loading Errors
```typescript
// Handle schema file not found or invalid JSON
const configResult = await ConfigBuilder
  .fromEnv('APP')
  .validateWithSchemaFile('./nonexistent.schema.json')
  .build()

// Returns Result<Config, QiError> with appropriate error
await match(
  (config) => console.log('Success:', config),
  (error) => {
    if (error.category === 'VALIDATION_ERROR') {
      console.error('Schema validation failed:', error.message)
      console.error('Context:', error.context)
    }
  },
  configResult
)
```

### Validation Error Details
```typescript
// JSON Schema validation errors are detailed
const result = validateConfig(config, schemaFromFile)

await match(
  (validConfig) => console.log('Valid:', validConfig),
  (error) => {
    console.error('Validation failed:', error.message)
    
    // Error context contains Zod validation issues
    if (error.context?.issues) {
      for (const issue of error.context.issues) {
        console.error(`  ${issue.path.join('.')}: ${issue.message}`)
      }
    }
  },
  result
)
```

## Integration with TypeScript

### Type Generation from JSON Schema
While JSON Schema files provide runtime validation, you can generate TypeScript types:

```bash
# Install json-schema-to-typescript
npm install -D json-schema-to-typescript

# Generate types
json2ts -i schemas/config.schema.json -o src/types/config.ts
```

### Using Generated Types
```typescript
import type { Config } from './types/config'

const configResult = await ConfigBuilder
  .fromEnv('APP')
  .validateWithSchemaFile('./schemas/config.schema.json')
  .build()

await match(
  (config: Config) => {
    // config is now properly typed
    console.log(`Server running on port ${config.server.port}`)
  },
  (error) => console.error(error.message),
  configResult
)
```

## Performance Considerations

### Schema Caching
```typescript
// Cache parsed schemas to avoid repeated file I/O
const schemaCache = new Map<string, z.ZodType>()

const getCachedSchema = (schemaPath: string): z.ZodType => {
  if (!schemaCache.has(schemaPath)) {
    const jsonSchema = JSON.parse(readFileSync(schemaPath, 'utf-8'))
    const zodSchema = zodFromJsonSchema(jsonSchema)
    schemaCache.set(schemaPath, zodSchema)
  }
  return schemaCache.get(schemaPath)!
}
```

### Schema Validation Performance
- JSON Schema conversion happens once during build
- Subsequent validations use the compiled Zod schema
- Large schemas may have initial conversion overhead
- Consider schema splitting for complex configurations

## Best Practices

1. **Version your schemas**: Include version in schema files for migration support
2. **Use descriptive properties**: Add descriptions and examples to schema properties
3. **Provide defaults**: Set sensible defaults in schemas, not just code
4. **Validate early**: Validate configuration at application startup
5. **Schema evolution**: Design schemas for backward compatibility
6. **Environment-specific**: Use different schemas for different deployment environments
7. **Documentation**: Keep schema files as living documentation

## Testing JSON Schema Integration

### Unit Tests
```typescript
import { test, expect } from 'vitest'
import { ConfigBuilder } from '@qi/core/config'

test('validates configuration with JSON schema file', async () => {
  const configResult = await ConfigBuilder
    .fromObject({
      server: { port: 3000, host: 'localhost' },
      database: { url: 'postgresql://localhost/test', ssl: false }
    })
    .validateWithSchemaFile('./test/fixtures/config.schema.json')
    .build()

  expect(configResult.isSuccess()).toBe(true)
})

test('fails validation with invalid configuration', async () => {
  const configResult = await ConfigBuilder
    .fromObject({
      server: { port: 'invalid' }, // Should be number
      database: {} // Missing required url
    })
    .validateWithSchemaFile('./test/fixtures/config.schema.json')
    .build()

  expect(configResult.isFailure()).toBe(true)
})
```

## Real-World Examples

See working examples in:
- `/app/config-example/` - Complete configuration loading with schema validation
- `/lib/core/tests/config.test.ts` - Comprehensive test cases
- `/lib/core/src/config.ts:597-618` - Implementation details

The JSON Schema integration provides powerful, flexible configuration validation that can adapt to changing requirements without code changes.