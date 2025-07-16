# External Schema Definition Research: QiCore Foundation Solutions

## Problem Statement

The QiCore Config system currently has Zod schemas hardcoded in TypeScript source code. This creates problems:
- Changing config structure requires code changes and recompilation
- Users can't define schemas externally
- Schema updates require developer intervention

## Research Findings

### 1. JSON Schema ↔ Zod Conversion Libraries

**json-schema-to-zod**
- Converts JSON Schema (draft 4+) to Zod schemas as JavaScript code
- CLI and runtime support
- Supports external references via `json-refs`
- Handles recursive schemas with max depth configuration

**zod-to-json-schema**
- Converts Zod schemas to JSON Schema
- Supports all relevant schema types
- Handles recursive schemas with internal $refs
- Targets OpenAPI 3.0 and JSON Schema specifications

### 2. Direct JSON Schema Validation

**AJV (Another JSON Schema Validator)**
- Fastest JSON Schema validator
- Supports JSON Schema draft-04/06/07/2019-09/2020-12
- TypeScript integration with `JSONSchemaType` utility
- Runtime validation without Zod dependency

**Standard Schema Initiative**
- Universal interface for validation libraries
- Supported by Zod, Valibot, and ArkType creators
- Enables library-agnostic validation code

### 3. Schema Registry Patterns

**Confluent Schema Registry**
- Centralized schema management
- Versioning and evolution support
- Supports Avro, JSON Schema, and Protobuf

**Azure Schema Registry**
- Cloud-based schema management
- JSON Schema support with Kafka integration
- Event-driven schema updates

### 4. TypeScript Integration Approaches

**TypeSchema**
- Universal adapter for validation libraries
- Decouples from specific validation implementations
- Works with any schema library

**TypeBox**
- JSON Schema definition with TypeScript types
- Runtime validation with compile-time types
- Node.js focused implementation

## Recommended Solution: Runtime JSON Schema → Zod Conversion

### Why This Approach

1. **Maintains QiCore's Zod ecosystem** - No breaking changes to existing Config API
2. **True external schema definition** - Users can modify schemas without code changes
3. **Runtime validation** - Catches configuration errors early with proper error messages
4. **Backward compatible** - Existing Zod schemas continue to work
5. **Reasonable complexity** - Clear implementation path, not over-engineered

### Implementation Design

#### Enhanced Config API

```typescript
// New factory functions for external schema
export const fromJsonFileWithSchema = async (
  configPath: string,
  schemaPath: string
): Promise<Result<Config, ConfigError>>

export const fromYamlFileWithSchema = async (
  configPath: string,
  schemaPath: string
): Promise<Result<Config, ConfigError>>

// Builder pattern extensions
ConfigBuilder.fromJsonFile('config.json')
  .withExternalSchema('config.schema.json')
  .build()

// Auto-discovery pattern
ConfigBuilder.fromJsonFile('config.json') // automatically looks for config.schema.json
  .build()
```

#### Schema Cache Implementation

```typescript
interface SchemaCache {
  readonly zodSchemas: Map<string, ZodSchema<unknown>>
  readonly jsonSchemas: Map<string, JSONSchema>
  readonly lastModified: Map<string, number>
}

class ExternalSchemaManager {
  private cache: SchemaCache = {
    zodSchemas: new Map(),
    jsonSchemas: new Map(),
    lastModified: new Map()
  }

  async loadJsonSchema(schemaPath: string): Promise<Result<ZodSchema<unknown>, ConfigError>> {
    // Check cache and file modification time
    const lastMod = await this.getFileModTime(schemaPath)
    const cached = this.cache.lastModified.get(schemaPath)
    
    if (cached && cached === lastMod) {
      const cachedSchema = this.cache.zodSchemas.get(schemaPath)
      if (cachedSchema) return Ok(cachedSchema)
    }

    // Load and convert schema
    const jsonSchemaResult = await this.loadJsonSchemaFile(schemaPath)
    if (jsonSchemaResult.tag === 'failure') return jsonSchemaResult

    const zodSchemaResult = await this.convertToZod(jsonSchemaResult.value)
    if (zodSchemaResult.tag === 'failure') return zodSchemaResult

    // Cache the result
    this.cache.zodSchemas.set(schemaPath, zodSchemaResult.value)
    this.cache.lastModified.set(schemaPath, lastMod)

    return Ok(zodSchemaResult.value)
  }

  private async convertToZod(jsonSchema: JSONSchema): Promise<Result<ZodSchema<unknown>, ConfigError>> {
    try {
      const { resolved } = await resolveRefs(jsonSchema)
      const zodCode = jsonSchemaToZod(resolved)
      
      // Evaluate the generated Zod code
      const zodSchema = this.evaluateZodCode(zodCode)
      return Ok(zodSchema)
    } catch (error) {
      return Err(configError(`Failed to convert JSON Schema to Zod: ${error}`, {
        schema: 'json-schema-to-zod'
      }))
    }
  }
}
```

#### File Structure

```
configs/
├── base.json                    # Base configuration
├── base.schema.json            # Base schema definition
├── development.json            # Development overrides
├── development.schema.json     # Development schema (inherits base)
├── production.json             # Production overrides
└── production.schema.json      # Production schema (inherits base)
```

#### Example Usage

```typescript
// User's config.schema.json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "app": {
      "type": "object",
      "properties": {
        "name": { "type": "string" },
        "port": { "type": "number", "minimum": 1, "maximum": 65535 },
        "debug": { "type": "boolean", "default": false }
      },
      "required": ["name", "port"],
      "additionalProperties": false
    },
    "database": {
      "type": "object",
      "properties": {
        "host": { "type": "string" },
        "port": { "type": "number", "minimum": 1, "maximum": 65535 },
        "name": { "type": "string" }
      },
      "required": ["host", "port", "name"],
      "additionalProperties": false
    }
  },
  "required": ["app", "database"],
  "additionalProperties": false
}

// User's config.json
{
  "app": {
    "name": "My Application",
    "port": 3000,
    "debug": true
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "myapp_db"
  }
}

// Application code - no changes needed when schema changes
async function loadConfig() {
  const configResult = await ConfigBuilder
    .fromJsonFileWithSchema('config.json', 'config.schema.json')
    .then(result => result.tag === 'success' ? result.value.build() : result)

  if (configResult.tag === 'failure') {
    console.error('Config validation failed:', configResult.error.message)
    process.exit(1)
  }

  return configResult.value
}
```

### Alternative Approach: Direct JSON Schema Validation

For simpler use cases, direct JSON Schema validation with AJV:

```typescript
import Ajv from 'ajv'

class JsonSchemaConfig {
  private ajv = new Ajv({ allErrors: true })

  async fromJsonFileWithSchema<T = unknown>(
    configPath: string,
    schemaPath: string
  ): Promise<Result<Config<T>, ConfigError>> {
    const [configData, schemaData] = await Promise.all([
      this.loadJsonFile(configPath),
      this.loadJsonFile(schemaPath)
    ])

    if (configData.tag === 'failure') return configData
    if (schemaData.tag === 'failure') return schemaData

    const validate = this.ajv.compile(schemaData.value)
    const valid = validate(configData.value)

    if (!valid) {
      return Err(configError('JSON Schema validation failed', {
        schema: 'ajv',
        context: { errors: validate.errors }
      }))
    }

    return Ok(new Config(configData.value as T))
  }
}
```

### Performance Considerations

1. **Schema Caching**: Cache converted Zod schemas to avoid repeated conversion
2. **File Watching**: Optionally watch schema files for changes in development
3. **Lazy Loading**: Load schemas only when needed
4. **Bundle Size**: `json-schema-to-zod` adds ~100KB to bundle size

### Migration Strategy

1. **Phase 1**: Add external schema support alongside existing Zod schemas
2. **Phase 2**: Provide migration tools to convert existing Zod schemas to JSON Schema
3. **Phase 3**: Deprecate hardcoded Zod schemas in favor of external definitions

## Implementation Dependencies

```json
{
  "dependencies": {
    "json-schema-to-zod": "^2.0.0",
    "json-refs": "^3.0.15",
    "ajv": "^8.12.0"
  }
}
```

## Conclusion

The **Runtime JSON Schema → Zod Conversion** approach provides the best balance of:
- External schema definition capability
- TypeScript type safety (at runtime)
- Backward compatibility with existing Config API
- Reasonable implementation complexity
- Performance through caching

This solution enables users to define configuration schemas externally while maintaining the power and familiarity of the QiCore Config system.