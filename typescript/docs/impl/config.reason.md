## Updated Configuration Implementation

**Decision**: Fluent API for configuration building and transformation

```typescript
export interface ConfigData {
  readonly [key: string]: unknown
}

export class ConfigBuilder {
  private constructor(private readonly data: ConfigData) {}

  // Factory methods return builders
  static fromObject(obj: object): ConfigBuilder {
    try {
      const frozen = deepFreeze(structuredClone(obj))
      return new ConfigBuilder(frozen as ConfigData)
    } catch (error) {
      throw new Error(`Invalid object: ${error}`)
    }
  }

  static fromFile(filePath: string): ConfigBuilder {
    try {
      const content = readFileSync(filePath, 'utf-8')
      const data = parseConfigFile(filePath, content)
      return ConfigBuilder.fromObject(data)
    } catch (error) {
      throw new Error(`Failed to load config from ${filePath}: ${error}`)
    }
  }

  static fromEnvironment(prefix?: string): ConfigBuilder {
    const config: Record<string, unknown> = {}
    for (const [key, value] of Object.entries(process.env)) {
      if (!prefix || key.startsWith(prefix)) {
        const configKey = envKeyToConfigKey(key, prefix)
        setNestedValue(config, configKey, parseEnvValue(value))
      }
    }
    return new ConfigBuilder(deepFreeze(config))
  }

  static empty(): ConfigBuilder {
    return new ConfigBuilder({})
  }

  // Fluent operations
  merge(...others: ConfigBuilder[]): ConfigBuilder {
    const configs = [this.data, ...others.map(b => b.data)]
    const merged = configs.reduce((acc, config) => deepMerge(acc, config), {})
    return new ConfigBuilder(deepFreeze(merged))
  }

  validate(schema: Schema): ConfigBuilder {
    const result = validateSchema(schema, this.data)
    if (!result.valid) {
      throw new Error(`Validation failed: ${result.errors.join(', ')}`)
    }
    return this
  }

  get<T = unknown>(key: string): T | undefined {
    return getNestedValue(key, this.data) as T
  }

  getOrThrow<T = unknown>(key: string): T {
    const value = this.get<T>(key)
    if (value === undefined) {
      throw new Error(`Required config key not found: ${key}`)
    }
    return value
  }

  getWithDefault<T>(key: string, defaultValue: T): T {
    return this.get<T>(key) ?? defaultValue
  }

  has(key: string): boolean {
    return this.get(key) !== undefined
  }

  // Transform to Result for error handling
  toResult(): Result<ConfigData> {
    return success(this.data)
  }

  // Get raw data
  build(): ConfigData {
    return this.data
  }
}

// Usage examples showing the procedural nature
const config = ConfigBuilder
  .fromFile('./config.json')
  .merge(
    ConfigBuilder.fromEnvironment('APP_'),
    ConfigBuilder.fromObject({ defaults: { timeout: 5000 } })
  )
  .validate(appConfigSchema)
  .build()

// Or with error handling
const configResult = pipe(
  tryCatch(() => 
    ConfigBuilder
      .fromFile('./config.json')
      .merge(ConfigBuilder.fromEnvironment())
      .validate(schema)
      .toResult()
  ),
  flatMap(x => x) // Flatten Result<Result<T>> to Result<T>
)
```

### Why This Works Better for Configuration

1. **Natural flow for procedural operations:**
   ```typescript
   // Reads like a recipe
   const config = ConfigBuilder
     .fromFile('./base.json')
     .merge(ConfigBuilder.fromEnvironment())
     .validate(schema)
     .build()
   ```

2. **Better error messages with stack traces:**
   ```typescript
   // Throws at the exact operation that failed
   ConfigBuilder
     .fromFile('./missing.json') // Error: Failed to load config from ./missing.json
     .validate(schema)            // Or Error: Validation failed: missing required field 'database.host'
   ```

3. **Easier to use in practice:**
   ```typescript
   // Getting values is straightforward
   const dbHost = config.getOrThrow<string>('database.host')
   const port = config.getWithDefault('server.port', 3000)
   ```

4. **Still maintains immutability:**
   ```typescript
   const base = ConfigBuilder.fromFile('./base.json')
   const withEnv = base.merge(ConfigBuilder.fromEnvironment()) // base is unchanged
   ```

5. **Interops with Result when needed:**
   ```typescript
   // For operations that need Result<T> error handling
   const result = tryCatch(() => 
     ConfigBuilder.fromFile('./config.json').toResult()
   ).flatMap(x => x)
   ```

This approach gives us:
- **Procedural ergonomics** for the naturally procedural config operations
- **Throw-based errors** which are more natural for configuration (you want to fail fast)
- **Fluent chaining** that reads like a series of steps
- **Escape hatch to Result<T>** when needed for error handling
- **Better stack traces** for debugging configuration issues

The key insight is that configuration is different from Result<T> - it's not about mathematical transformations but about procedural setup that should fail fast and clearly.