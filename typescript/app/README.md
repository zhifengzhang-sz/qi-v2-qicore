# QiCore Foundation Example Applications

This directory contains comprehensive example applications demonstrating how to use QiCore Foundation in real-world scenarios.

## Examples Overview

### 1. Basic Result Usage (`basic-result/`)
**Most important for qi/base learning**
- Creating and handling Results with Ok/Err
- Basic transformations (map, flatMap, match)
- Pattern matching and safe value extraction
- Real-world user profile example

### 2. Error Extension (`error-extension/`)
**Domain-specific error handling patterns**
- Creating custom error types (UserError, PaymentError, OrderError)
- Error factory functions with type safety
- Error handling strategies based on categories
- Error composition across different domains
- Retry logic based on error types

### 3. Error Handling (`error-handling/`)
**Comprehensive error management**
- Structured error creation with QiError
- Error chaining and context accumulation
- Validation patterns and aggregate errors
- Retry strategies for different error categories

### 4. Configuration Example (`config-example/`)
**Config + Logger integration (project-specific)**
- Multi-source configuration (YAML, environment, defaults)
- External schema validation with JSON Schema
- Logger integration with structured logging
- ValidatedConfig API for type-safe access
- Error handling for configuration failures

### 5. Async Composition (`async-composition/`)
**Advanced async patterns**
- Async Result composition with functional helpers
- Collection operations (sequential, parallel processing)
- Pipeline patterns for batch processing
- Error handling strategies in async contexts
- Performance monitoring and graceful degradation

### 6. Cache Example (`cache-example/`)
**Caching strategies (project-specific)**
- Memory cache with TTL and LRU eviction
- Redis cache integration with real connectivity
- Cache-aside pattern for expensive operations
- Multi-get operations and key pattern matching
- Performance monitoring with cache statistics

## Usage Priority (matches real-world usage)

1. **qi/base** (most used) → `basic-result/` and `error-extension/`
2. **logger** (second most) → integrated in `config-example/`
3. **config** (project-specific) → `config-example/`
4. **cache** (project-specific) → `cache-example/`

## Running the Examples

### Prerequisites

1. **Install Bun or Node.js**:
   - Bun 1.0+ (recommended) or Node.js 18+
   - TypeScript 5.8+

2. **Start Redis** (required for cache and some integration tests):
   ```bash
   # Option 1: Use QiCore services (recommended)
   cd ../../services
   docker compose up -d redis
   
   # Option 2: Start Redis directly
   docker run -p 6379:6379 redis:alpine
   
   # Verify Redis is running
   docker ps | grep redis
   ```

3. **Build QiCore packages**:
   ```bash
   # From the typescript directory
   cd /path/to/qi-v2-qicore/typescript
   bun run build
   ```

### Running Examples

All examples can be run from the **typescript directory**:

```bash
# From /path/to/qi-v2-qicore/typescript

# Basic Result patterns
bun app/basic-result/src/index.ts

# Configuration management  
bun app/config-example/src/index.ts

# Caching strategies
bun app/cache-example/src/index.ts

# Async composition patterns
bun app/async-composition/src/index.ts

# Error handling strategies
bun app/error-handling/src/index.ts

# Domain-specific errors
bun app/error-extension/src/index.ts
```

### Individual Example Usage

Each example can also be run from its own directory:

```bash
# Navigate to any example
cd app/basic-result

# Run with bun
bun run src/index.ts

# Or use package.json scripts if available
bun run dev
```

## Key Learning Points

### Result<T> Patterns (qi/base)
- Functional error handling without exceptions
- Composable operations with map/flatMap
- Safe value extraction and pattern matching
- Type-safe error handling with QiError

### Error Management
- Structured errors with categories and context
- Domain-specific error types for better debugging
- Error categorization determines retry strategies
- Context preservation and error chaining

### Infrastructure Integration
- Configuration management with validation
- Structured logging with context accumulation
- Caching strategies (memory and Redis)
- Real package integration (not fake implementations)

## Example Patterns Demonstrated

### 1. Basic Result Pattern
```typescript
// From basic-result/
function divide(a: number, b: number): Result<number, QiError> {
  if (b === 0) {
    return Err(validationError('Division by zero'))
  }
  return Ok(a / b)
}
```

### 2. Error Extension Pattern
```typescript
// From error-extension/
interface UserError extends QiError {
  category: 'VALIDATION' | 'AUTHENTICATION' | 'AUTHORIZATION'
  context: {
    userId?: string
    field?: string
    operation?: string
  }
}
```

### 3. Configuration Pattern
```typescript
// From config-example/
const config = ConfigBuilder
  .fromYamlFile('./config.yaml')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .validateWithSchemaFile('./schema.json')
  .buildValidated()
```

### 4. Cache Pattern
```typescript
// From cache-example/
async function getUserWithCache(id: string) {
  const cached = await cache.get(`user:${id}`)
  if (cached.tag === 'success') return cached.value
  
  const user = await fetchUser(id)
  await cache.set(`user:${id}`, user, 60)
  return user
}
```

## Validation

Test that everything works:

```bash
# From typescript directory
bun run check  # Runs typecheck + format + lint + tests

# Test all examples work
bun app/basic-result/src/index.ts
bun app/config-example/src/index.ts  
bun app/cache-example/src/index.ts
bun app/async-composition/src/index.ts
bun app/error-handling/src/index.ts
bun app/error-extension/src/index.ts
```

## Learning Path

1. **Start with `basic-result/`** - Learn fundamental Result<T> patterns
2. **Explore `error-handling/`** - Comprehensive error management strategies  
3. **Try `error-extension/`** - Domain-specific error handling patterns
4. **Study `async-composition/`** - Advanced async Result patterns
5. **Check `config-example/`** - Configuration + logging integration
6. **Try `cache-example/`** - Caching strategies and Redis integration

## Why These Examples Work

- **Real implementations**: All code uses actual v-0.3.6 API
- **Production patterns**: Patterns you'll actually use in projects
- **Tested code**: All examples run successfully
- **Progressive complexity**: Build understanding step by step
- **Error-first approach**: Shows problems before solutions

## Next Steps

After working through these examples:
1. Apply patterns to your own projects
2. Start with one Result<T> function and expand
3. Use error categories for retry strategies
4. Integrate configuration and logging as needed
5. Add caching where appropriate

All examples demonstrate the core principle: **qi/base gets used most, followed by logger, with config and cache serving specific project needs**.

The "easier to use, the better" principle is evident in the fluent APIs and clear error messages throughout.