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

### 5. Cache Example (`cache-example/`)
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

Each example directory contains:
- `README.md` - Detailed explanation and usage
- `package.json` - Dependencies and scripts
- `src/index.ts` - Runnable demonstration

### Prerequisites

- Node.js 18+ or Bun 1.0+
- TypeScript 5.8+
- Docker (for Redis examples)

### Setup

```bash
# Navigate to any example directory
cd basic-result

# Run the example
bun run dev

# Or with specific scenarios
bun run dev -- map
bun run dev -- flatmap
bun run dev -- pattern
```

### Cache Example with Redis

```bash
# Start Redis first
docker run -p 6379:6379 redis:alpine

# Run cache example
cd cache-example
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

## Testing Examples

Each example includes:
- Working demonstrations that you can run
- Error scenario testing
- Real integration patterns
- No fake or stub implementations

## Learning Path

1. **Start with `basic-result/`** - Learn fundamental Result<T> patterns
2. **Explore `error-extension/`** - Understand domain-specific error handling
3. **Check `config-example/`** - See configuration + logging integration
4. **Try `cache-example/`** - Learn caching strategies
5. **Review `error-handling/`** - Advanced error management patterns

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