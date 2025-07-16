# QiCore Foundation Tutorial

## Overview

QiCore Foundation provides mathematical foundation types and infrastructure services built on three core principles:

- **Law**: Mathematical properties that guarantee predictable behavior
- **Contract**: Behavioral specifications that define what operations do
- **Behavior**: Practical usage patterns that emerge from laws and contracts

## Part 1: Basic Usage

### Understanding Result&lt;T&gt; - Mathematical Error Handling

Result&lt;T&gt; replaces exceptions with explicit error handling based on mathematical laws.

**Key Concepts:**
- **Law**: Functor and Monad laws guarantee safe composition
- **Contract**: Operations never throw exceptions, always return Result&lt;T&gt;
- **Behavior**: Errors propagate automatically, success values transform predictably

**Working Example** (see `typescript/app/basic-result/`):

```typescript
import { Ok, Err, map, flatMap, match } from '@qi/base'

// Basic operations follow functor laws
const double = (x: number) => x * 2
const parseNumber = (s: string) => 
  isNaN(+s) ? Err('Not a number') : Ok(+s)

// Law: map(id) === id (identity)
const identity = <T>(x: T) => x
const result = Ok(42)
console.log(map(identity, result)) // Same as result

// Law: map(f ∘ g) === map(f) ∘ map(g) (composition)
const add1 = (x: number) => x + 1
const composed = map(double, map(add1, Ok(5)))
const direct = map(x => double(add1(x)), Ok(5))
console.log(composed, direct) // Equivalent results

// Contract: flatMap enables monadic composition
const divide = (a: number, b: number) => 
  b === 0 ? Err('Division by zero') : Ok(a / b)

const calculate = (input: string) => 
  flatMap(n => divide(n, 2), 
    flatMap(parseNumber, 
      map(s => s.trim(), Ok(input))))

// Behavior: Errors propagate, success values transform
match(
  value => console.log(`Result: ${value}`),
  error => console.log(`Error: ${error}`),
  calculate("  10  ")
)
```

### Understanding QiError - Structured Error Information

QiError provides structured error information with categorization for retry strategies.

**Key Concepts:**
- **Law**: Error chaining preserves causal relationships
- **Contract**: Every error has code, message, category, and context
- **Behavior**: Categories determine retry strategies

**Working Example** (see `typescript/app/error-handling/`):

```typescript
import { createError, ErrorCategory, withContext, withCause } from '@qi/base'

// Contract: Every error has required fields
const validationError = createError({
  code: 'VALIDATION_ERROR',
  message: 'Invalid input data',
  category: ErrorCategory.VALIDATION,
  context: { field: 'email', value: 'invalid-email' }
})

// Law: Context accumulation is associative
const enhanced = withContext(
  { userId: '123', operation: 'signup' },
  validationError
)

// Behavior: Categories determine retry strategies
const retryStrategy = (error: QiError) => {
  switch (error.category) {
    case ErrorCategory.VALIDATION:
    case ErrorCategory.BUSINESS:
      return 'never'
    case ErrorCategory.NETWORK:
      return 'exponential_backoff'
    default:
      return 'linear_backoff'
  }
}

console.log(retryStrategy(validationError)) // 'never'
```

### Understanding Configuration - External Schema Validation

Configuration uses fluent API with external schema validation for type safety.

**Key Concepts:**
- **Law**: Configuration forms a monoid with associative merge
- **Contract**: External schemas provide validation without source changes
- **Behavior**: Fluent API enables readable configuration pipelines

**Working Example** (see `typescript/app/config-example/`):

```typescript
// External schema file: config.schema.json
// Users can modify schema without touching source code

import { ConfigBuilder } from '@qi/core'
import { jsonSchemaToZod } from 'json-schema-to-zod'

const loadConfigWithExternalSchema = async () => {
  // Load external JSON schema and convert to Zod
  const jsonSchema = JSON.parse(readFileSync('config.schema.json', 'utf-8'))
  const zodSchema = jsonSchemaToZod(jsonSchema, { module: 'esm' })
  
  // Law: Merge is associative (order doesn't matter for same keys)
  const config = await ConfigBuilder
    .fromYamlFile('base.yaml')
    .then(result => result.value
      .merge(ConfigBuilder.fromYamlFile('prod.yaml'))
      .merge(ConfigBuilder.fromEnv('APP'))
      .validateWith(zodSchema)
      .build()
    )
  
  // Behavior: Type-safe access to validated configuration
  const appName = config.get('app.name')
  const port = config.get('app.port')
  
  return { appName, port }
}
```

## Part 2: Advanced Usage

### Mathematical Law Verification

QiCore Foundation includes property-based testing to verify mathematical laws.

**Key Concepts:**
- **Law**: Functor, Monad, and Applicative laws must hold
- **Contract**: Property-based tests verify laws with 1000+ test cases
- **Behavior**: Laws ensure predictable composition

**Working Example** (see `typescript/lib/tests/properties/`):

```typescript
import { test } from '@fast-check/vitest'
import { Ok, Err, map, flatMap } from '@qi/base'

// Functor Law: Identity
test.prop([fc.integer()])(
  'map(identity) === identity',
  (value) => {
    const result = Ok(value)
    const identity = <T>(x: T) => x
    expect(map(identity, result)).toEqual(result)
  }
)

// Monad Law: Left Identity
test.prop([fc.integer()])(
  'flatMap(f)(Ok(x)) === f(x)',
  (value) => {
    const f = (x: number) => Ok(x * 2)
    expect(flatMap(f, Ok(value))).toEqual(f(value))
  }
)

// These laws guarantee safe composition in your application code
```

### Domain-Specific Error Types

Extend QiError for domain-specific error handling with type safety.

**Key Concepts:**
- **Law**: Error chaining preserves causal relationships
- **Contract**: Domain errors extend base QiError structure
- **Behavior**: Type-safe error handling with specific context

```typescript
import { createError, withCause, type QiError } from '@qi/base'

// Contract: Domain-specific error types
type UserError = QiError & {
  category: 'VALIDATION' | 'BUSINESS'
  context: { userId?: string; field?: string }
}

type PaymentError = QiError & {
  category: 'NETWORK' | 'BUSINESS'
  context: { paymentId: string; amount: number }
}

// Law: Error chaining preserves causal relationships
const createPaymentError = (message: string, cause?: QiError): PaymentError => {
  const error = createError({
    code: 'PAYMENT_ERROR',
    message,
    category: 'NETWORK',
    context: { paymentId: 'pay_123', amount: 100 }
  }) as PaymentError
  
  return cause ? withCause(cause, error) : error
}

// Behavior: Type-safe error handling with domain context
const handlePaymentError = (error: PaymentError) => {
  console.log(`Payment ${error.context.paymentId} failed`)
  console.log(`Amount: ${error.context.amount}`)
  
  // Access to full error chain
  const rootCause = error.getRootCause()
  console.log(`Root cause: ${rootCause.message}`)
}
```

### Advanced Configuration Patterns

Multi-source configuration with transformation and validation.

**Key Concepts:**
- **Law**: Configuration merge is associative and has identity
- **Contract**: External schemas enable validation without code changes
- **Behavior**: Fluent API supports complex configuration pipelines

```typescript
import { ConfigBuilder } from '@qi/core'
import { z } from 'zod'

// Complex schema with nested validation
const ProductionSchema = z.object({
  app: z.object({
    name: z.string(),
    port: z.number().min(1000).max(65535),
    environment: z.enum(['development', 'production', 'test'])
  }),
  database: z.object({
    host: z.string(),
    port: z.number(),
    ssl: z.boolean().default(true)
  }),
  features: z.object({
    monitoring: z.boolean().default(true),
    caching: z.boolean().default(true)
  })
})

// Advanced pipeline: load → merge → transform → validate
const loadProductionConfig = async () => {
  const baseConfig = await ConfigBuilder.fromYamlFile('base.yaml')
  const envConfig = await ConfigBuilder.fromYamlFile('production.yaml')
  const secrets = await ConfigBuilder.fromJsonFile('/secrets/app.json')
  
  return baseConfig.value
    .merge(envConfig.value)
    .merge(secrets.value)
    .merge(ConfigBuilder.fromEnv('APP_'))
    .transform(data => ({
      ...data,
      computed: {
        isProduction: data.app?.environment === 'production',
        databaseUrl: `postgresql://${data.database?.host}:${data.database?.port}/app`
      }
    }))
    .validateWith(ProductionSchema)
    .build()
}
```

### Cross-Language Behavioral Consistency

QiCore Foundation maintains identical behavior across TypeScript, Haskell, and other implementations.

**Key Concepts:**
- **Law**: Same mathematical laws verified across all languages
- **Contract**: Identical behavioral specifications
- **Behavior**: Consistent error handling and operation results

```typescript
// TypeScript Result operations
const result = flatMap(
  x => Ok(x * 2),
  map(x => x + 1, Ok(5))
)

// Equivalent Haskell: Ok 5 >>= \x -> return (x + 1) >>= \y -> return (y * 2)
// Equivalent behavior guaranteed by mathematical laws
```

## Summary

### Basic Usage - Immediate Value
- **Result&lt;T&gt;**: Replace exceptions with mathematical error handling
- **QiError**: Structured errors with retry strategy categorization
- **Configuration**: External schema validation without source changes

### Advanced Usage - Power and Flexibility
- **Mathematical Laws**: Property-based testing ensures predictable behavior
- **Domain Extensions**: Type-safe error handling for specific domains
- **Complex Pipelines**: Multi-source configuration with transformation
- **Cross-Language**: Consistent behavior across implementations

The library provides simple patterns for immediate use while offering mathematical guarantees for complex applications.

---

**Next Steps:**
1. Explore working examples in `typescript/app/`
2. Run tests to see mathematical laws in action
3. Experiment with your own domain-specific error types
4. Create configuration schemas for your applications