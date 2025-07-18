# QiError API Reference

QiError provides structured error handling with categorization and context for consistent error management across the QiCore Foundation.

## Type Definition

```typescript
interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Record<string, unknown>
}

type ErrorCategory = 
  | 'VALIDATION'
  | 'NETWORK'
  | 'SYSTEM'
  | 'BUSINESS'
  | 'AUTHENTICATION'
  | 'AUTHORIZATION'
  | 'CONFIGURATION'
  | 'TIMEOUT'
  | 'RESOURCE'
  | 'CONCURRENCY'
  | 'LOGGER'

interface RetryStrategy {
  readonly strategy: 'never' | 'exponential_backoff' | 'linear_backoff'
  readonly description: string
}

interface ErrorOptions {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
}
```

## Error Categories

### ErrorCategories

Array containing all available error categories.

```typescript
const ErrorCategories: ReadonlyArray<ErrorCategory> = [
  'VALIDATION',
  'NETWORK', 
  'SYSTEM',
  'BUSINESS',
  'AUTHENTICATION',
  'AUTHORIZATION',
  'CONFIGURATION',
  'TIMEOUT',
  'RESOURCE',
  'CONCURRENCY',
  'LOGGER'
] as const
```

### isErrorCategory(category: string): category is ErrorCategory

Type guard for error categories.

```typescript
if (isErrorCategory('VALIDATION')) {
  // TypeScript knows this is a valid category
}
```

## Error Creation

### createError(options: ErrorOptions): QiError

Creates a new QiError from options object.

```typescript
const error = createError({
  code: 'USER_NOT_FOUND',
  message: 'User not found',
  category: 'BUSINESS'
})

const errorWithContext = createError({
  code: 'API_ERROR',
  message: 'API call failed',
  category: 'NETWORK',
  context: { endpoint: '/api/users', statusCode: 500 }
})
```

## Factory Functions

### create(code: string, message: string, category: ErrorCategory, context?: Record<string, unknown>): QiError

Creates a QiError with full control over all properties.

```typescript
const error = create(
  'DATABASE_ERROR',
  'Database connection failed', 
  'SYSTEM',
  { host: 'localhost', port: 5432 }
)
```

## Utility Functions

### fromException(exception: unknown, category?: ErrorCategory): QiError

Creates a QiError from a JavaScript exception.

```typescript
try {
  JSON.parse(invalidJson)
} catch (error) {
  const qiError = fromException(error, 'VALIDATION')
}
```

### fromString(message: string, category?: ErrorCategory): QiError

Creates a QiError from a simple string message.

```typescript
const error = fromString('Something went wrong', 'SYSTEM')
```

### errorToString(error: QiError): string

Converts a QiError to a human-readable string.

```typescript
const errorStr = errorToString(error)
// Returns: '[USER_NOT_FOUND] User not found'
```

### getCategory(error: QiError): ErrorCategory

Gets the category of an error.

```typescript
const category = getCategory(error) // Returns: 'VALIDATION'
```

### toStructuredData(error: QiError): Record<string, unknown>

Converts a QiError to a serializable structure.

```typescript
const data = toStructuredData(error)
// Returns: { code: 'USER_NOT_FOUND', message: '...', category: 'BUSINESS', context: {...} }
```

## Error Utilities

### getRetryStrategy(category: ErrorCategory): RetryStrategy

Gets the retry strategy for an error category.

```typescript
const strategy = getRetryStrategy('NETWORK')
console.log(`Strategy: ${strategy.strategy}, Description: ${strategy.description}`)
```

**Default Strategies:**
- `NETWORK`: `{ strategy: 'exponential_backoff', description: 'Network communication failures' }`
- `TIMEOUT`: `{ strategy: 'exponential_backoff', description: 'Timeout errors' }`
- `SYSTEM`: `{ strategy: 'linear_backoff', description: 'System resource and infrastructure problems' }`
- `RESOURCE`: `{ strategy: 'linear_backoff', description: 'Resource exhaustion/unavailable' }`
- `CONCURRENCY`: `{ strategy: 'linear_backoff', description: 'Concurrency conflicts' }`
- `VALIDATION`: `{ strategy: 'never', description: 'Input validation and constraint violations' }`
- `BUSINESS`: `{ strategy: 'never', description: 'Business logic and domain rule violations' }`
- `AUTHENTICATION`: `{ strategy: 'never', description: 'Authentication failures' }`
- `AUTHORIZATION`: `{ strategy: 'never', description: 'Authorization/permission failures' }`
- `CONFIGURATION`: `{ strategy: 'never', description: 'Configuration/setup errors' }`
- `LOGGER`: `{ strategy: 'never', description: 'Logger-related errors' }`

## Context Management

### withContext(context: Record<string, unknown>, error: QiError): QiError

Adds context to an existing error.

```typescript
const enhanced = withContext({
  userId: 123,
  timestamp: new Date().toISOString(),
  requestId: 'req-abc123'
}, error)
```

## Convenience Functions

### validationError(message: string, context: Record<string, unknown> = {}): QiError

Creates a validation error.

```typescript
const error = validationError('Email format is invalid', { field: 'email' })
```

### networkError(message: string, context: Record<string, unknown> = {}): QiError

Creates a network error.

```typescript
const error = networkError('Network connection failed', { host: 'api.example.com' })
```

### systemError(message: string, context: Record<string, unknown> = {}): QiError

Creates a system error.

```typescript
const error = systemError('Insufficient memory', { available: '1GB', required: '2GB' })
```

### businessError(message: string, context: Record<string, unknown> = {}): QiError

Creates a business logic error.

```typescript
const error = businessError('Account balance too low', { balance: 100, required: 250 })
```

### authenticationError(message: string, context: Record<string, unknown> = {}): QiError

Creates an authentication error.

```typescript
const error = authenticationError('Authentication token expired')
```

### authorizationError(message: string, context: Record<string, unknown> = {}): QiError

Creates an authorization error.

```typescript
const error = authorizationError('Insufficient permissions')
```

### configurationError(message: string, context: Record<string, unknown> = {}): QiError

Creates a configuration error.

```typescript
const error = configurationError('Required configuration missing')
```

### timeoutError(message: string, context: Record<string, unknown> = {}): QiError

Creates a timeout error.

```typescript
const error = timeoutError('Request exceeded timeout')
```

### resourceError(message: string, context: Record<string, unknown> = {}): QiError

Creates a resource error.

```typescript
const error = resourceError('Resource quota exceeded')
```

### concurrencyError(message: string, context: Record<string, unknown> = {}): QiError

Creates a concurrency error.

```typescript
const error = concurrencyError('Concurrent modification detected')
```

### loggerError(message: string, context: Record<string, unknown> = {}): QiError

Creates a logger error.

```typescript
const error = loggerError('Failed to write log file')
```

## Usage Examples

### Basic Error Creation

```typescript
import { create, createError, validationError, networkError } from '@qi/qicore-foundation/base'

// Using create function
const error1 = create('USER_NOT_FOUND', 'User not found', 'BUSINESS')

// Using createError with options
const error2 = createError({
  code: 'INVALID_EMAIL',
  message: 'Email format is invalid',
  category: 'VALIDATION'
})

// Using convenience functions
const error3 = networkError('API call failed', {
  endpoint: '/api/users',
  statusCode: 500,
  responseTime: 1500
})
```

### Working with Error Context

```typescript
import { validationError, withContext, toStructuredData } from '@qi/qicore-foundation/base'

// Create error with initial context
const error = validationError('Invalid input', { field: 'email' })

// Add more context
const enhanced = withContext({
  userId: 123,
  requestId: 'req-abc123'
}, error)

// Convert to serializable format
const data = toStructuredData(enhanced)
console.log(JSON.stringify(data, null, 2))
```

### Retry Strategy Usage

```typescript
import { getRetryStrategy, networkError } from '@qi/qicore-foundation/base'

const error = networkError('Connection failed')
const strategy = getRetryStrategy(error.category)

if (strategy.strategy !== 'never') {
  console.log(`Should retry with strategy: ${strategy.strategy}`)
  console.log(`Description: ${strategy.description}`)
}
```

### Integration with Result<T>

```typescript
import { Result, success, failure, match, validationError } from '@qi/qicore-foundation/base'

function validateEmail(email: string): Result<string, QiError> {
  if (!email.includes('@')) {
    return failure(validationError('Invalid email format', { email }))
  }
  return success(email)
}

const result = validateEmail('invalid-email')
match(
  (email) => console.log('Valid email:', email),
  (error) => console.error('Validation failed:', errorToString(error)),
  result
)
```

## Best Practices

1. **Use appropriate categories**: Choose the most specific error category
2. **Provide meaningful context**: Include relevant debugging information
3. **Use convenience functions**: They set appropriate codes and categories
4. **Preserve error information**: Use withContext to add without losing data
5. **Follow retry strategies**: Use getRetryStrategy for consistent retry behavior
6. **Structure error codes**: Use consistent naming conventions
7. **Avoid sensitive data**: Don't include passwords or tokens in context

## Performance Considerations

- QiError objects are lightweight plain objects
- Context objects should be kept reasonably small
- toStructuredData is optimized for serialization
- Error creation is fast and allocation-efficient