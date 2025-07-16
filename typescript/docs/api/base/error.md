# QiError API Reference

QiError provides structured error handling with categorization, context, and chaining capabilities.

## Type Definition

```typescript
interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
  readonly timestamp: string
  readonly cause?: QiError
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

type ErrorCode = string
type RetryStrategy = {
  shouldRetry: boolean
  maxRetries: number
  backoffMs: number
}
```

## Error Categories

### ErrorCategories

Object containing all available error categories.

```typescript
const ErrorCategories = {
  VALIDATION: 'VALIDATION',
  NETWORK: 'NETWORK',
  SYSTEM: 'SYSTEM',
  BUSINESS: 'BUSINESS',
  AUTHENTICATION: 'AUTHENTICATION',
  AUTHORIZATION: 'AUTHORIZATION',
  CONFIGURATION: 'CONFIGURATION',
  TIMEOUT: 'TIMEOUT',
  RESOURCE: 'RESOURCE',
  CONCURRENCY: 'CONCURRENCY'
} as const
```

### isErrorCategory(category: string): category is ErrorCategory

Type guard for error categories.

```typescript
if (isErrorCategory('VALIDATION')) {
  // TypeScript knows this is a valid category
}
```

## Error Creation

### createError(code: string, message: string, category?: ErrorCategory, context?: Record<string, unknown>): QiError

Creates a new QiError.

```typescript
const error = createError('USER_NOT_FOUND', 'User not found')
const errorWithCategory = createError('INVALID_EMAIL', 'Invalid email', 'VALIDATION')
const errorWithContext = createError(
  'API_ERROR',
  'API call failed',
  'NETWORK',
  { endpoint: '/api/users', statusCode: 500 }
)
```

### createErrorCode(domain: string, operation: string, reason: string): ErrorCode

Creates a structured error code.

```typescript
const code = createErrorCode('USER', 'FETCH', 'NOT_FOUND')
// Returns: 'USER_FETCH_NOT_FOUND'
```

## Error Builder

### QiErrorBuilder

Fluent API for building complex errors.

```typescript
class QiErrorBuilder {
  code(code: string): QiErrorBuilder
  message(message: string): QiErrorBuilder
  category(category: ErrorCategory): QiErrorBuilder
  context(context: Record<string, unknown>): QiErrorBuilder
  cause(cause: QiError): QiErrorBuilder
  build(): QiError
}
```

### errorBuilder(): QiErrorBuilder

Creates a new error builder instance.

```typescript
const error = errorBuilder()
  .code('DATABASE_ERROR')
  .message('Database connection failed')
  .category('SYSTEM')
  .context({ host: 'localhost', port: 5432 })
  .build()
```

## Convenience Functions

### validationError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a validation error.

```typescript
const error = validationError('INVALID_EMAIL', 'Email format is invalid')
```

### networkError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a network error.

```typescript
const error = networkError('CONNECTION_FAILED', 'Network connection failed')
```

### systemError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a system error.

```typescript
const error = systemError('OUT_OF_MEMORY', 'Insufficient memory')
```

### businessError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a business logic error.

```typescript
const error = businessError('INSUFFICIENT_FUNDS', 'Account balance too low')
```

### authenticationError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates an authentication error.

```typescript
const error = authenticationError('INVALID_TOKEN', 'Authentication token expired')
```

### authorizationError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates an authorization error.

```typescript
const error = authorizationError('ACCESS_DENIED', 'Insufficient permissions')
```

### configurationError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a configuration error.

```typescript
const error = configurationError('MISSING_CONFIG', 'Required configuration missing')
```

### timeoutError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a timeout error.

```typescript
const error = timeoutError('REQUEST_TIMEOUT', 'Request exceeded timeout')
```

### resourceError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a resource error.

```typescript
const error = resourceError('QUOTA_EXCEEDED', 'Resource quota exceeded')
```

### concurrencyError(code: string, message: string, context?: Record<string, unknown>): QiError

Creates a concurrency error.

```typescript
const error = concurrencyError('LOCK_FAILED', 'Failed to acquire lock')
```

## Error Utilities

### hasCategory(error: QiError, category: ErrorCategory): boolean

Checks if an error has a specific category.

```typescript
if (hasCategory(error, 'NETWORK')) {
  // Handle network errors
}
```

### getRetryStrategy(error: QiError): RetryStrategy

Gets the retry strategy for an error based on its category.

```typescript
const strategy = getRetryStrategy(error)
if (strategy.shouldRetry) {
  console.log(`Retrying up to ${strategy.maxRetries} times`)
}
```

**Default Strategies:**
- `NETWORK`: `{ shouldRetry: true, maxRetries: 3, backoffMs: 1000 }`
- `TIMEOUT`: `{ shouldRetry: true, maxRetries: 3, backoffMs: 1000 }`
- `SYSTEM`: `{ shouldRetry: true, maxRetries: 2, backoffMs: 2000 }`
- `VALIDATION`: `{ shouldRetry: false, maxRetries: 0, backoffMs: 0 }`
- `BUSINESS`: `{ shouldRetry: false, maxRetries: 0, backoffMs: 0 }`

## Error Chaining

### chainError(error: QiError, cause: QiError): QiError

Chains errors to show causality.

```typescript
const dbError = systemError('CONNECTION_FAILED', 'Database connection failed')
const serviceError = chainError(
  businessError('USER_FETCH_FAILED', 'Failed to fetch user'),
  dbError
)
```

### getRootCause(error: QiError): QiError

Gets the root cause of an error chain.

```typescript
const root = getRootCause(chainedError)
```

### formatErrorChain(error: QiError): string

Formats an error chain for display.

```typescript
const formatted = formatErrorChain(error)
console.log(formatted)
// "API_ERROR: API request failed
//  └─ USER_FETCH_FAILED: Failed to fetch user
//     └─ CONNECTION_FAILED: Database connection failed"
```

## Context Management

### withContext(error: QiError, context: Record<string, unknown>): QiError

Adds context to an existing error.

```typescript
const enhanced = withContext(error, {
  userId: 123,
  timestamp: new Date().toISOString(),
  requestId: 'req-abc123'
})
```

### getContext(error: QiError): Record<string, unknown> | undefined

Gets the context from an error.

```typescript
const context = getContext(error)
console.log(context?.userId)
```

## Aggregate Errors

### createAggregateError(code: string, message: string, errors: QiError[]): QiError

Creates an aggregate error containing multiple errors.

```typescript
const validationErrors = [
  validationError('MISSING_NAME', 'Name is required'),
  validationError('INVALID_EMAIL', 'Email format is invalid')
]

const aggregate = createAggregateError(
  'VALIDATION_FAILED',
  'Multiple validation errors',
  validationErrors
)

// Access individual errors
const individualErrors = aggregate.context?.errors as QiError[]
```

## Serialization

### serializeError(error: QiError): Record<string, unknown>

Serializes an error for storage or transmission.

```typescript
const serialized = serializeError(error)
const json = JSON.stringify(serialized)
```

### deserializeError(data: Record<string, unknown>): QiError

Deserializes an error from storage or transmission.

```typescript
const error = deserializeError(JSON.parse(json))
```

## Usage Examples

### Basic Error Creation

```typescript
import { createError, validationError, networkError } from '@qi/qicore-foundation/base'

// Simple error
const error1 = createError('USER_NOT_FOUND', 'User not found')

// Categorized error
const error2 = validationError('INVALID_EMAIL', 'Email format is invalid')

// Error with context
const error3 = networkError('API_FAILED', 'API call failed', {
  endpoint: '/api/users',
  statusCode: 500,
  responseTime: 1500
})
```

### Error Chaining

```typescript
import { chainError, systemError, businessError } from '@qi/qicore-foundation/base'

// Chain related errors
const dbError = systemError('CONNECTION_FAILED', 'Database connection failed')
const serviceError = chainError(
  businessError('USER_SAVE_FAILED', 'Failed to save user'),
  dbError
)
const apiError = chainError(
  networkError('API_ERROR', 'API request failed'),
  serviceError
)

// Access the chain
console.log(apiError.cause?.code) // 'USER_SAVE_FAILED'
console.log(apiError.cause?.cause?.code) // 'CONNECTION_FAILED'
```

### Builder Pattern

```typescript
import { errorBuilder } from '@qi/qicore-foundation/base'

const error = errorBuilder()
  .code('COMPLEX_ERROR')
  .message('A complex error occurred')
  .category('SYSTEM')
  .context({
    operation: 'processData',
    input: { type: 'user', id: 123 },
    metadata: { timestamp: Date.now() }
  })
  .cause(previousError)
  .build()
```

### Error Handling in Services

```typescript
import { 
  Result, Ok, Err, 
  hasCategory, getRetryStrategy, chainError 
} from '@qi/qicore-foundation/base'

class UserService {
  async getUser(id: number): Promise<Result<User, QiError>> {
    try {
      const user = await this.database.findUser(id)
      if (!user) {
        return Err(businessError('USER_NOT_FOUND', 'User not found'))
      }
      return Ok(user)
    } catch (error) {
      const dbError = systemError('DATABASE_ERROR', 'Database operation failed')
      return Err(chainError(
        businessError('USER_FETCH_FAILED', 'Failed to fetch user'),
        dbError
      ))
    }
  }

  async withRetry<T>(operation: () => Promise<Result<T, QiError>>): Promise<Result<T, QiError>> {
    let lastError: QiError | undefined
    
    for (let attempt = 0; attempt < 3; attempt++) {
      const result = await operation()
      
      if (result.tag === 'success') {
        return result
      }
      
      lastError = result.error
      const strategy = getRetryStrategy(result.error)
      
      if (!strategy.shouldRetry || attempt >= strategy.maxRetries - 1) {
        break
      }
      
      await new Promise(resolve => 
        setTimeout(resolve, strategy.backoffMs * (attempt + 1))
      )
    }
    
    return Err(lastError!)
  }
}
```

### Validation with Aggregate Errors

```typescript
import { createAggregateError, validationError } from '@qi/qicore-foundation/base'

function validateUser(data: any): Result<User, QiError> {
  const errors: QiError[] = []
  
  if (!data.name) {
    errors.push(validationError('MISSING_NAME', 'Name is required'))
  }
  
  if (!data.email) {
    errors.push(validationError('MISSING_EMAIL', 'Email is required'))
  } else if (!isValidEmail(data.email)) {
    errors.push(validationError('INVALID_EMAIL', 'Email format is invalid'))
  }
  
  if (!data.age || data.age < 0) {
    errors.push(validationError('INVALID_AGE', 'Age must be positive'))
  }
  
  if (errors.length > 0) {
    return Err(createAggregateError(
      'VALIDATION_FAILED',
      'User validation failed',
      errors
    ))
  }
  
  return Ok(data as User)
}
```

## Integration with Result<T>

QiError is designed to work seamlessly with Result<T>:

```typescript
import { Result, Ok, Err, map, flatMap } from '@qi/qicore-foundation/base'

// Functions return Result<T, QiError>
function fetchUser(id: number): Result<User, QiError> {
  if (id <= 0) {
    return Err(validationError('INVALID_ID', 'User ID must be positive'))
  }
  // ... fetch logic
}

// Chain operations with proper error handling
const result = flatMap(fetchUser(123), user =>
  map(validateUser(user), validatedUser => ({
    ...validatedUser,
    processed: true
  }))
)
```

## Best Practices

1. **Use appropriate categories**: Choose the right category for proper error handling
2. **Provide context**: Include relevant debugging information
3. **Chain related errors**: Link errors to show causality
4. **Use structured codes**: Create consistent error codes
5. **Leverage retry strategies**: Use built-in retry guidance
6. **Aggregate validation errors**: Collect all validation errors at once
7. **Preserve error chains**: Don't lose important error information

## Performance Considerations

- QiError objects are lightweight and optimized for serialization
- Context objects should be kept reasonably small
- Error chaining maintains references, not copies
- Serialization/deserialization is optimized for common use cases