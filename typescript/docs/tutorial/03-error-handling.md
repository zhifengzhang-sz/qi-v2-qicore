# Error Handling with QiError

QiError provides a structured, composable approach to error handling that integrates seamlessly with Result<T> operations. It offers rich context, error categorization, and retry strategies.

## What is QiError?

QiError is a structured error type that provides:
- **Categorized errors**: Predefined categories for different error types
- **Rich context**: Structured metadata for debugging and monitoring
- **Error chaining**: Link related errors to show causality
- **Retry strategies**: Built-in guidance for error recovery
- **Serialization**: Safe serialization for logging and API responses

```typescript
interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
  readonly timestamp: string
  readonly cause?: QiError
}
```

## Error Categories

QiError uses predefined categories that indicate the nature of the error and suggest appropriate handling strategies:

```typescript
import { ErrorCategories } from '@qi/qicore-foundation/base'

// Available categories
const categories = {
  VALIDATION: 'VALIDATION',           // Input validation errors
  NETWORK: 'NETWORK',                 // Network-related errors
  SYSTEM: 'SYSTEM',                   // System/infrastructure errors
  BUSINESS: 'BUSINESS',               // Business logic violations
  AUTHENTICATION: 'AUTHENTICATION',   // Authentication failures
  AUTHORIZATION: 'AUTHORIZATION',     // Authorization/permission errors
  CONFIGURATION: 'CONFIGURATION',     // Configuration errors
  TIMEOUT: 'TIMEOUT',                 // Timeout errors
  RESOURCE: 'RESOURCE',               // Resource exhaustion/unavailability
  CONCURRENCY: 'CONCURRENCY'          // Concurrency/locking errors
}
```

## Creating QiErrors

### Basic Error Creation

```typescript
import { createError } from '@qi/qicore-foundation/base'

// Simple error
const error = createError('USER_NOT_FOUND', 'User not found')

// Error with category
const validationError = createError(
  'INVALID_EMAIL',
  'Email format is invalid',
  'VALIDATION'
)

// Error with context
const networkError = createError(
  'API_REQUEST_FAILED',
  'Failed to fetch user data',
  'NETWORK',
  { 
    endpoint: '/api/users/123',
    statusCode: 500,
    retryCount: 3
  }
)
```

### Using the Error Builder

```typescript
import { QiErrorBuilder, errorBuilder } from '@qi/qicore-foundation/base'

// Fluent API for complex errors
const error = errorBuilder()
  .code('DATABASE_CONNECTION_FAILED')
  .message('Unable to connect to database')
  .category('SYSTEM')
  .context({ 
    host: 'localhost',
    port: 5432,
    database: 'myapp'
  })
  .build()

// Or using the class directly
const builderError = new QiErrorBuilder()
  .code('VALIDATION_FAILED')
  .message('User input validation failed')
  .category('VALIDATION')
  .context({ 
    field: 'email',
    value: 'invalid-email',
    constraints: ['email format', 'required']
  })
  .build()
```

### Convenience Functions

```typescript
import { 
  validationError,
  networkError,
  systemError,
  businessError,
  authenticationError,
  authorizationError,
  configurationError,
  timeoutError,
  resourceError,
  concurrencyError
} from '@qi/qicore-foundation/base'

// Predefined error creators
const validation = validationError('INVALID_INPUT', 'Input validation failed')
const network = networkError('CONNECTION_FAILED', 'Network connection failed')
const system = systemError('DATABASE_ERROR', 'Database operation failed')
const business = businessError('INSUFFICIENT_FUNDS', 'Account balance too low')
const auth = authenticationError('INVALID_TOKEN', 'Authentication token invalid')
const authz = authorizationError('ACCESS_DENIED', 'Insufficient permissions')
const config = configurationError('MISSING_CONFIG', 'Required configuration missing')
const timeout = timeoutError('REQUEST_TIMEOUT', 'Request exceeded timeout limit')
const resource = resourceError('QUOTA_EXCEEDED', 'Resource quota exceeded')
const concurrency = concurrencyError('LOCK_FAILED', 'Failed to acquire lock')
```

## Error Chaining

Link related errors to show causality:

```typescript
import { createError, chainError } from '@qi/qicore-foundation/base'

// Original error
const dbError = systemError('CONNECTION_FAILED', 'Database connection failed')

// Chain a higher-level error
const serviceError = chainError(
  businessError('USER_FETCH_FAILED', 'Failed to fetch user'),
  dbError
)

// Chain multiple levels
const apiError = chainError(
  networkError('API_ERROR', 'API request failed'),
  serviceError
)

console.log(apiError.cause?.code) // 'USER_FETCH_FAILED'
console.log(apiError.cause?.cause?.code) // 'CONNECTION_FAILED'
```

## Error Context

Add structured context for debugging and monitoring:

```typescript
import { createError, withContext } from '@qi/qicore-foundation/base'

// Add context to existing error
const baseError = validationError('INVALID_INPUT', 'Invalid user input')

const contextualError = withContext(baseError, {
  userId: 123,
  action: 'updateProfile',
  timestamp: new Date().toISOString(),
  requestId: 'req-abc123',
  metadata: {
    userAgent: 'Mozilla/5.0...',
    ipAddress: '192.168.1.1'
  }
})

// Access context
import { getContext } from '@qi/qicore-foundation/base'

const context = getContext(contextualError)
console.log(context?.userId) // 123
```

## Retry Strategies

QiError provides built-in retry strategy recommendations:

```typescript
import { getRetryStrategy } from '@qi/qicore-foundation/base'

const networkError = networkError('TIMEOUT', 'Request timed out')
const validationError = validationError('INVALID_INPUT', 'Bad input')

const networkStrategy = getRetryStrategy(networkError)
// { shouldRetry: true, maxRetries: 3, backoffMs: 1000 }

const validationStrategy = getRetryStrategy(validationError)
// { shouldRetry: false, maxRetries: 0, backoffMs: 0 }
```

## Error Utilities

### Checking Error Categories

```typescript
import { hasCategory } from '@qi/qicore-foundation/base'

const error = networkError('API_FAILED', 'API call failed')

if (hasCategory(error, 'NETWORK')) {
  console.log('Network error detected')
}
```

### Formatting Error Chains

```typescript
import { formatErrorChain } from '@qi/qicore-foundation/base'

const chainedError = chainError(
  apiError,
  chainError(serviceError, dbError)
)

const formatted = formatErrorChain(chainedError)
console.log(formatted)
// "API_ERROR: API request failed
//  └─ USER_FETCH_FAILED: Failed to fetch user
//     └─ CONNECTION_FAILED: Database connection failed"
```

### Finding Root Causes

```typescript
import { getRootCause } from '@qi/qicore-foundation/base'

const rootError = getRootCause(chainedError)
console.log(rootError.code) // 'CONNECTION_FAILED'
```

## Serialization

QiError supports safe serialization for logging and API responses:

```typescript
import { serializeError, deserializeError } from '@qi/qicore-foundation/base'

const error = createError('TEST_ERROR', 'Test error', 'SYSTEM', {
  data: { key: 'value' }
})

// Serialize for storage/transmission
const serialized = serializeError(error)
console.log(JSON.stringify(serialized, null, 2))

// Deserialize back to QiError
const deserialized = deserializeError(serialized)
console.log(deserialized.code) // 'TEST_ERROR'
```

## Using QiError with Result<T>

### Creating Result<T> with QiError

```typescript
import { Ok, Err, type Result } from '@qi/qicore-foundation/base'

function validateUser(user: any): Result<User> {
  if (!user.email) {
    return Err(validationError('MISSING_EMAIL', 'Email is required'))
  }
  
  if (!user.email.includes('@')) {
    return Err(validationError('INVALID_EMAIL', 'Email format is invalid', {
      field: 'email',
      value: user.email
    }))
  }
  
  return Ok(user as User)
}
```

### Error Handling in Chains

```typescript
import { flatMap, mapError } from '@qi/qicore-foundation/base'

async function processUserRegistration(userData: any): Promise<Result<User>> {
  const validationResult = validateUser(userData)
  
  return flatMap(validationResult, async user => {
    const saveResult = await saveUser(user)
    
    return mapError(saveResult, error => 
      chainError(
        businessError('REGISTRATION_FAILED', 'User registration failed'),
        error
      )
    )
  })
}
```

## Aggregate Errors

Handle multiple errors together:

```typescript
import { createAggregateError } from '@qi/qicore-foundation/base'

const errors = [
  validationError('MISSING_NAME', 'Name is required'),
  validationError('INVALID_EMAIL', 'Email format is invalid'),
  validationError('WEAK_PASSWORD', 'Password too weak')
]

const aggregateError = createAggregateError(
  'VALIDATION_ERRORS',
  'Multiple validation errors occurred',
  errors
)

// Access individual errors
console.log(aggregateError.context?.errors) // Array of QiError
```

## Real-World Example

Here's a comprehensive example showing QiError in action:

```typescript
import { 
  Ok, Err, flatMap, mapError, 
  type Result, type QiError,
  validationError, networkError, systemError, businessError,
  chainError, withContext, getRetryStrategy, hasCategory
} from '@qi/qicore-foundation/base'

interface User {
  id: number
  email: string
  name: string
}

interface UserService {
  findById(id: number): Promise<Result<User>>
  save(user: User): Promise<Result<User>>
  sendWelcomeEmail(user: User): Promise<Result<void>>
}

class UserRegistrationService {
  constructor(private userService: UserService) {}

  async registerUser(userData: any): Promise<Result<User>> {
    // Step 1: Validate input
    const validationResult = this.validateUserData(userData)
    if (validationResult.tag === 'failure') {
      return validationResult
    }

    // Step 2: Save user
    const saveResult = await this.userService.save(validationResult.value)
    
    return flatMap(saveResult, async user => {
      // Step 3: Send welcome email (non-critical)
      const emailResult = await this.userService.sendWelcomeEmail(user)
      
      if (emailResult.tag === 'failure') {
        // Log email failure but don't fail registration
        console.warn('Welcome email failed:', emailResult.error)
      }
      
      return Ok(user)
    })
  }

  private validateUserData(data: any): Result<User> {
    const errors: QiError[] = []

    if (!data.name) {
      errors.push(validationError('MISSING_NAME', 'Name is required'))
    }

    if (!data.email) {
      errors.push(validationError('MISSING_EMAIL', 'Email is required'))
    } else if (!this.isValidEmail(data.email)) {
      errors.push(validationError('INVALID_EMAIL', 'Email format is invalid', {
        field: 'email',
        value: data.email
      }))
    }

    if (errors.length > 0) {
      return Err(createAggregateError(
        'VALIDATION_FAILED',
        'User data validation failed',
        errors
      ))
    }

    return Ok({
      id: 0, // Will be assigned by database
      name: data.name,
      email: data.email
    })
  }

  private isValidEmail(email: string): boolean {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
  }
}

// Usage with error handling
async function handleUserRegistration(userData: any) {
  const service = new UserRegistrationService(userService)
  const result = await service.registerUser(userData)

  if (result.tag === 'success') {
    console.log('User registered successfully:', result.value)
  } else {
    const error = result.error
    
    // Handle different error categories
    if (hasCategory(error, 'VALIDATION')) {
      console.log('Validation errors:', error.message)
      // Show user-friendly validation messages
    } else if (hasCategory(error, 'NETWORK')) {
      console.log('Network error:', error.message)
      // Maybe retry the operation
      const strategy = getRetryStrategy(error)
      if (strategy.shouldRetry) {
        console.log(`Retrying in ${strategy.backoffMs}ms...`)
      }
    } else {
      console.error('Unexpected error:', error.message)
      // Log full error details for debugging
      console.error('Error context:', error.context)
    }
  }
}
```

## Error Logging Integration

QiError works well with structured logging:

```typescript
import { createLogger } from '@qi/qicore-foundation'

const logger = createLogger({ name: 'user-service' })

function logError(error: QiError, context?: Record<string, unknown>) {
  logger.error(error.message, {
    errorCode: error.code,
    errorCategory: error.category,
    errorContext: error.context,
    timestamp: error.timestamp,
    cause: error.cause,
    ...context
  })
}

// Usage
const error = networkError('API_FAILED', 'External API call failed')
logError(error, { userId: 123, operation: 'fetchUserProfile' })
```

## Best Practices

1. **Use appropriate categories**: Choose the right error category for proper handling
2. **Provide context**: Include relevant debugging information
3. **Chain errors**: Link related errors to show causality
4. **Don't over-contextualize**: Keep context relevant and structured
5. **Use retry strategies**: Leverage built-in retry guidance
6. **Log structured errors**: Use structured logging for better observability
7. **Handle aggregates**: Use aggregate errors for validation scenarios

## Common Patterns

```typescript
// Pattern 1: Validation with multiple errors
const validateInput = (data: any): Result<ValidData> => {
  const errors: QiError[] = []
  // ... validation logic
  return errors.length > 0 
    ? Err(createAggregateError('VALIDATION_FAILED', 'Validation failed', errors))
    : Ok(validData)
}

// Pattern 2: API call with error categorization
const apiCall = async (endpoint: string): Promise<Result<ApiResponse>> => {
  try {
    const response = await fetch(endpoint)
    if (!response.ok) {
      return Err(networkError('API_ERROR', `API call failed: ${response.status}`))
    }
    return Ok(await response.json())
  } catch (error) {
    return Err(systemError('NETWORK_ERROR', 'Network request failed'))
  }
}

// Pattern 3: Error enhancement in service layers
const enhanceError = (error: QiError, operation: string): QiError => {
  return chainError(
    businessError('OPERATION_FAILED', `${operation} failed`),
    withContext(error, { operation, timestamp: new Date().toISOString() })
  )
}
```

## Next Steps

Now that you understand QiError, learn about:
- [Advanced Result Operations](./04-advanced-result.md)
- [Configuration Management](./05-configuration.md)

## Resources

- [QiError API Reference](../api/base/error.md)
- [Result<T> Integration](../api/base/result.md)
- [Error Handling Examples](../../app/error-handling)