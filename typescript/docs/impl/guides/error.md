# Error Implementation Guide

## Overview

The Error module provides structured error handling with QiError types that support categorization, context, and error chaining. It implements a complete error handling system that integrates seamlessly with Result<T> for functional error management.

## Design Approach

**Structured errors with categorization and context**

### Rationale
- Errors should carry structured information for better debugging
- Categories enable automated retry strategies and handling
- Context provides specific information about error conditions
- Chaining preserves causal relationships between errors

### Key Decisions
- All errors implement QiError interface
- Categories follow contract specifications exactly
- Context is type-safe and extensible
- Error chaining preserves stack traces and causality

## Core Interface

### QiError Structure
```typescript
export interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Record<string, unknown>
  readonly cause?: QiError
  readonly timestamp: Date
  readonly stack?: string
}
```

### Error Categories
```typescript
export type ErrorCategory = 
  | 'VALIDATION'    // Input validation failures
  | 'NETWORK'       // Network connectivity issues
  | 'BUSINESS'      // Business logic violations
  | 'SYSTEM'        // System-level errors
  | 'RESOURCE'      // Resource access/availability
  | 'AUTHENTICATION' // Authentication failures
  | 'AUTHORIZATION'  // Authorization/permission failures
  | 'PARSING'       // Data parsing/format errors
  | 'TIMEOUT'       // Operation timeout errors
  | 'CONFLICT'      // Resource conflict errors
```

## Factory Functions

### Core Error Factories
```typescript
// Generic error creation
export const create = (
  code: string,
  message: string,
  category: ErrorCategory,
  context: Record<string, unknown> = {}
): QiError

// Category-specific factories
export const validationError = (message: string, context?: Record<string, unknown>): QiError
export const networkError = (message: string, context?: Record<string, unknown>): QiError
export const businessError = (message: string, context?: Record<string, unknown>): QiError
export const systemError = (message: string, context?: Record<string, unknown>): QiError
export const resourceError = (message: string, context?: Record<string, unknown>): QiError
export const authenticationError = (message: string, context?: Record<string, unknown>): QiError
export const authorizationError = (message: string, context?: Record<string, unknown>): QiError
export const parsingError = (message: string, context?: Record<string, unknown>): QiError
export const timeoutError = (message: string, context?: Record<string, unknown>): QiError
export const conflictError = (message: string, context?: Record<string, unknown>): QiError
```

### Usage Examples
```typescript
// Basic error creation
const error = create('USER_NOT_FOUND', 'User not found', 'VALIDATION', {
  userId: '123',
  operation: 'fetchUser'
})

// Category-specific creation
const validationErr = validationError('Email format invalid', {
  field: 'email',
  value: 'invalid-email'
})

const networkErr = networkError('Connection timeout', {
  host: 'api.example.com',
  port: 443,
  timeout: 30000
})
```

## Error Chaining

### Causal Relationships
```typescript
// Chain errors to preserve causality
export const withCause = (error: QiError, cause: QiError): QiError

// Add context to existing errors
export const withContext = (error: QiError, context: Record<string, unknown>): QiError

// Usage example
const databaseError = systemError('Database connection failed', {
  host: 'db.example.com',
  port: 5432
})

const serviceError = businessError('Failed to retrieve user data', {
  userId: '123',
  operation: 'getUser'
})

const chainedError = withCause(serviceError, databaseError)
```

### Error Chain Navigation
```typescript
// Navigate error chains
export const getRootCause = (error: QiError): QiError
export const getErrorChain = (error: QiError): QiError[]
export const hasCategory = (error: QiError, category: ErrorCategory): boolean

// Usage examples
const rootError = getRootCause(chainedError)
const errorChain = getErrorChain(chainedError)
const hasNetworkError = hasCategory(chainedError, 'NETWORK')
```

## Error Formatting

### String Representation
```typescript
// Convert error to string
export const errorToString = (error: QiError): string

// Format error chain
export const formatChain = (error: QiError): string

// Usage examples
const errorString = errorToString(error)
// Output: "USER_NOT_FOUND: User not found"

const chainString = formatChain(chainedError)
// Output: "BUSINESS: Failed to retrieve user data → SYSTEM: Database connection failed"
```

### Structured Formatting
```typescript
// Convert to JSON-serializable format
export const toSerializable = (error: QiError): SerializableError

// Format for logging
export const formatForLogging = (error: QiError): LoggableError

// Usage examples
const serializable = toSerializable(error)
const loggable = formatForLogging(error)
```

## Retry Strategies

### Strategy Determination
```typescript
// Determine retry strategy based on error category
export const getRetryStrategy = (category: ErrorCategory): RetryStrategy

export interface RetryStrategy {
  readonly strategy: 'never' | 'immediate' | 'exponential' | 'linear'
  readonly maxAttempts: number
  readonly initialDelay: number
  readonly maxDelay: number
  readonly backoffMultiplier: number
}
```

### Category-Specific Strategies
```typescript
// Retry strategies by category
const retryStrategies: Record<ErrorCategory, RetryStrategy> = {
  'VALIDATION': { strategy: 'never', maxAttempts: 0, initialDelay: 0, maxDelay: 0, backoffMultiplier: 1 },
  'NETWORK': { strategy: 'exponential', maxAttempts: 3, initialDelay: 1000, maxDelay: 10000, backoffMultiplier: 2 },
  'BUSINESS': { strategy: 'never', maxAttempts: 0, initialDelay: 0, maxDelay: 0, backoffMultiplier: 1 },
  'SYSTEM': { strategy: 'linear', maxAttempts: 2, initialDelay: 500, maxDelay: 5000, backoffMultiplier: 1 },
  'RESOURCE': { strategy: 'exponential', maxAttempts: 5, initialDelay: 100, maxDelay: 5000, backoffMultiplier: 2 },
  'AUTHENTICATION': { strategy: 'never', maxAttempts: 0, initialDelay: 0, maxDelay: 0, backoffMultiplier: 1 },
  'AUTHORIZATION': { strategy: 'never', maxAttempts: 0, initialDelay: 0, maxDelay: 0, backoffMultiplier: 1 },
  'PARSING': { strategy: 'never', maxAttempts: 0, initialDelay: 0, maxDelay: 0, backoffMultiplier: 1 },
  'TIMEOUT': { strategy: 'linear', maxAttempts: 3, initialDelay: 1000, maxDelay: 10000, backoffMultiplier: 1 },
  'CONFLICT': { strategy: 'exponential', maxAttempts: 2, initialDelay: 100, maxDelay: 1000, backoffMultiplier: 2 }
}
```

## Domain-Specific Errors

### Creating Domain Errors
```typescript
// Define domain-specific error types
export interface UserError extends QiError {
  category: 'VALIDATION' | 'BUSINESS' | 'AUTHENTICATION' | 'AUTHORIZATION'
  context: {
    userId?: string
    email?: string
    operation?: string
    field?: string
  }
}

export interface PaymentError extends QiError {
  category: 'NETWORK' | 'BUSINESS' | 'VALIDATION'
  context: {
    paymentId?: string
    amount?: number
    currency?: string
    provider?: string
  }
}

// Domain-specific factories
export const createUserError = (
  code: string,
  message: string,
  category: UserError['category'],
  context: UserError['context'] = {}
): UserError => {
  return create(code, message, category, context) as UserError
}

export const createPaymentError = (
  code: string,
  message: string,
  category: PaymentError['category'],
  context: PaymentError['context'] = {}
): PaymentError => {
  return create(code, message, category, context) as PaymentError
}
```

### Domain Error Usage
```typescript
// User domain errors
const userNotFound = createUserError(
  'USER_NOT_FOUND',
  'User not found',
  'VALIDATION',
  { userId: '123', operation: 'fetchUser' }
)

const insufficientPermissions = createUserError(
  'INSUFFICIENT_PERMISSIONS',
  'User lacks required permissions',
  'AUTHORIZATION',
  { userId: '123', operation: 'deleteUser', requiredPermission: 'admin' }
)

// Payment domain errors
const paymentFailed = createPaymentError(
  'PAYMENT_FAILED',
  'Payment processing failed',
  'NETWORK',
  { paymentId: 'pay_123', amount: 100, provider: 'stripe' }
)
```

## Error Matching and Handling

### Pattern Matching
```typescript
// Handle different error categories
export const handleError = (error: QiError): ErrorResponse => {
  switch (error.category) {
    case 'VALIDATION':
      return {
        status: 400,
        message: error.message,
        code: error.code,
        details: error.context
      }
    
    case 'AUTHENTICATION':
      return {
        status: 401,
        message: 'Authentication required',
        code: 'UNAUTHORIZED'
      }
    
    case 'AUTHORIZATION':
      return {
        status: 403,
        message: 'Access denied',
        code: 'FORBIDDEN'
      }
    
    case 'NETWORK':
      return {
        status: 502,
        message: 'Service unavailable',
        code: 'SERVICE_UNAVAILABLE',
        retryAfter: 30
      }
    
    case 'BUSINESS':
      return {
        status: 422,
        message: error.message,
        code: error.code,
        details: error.context
      }
    
    case 'SYSTEM':
      return {
        status: 500,
        message: 'Internal server error',
        code: 'INTERNAL_ERROR'
      }
    
    default:
      return {
        status: 500,
        message: 'Unknown error',
        code: 'UNKNOWN_ERROR'
      }
  }
}
```

### Error Guards
```typescript
// Type guards for error categories
export const isValidationError = (error: QiError): error is QiError & { category: 'VALIDATION' } =>
  error.category === 'VALIDATION'

export const isNetworkError = (error: QiError): error is QiError & { category: 'NETWORK' } =>
  error.category === 'NETWORK'

export const isBusinessError = (error: QiError): error is QiError & { category: 'BUSINESS' } =>
  error.category === 'BUSINESS'

// Usage with type narrowing
if (isValidationError(error)) {
  // TypeScript knows error.category is 'VALIDATION'
  console.log('Validation failed:', error.message)
}
```

## Integration with Result<T>

### Error Propagation
```typescript
// Errors integrate seamlessly with Result<T>
const validateUser = (data: unknown): Result<User, UserError> => {
  if (!data || typeof data !== 'object') {
    return failure(createUserError(
      'INVALID_DATA',
      'Invalid user data',
      'VALIDATION',
      { data: typeof data }
    ))
  }
  
  // Further validation...
  return success(data as User)
}

// Error chaining with Result<T>
const processUser = async (userData: unknown): Promise<Result<ProcessedUser, UserError>> => {
  const userResult = validateUser(userData)
  if (userResult.tag === 'failure') {
    return userResult
  }
  
  const processingResult = await processUserData(userResult.value)
  if (processingResult.tag === 'failure') {
    return failure(withContext(processingResult.error, {
      originalData: userData,
      processingStep: 'processUserData'
    }))
  }
  
  return success(processingResult.value)
}
```

### Error Composition
```typescript
// Compose errors from multiple operations
const composeErrors = (...errors: QiError[]): QiError => {
  if (errors.length === 0) {
    return systemError('No errors to compose')
  }
  
  if (errors.length === 1) {
    return errors[0]
  }
  
  const primaryError = errors[0]
  const secondaryErrors = errors.slice(1)
  
  return withContext(primaryError, {
    additionalErrors: secondaryErrors.map(err => ({
      code: err.code,
      message: err.message,
      category: err.category
    }))
  })
}
```

## Testing Error Handling

### Unit Testing
```typescript
describe('Error Creation', () => {
  it('should create validation error', () => {
    const error = validationError('Invalid email', { field: 'email' })
    
    expect(error.category).toBe('VALIDATION')
    expect(error.message).toBe('Invalid email')
    expect(error.context.field).toBe('email')
    expect(error.timestamp).toBeInstanceOf(Date)
  })
  
  it('should chain errors', () => {
    const cause = networkError('Connection failed')
    const error = withCause(businessError('Operation failed'), cause)
    
    expect(error.cause).toBe(cause)
    expect(getRootCause(error)).toBe(cause)
  })
})
```

### Integration Testing
```typescript
describe('Error Handling Integration', () => {
  it('should handle validation errors in API', async () => {
    const response = await request(app)
      .post('/api/users')
      .send({ email: 'invalid-email' })
    
    expect(response.status).toBe(400)
    expect(response.body.code).toBe('VALIDATION_ERROR')
    expect(response.body.details.field).toBe('email')
  })
  
  it('should retry on network errors', async () => {
    const mockFetch = vi.fn()
      .mockRejectedValueOnce(networkError('Connection timeout'))
      .mockRejectedValueOnce(networkError('Connection refused'))
      .mockResolvedValueOnce({ data: 'success' })
    
    const result = await retryableOperation(mockFetch)
    
    expect(result.tag).toBe('success')
    expect(mockFetch).toHaveBeenCalledTimes(3)
  })
})
```

## Best Practices

### Error Creation
- Use specific error codes that identify the exact failure
- Include relevant context information
- Choose appropriate error categories
- Preserve error chains when wrapping errors

### Error Handling
- Always handle errors explicitly
- Use pattern matching on error categories
- Implement appropriate retry strategies
- Log errors with full context

### Performance Considerations
- Avoid creating errors in hot paths
- Use error pooling for frequently created errors
- Minimize stack trace capture overhead
- Cache retry strategies by category

### Security
- Sanitize error messages for external APIs
- Don't expose sensitive information in errors
- Use appropriate error codes for different audiences
- Implement proper error logging and monitoring

## Common Patterns

### API Error Responses
```typescript
// Convert QiError to API response
export const toApiResponse = (error: QiError): ApiErrorResponse => {
  const baseResponse = handleError(error)
  
  return {
    ...baseResponse,
    timestamp: error.timestamp.toISOString(),
    traceId: error.context.traceId as string,
    path: error.context.path as string,
    details: sanitizeErrorDetails(error.context)
  }
}
```

### Error Aggregation
```typescript
// Collect and aggregate multiple errors
export class ErrorCollector {
  private errors: QiError[] = []
  
  add(error: QiError): void {
    this.errors.push(error)
  }
  
  hasErrors(): boolean {
    return this.errors.length > 0
  }
  
  getErrors(): QiError[] {
    return [...this.errors]
  }
  
  toSummaryError(): QiError {
    if (this.errors.length === 0) {
      return systemError('No errors collected')
    }
    
    return businessError('Multiple validation errors', {
      errorCount: this.errors.length,
      errors: this.errors.map(err => ({
        code: err.code,
        message: err.message,
        category: err.category
      }))
    })
  }
}
```

### Retry Implementation
```typescript
// Implement retry logic with exponential backoff
export const withRetry = async <T>(
  operation: () => Promise<Result<T, QiError>>,
  error: QiError
): Promise<Result<T, QiError>> => {
  const strategy = getRetryStrategy(error.category)
  
  if (strategy.strategy === 'never') {
    return failure(error)
  }
  
  let attempts = 0
  let delay = strategy.initialDelay
  
  while (attempts < strategy.maxAttempts) {
    await new Promise(resolve => setTimeout(resolve, delay))
    
    const result = await operation()
    if (result.tag === 'success') {
      return result
    }
    
    attempts++
    delay = Math.min(delay * strategy.backoffMultiplier, strategy.maxDelay)
  }
  
  return failure(withContext(error, {
    retryAttempts: attempts,
    finalDelay: delay
  }))
}
```

## Troubleshooting

### Common Issues
1. **Lost error context**: Ensure context is preserved during error transformation
2. **Circular references**: Avoid circular error chains
3. **Memory leaks**: Clear error references in long-running applications
4. **Performance impact**: Minimize error creation in hot paths

### Debugging Tools
```typescript
// Debug error chains
export const debugErrorChain = (error: QiError): void => {
  console.log('Error Chain:')
  const chain = getErrorChain(error)
  
  chain.forEach((err, index) => {
    console.log(`${index}: ${err.code} - ${err.message}`)
    console.log(`   Category: ${err.category}`)
    console.log(`   Context:`, err.context)
  })
}

// Analyze error patterns
export const analyzeErrors = (errors: QiError[]): ErrorAnalysis => {
  const categoryCount = errors.reduce((acc, err) => {
    acc[err.category] = (acc[err.category] || 0) + 1
    return acc
  }, {} as Record<ErrorCategory, number>)
  
  return {
    totalErrors: errors.length,
    categoryDistribution: categoryCount,
    mostCommonCategory: Object.entries(categoryCount)
      .sort(([,a], [,b]) => b - a)[0]?.[0] as ErrorCategory
  }
}
```