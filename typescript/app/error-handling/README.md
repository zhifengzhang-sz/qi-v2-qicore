# Error Handling Example

This example demonstrates comprehensive error handling patterns using QiCore Foundation's QiError and Result<T> types.

## Features Demonstrated

- **Structured Error Creation**: Using QiError with categories and context
- **Error Chaining**: Linking related errors to show causality
- **Error Recovery**: Implementing retry strategies and fallback mechanisms
- **Validation Errors**: Handling multiple validation errors with aggregation
- **Service Integration**: Error handling in service layers
- **Logging Integration**: Structured error logging
- **API Error Responses**: Converting errors to HTTP responses

## Key Concepts

### 1. Error Categories
- **VALIDATION**: Input validation errors
- **NETWORK**: Network-related errors
- **SYSTEM**: System/infrastructure errors
- **BUSINESS**: Business logic violations
- **AUTHENTICATION**: Authentication failures
- **AUTHORIZATION**: Authorization/permission errors
- **TIMEOUT**: Timeout errors

### 2. Error Context
- Structured metadata for debugging
- Request tracing information
- User context and permissions
- System state information

### 3. Error Chaining
- Link related errors to show causality
- Preserve error history for debugging
- Format error chains for display

### 4. Retry Strategies
- Automatic retry based on error category
- Exponential backoff implementation
- Circuit breaker pattern

## Running the Example

```bash
# Install dependencies
bun install

# Run basic error handling demo
bun run dev

# Run specific scenarios
bun run dev -- validation
bun run dev -- network
bun run dev -- retry
bun run dev -- chain
bun run dev -- service
```

## Example Scenarios

### Validation Errors
```typescript
const validateUser = (data: any): Result<User, QiError> => {
  const errors: QiError[] = []
  
  if (!data.name) {
    errors.push(validationError('MISSING_NAME', 'Name is required'))
  }
  
  if (!data.email) {
    errors.push(validationError('MISSING_EMAIL', 'Email is required'))
  }
  
  if (errors.length > 0) {
    return Err(createAggregateError('VALIDATION_FAILED', 'Validation failed', errors))
  }
  
  return Ok(data as User)
}
```

### Network Error Handling
```typescript
const fetchUserData = async (id: number): Promise<Result<User, QiError>> => {
  try {
    const response = await fetch(`/api/users/${id}`)
    
    if (!response.ok) {
      return Err(networkError('API_ERROR', `HTTP ${response.status}: ${response.statusText}`, {
        endpoint: `/api/users/${id}`,
        statusCode: response.status,
        method: 'GET'
      }))
    }
    
    const data = await response.json()
    return Ok(data)
  } catch (error) {
    return Err(networkError('NETWORK_ERROR', 'Network request failed', {
      endpoint: `/api/users/${id}`,
      originalError: error instanceof Error ? error.message : 'Unknown error'
    }))
  }
}
```

### Error Recovery with Retry
```typescript
const withRetry = async <T>(
  operation: () => Promise<Result<T, QiError>>,
  maxRetries: number = 3
): Promise<Result<T, QiError>> => {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    const result = await operation()
    
    if (result.tag === 'success') {
      return result
    }
    
    const strategy = getRetryStrategy(result.error)
    
    if (!strategy.shouldRetry || attempt >= maxRetries) {
      return result
    }
    
    await delay(strategy.backoffMs * attempt)
  }
  
  return Err(systemError('MAX_RETRIES_EXCEEDED', 'Maximum retry attempts exceeded'))
}
```

## Learning Objectives

After running this example, you'll understand:
- How to create structured errors with QiError
- When to use different error categories
- How to implement error chaining and context
- How to build retry and recovery mechanisms
- How to handle errors in service architectures
- How to convert errors to API responses
- How to implement comprehensive error logging