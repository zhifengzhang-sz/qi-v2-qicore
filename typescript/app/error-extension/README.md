# Error Extension Example

Demonstrates how to create domain-specific error types and comprehensive error handling strategies with QiCore Foundation.

## Features Demonstrated

- **Domain-Specific Error Types**: UserError, PaymentError, OrderError
- **Error Factory Functions**: Type-safe error creation
- **Error Handling Strategies**: Category-based error responses
- **Error Composition**: Combining multiple domains with different error types
- **Retry Logic**: Error categories determine retry strategies
- **Structured Context**: Rich error context for debugging
- **Type Safety**: Full TypeScript support for error handling

## Running the Example

```bash
bun run dev
```

## Key Concepts

### 1. Domain-Specific Error Types

Instead of generic errors, create specific types for each domain:

```typescript
interface UserError extends QiError {
  category: 'VALIDATION' | 'BUSINESS' | 'AUTHENTICATION' | 'AUTHORIZATION'
  context: {
    userId?: string
    field?: string
    operation?: string
    permissions?: string[]
  }
}

interface PaymentError extends QiError {
  category: 'NETWORK' | 'BUSINESS' | 'VALIDATION'
  context: {
    paymentId?: string
    amount?: number
    currency?: string
    provider?: string
  }
}
```

### 2. Error Factory Functions

Create type-safe error factories:

```typescript
function createUserError(
  code: string,
  message: string,
  category: UserError['category'],
  context: UserError['context'] = {}
): UserError {
  return create(code, message, category, context) as UserError
}
```

### 3. Error Handling Strategies

Handle errors based on their category and context:

```typescript
function handleUserError(error: UserError, logger: any) {
  switch (error.category) {
    case 'VALIDATION':
      return { status: 400, message: error.message, field: error.context.field }
    case 'AUTHENTICATION':
      return { status: 401, message: 'Please login', redirectTo: '/login' }
    case 'AUTHORIZATION':
      return { status: 403, message: 'Access denied' }
    case 'BUSINESS':
      return { status: 422, message: error.message, context: error.context }
  }
}
```

### 4. Error Composition

Combine operations from different domains:

```typescript
async function fullOrderFlow(userData: any, paymentData: any, orderData: any) {
  const userResult = await validateUser(userData)
  if (userResult.tag === 'failure') return userResult

  const authResult = await authenticateUser(userData.email, userData.password)
  if (authResult.tag === 'failure') return authResult

  const paymentResult = await processPayment(paymentData)
  if (paymentResult.tag === 'failure') return paymentResult

  const orderResult = await createOrder(orderData)
  if (orderResult.tag === 'failure') return orderResult

  return success({ user: authResult.value, payment: paymentResult.value, order: orderResult.value })
}
```

### 5. Retry Strategies

Error categories automatically determine retry behavior:

```typescript
const retryStrategy = getRetryStrategy(error.category)
// 'NETWORK' errors -> exponential backoff
// 'VALIDATION' errors -> never retry
// 'BUSINESS' errors -> never retry
```

## Error Categories and Responses

| Category | HTTP Status | Retry Strategy | Use Case |
|----------|-------------|----------------|----------|
| VALIDATION | 400 | Never | Input validation failures |
| AUTHENTICATION | 401 | Never | Login failures |
| AUTHORIZATION | 403 | Never | Permission denied |
| BUSINESS | 422 | Never | Business rule violations |
| NETWORK | 502 | Exponential backoff | Service unavailable |
| SYSTEM | 500 | Linear backoff | Internal errors |

## Benefits

1. **Type Safety**: Compiler ensures proper error handling
2. **Consistency**: Same error patterns across all domains
3. **Debugging**: Rich context makes issues easier to trace
4. **Retry Logic**: Automatic retry decisions based on error type
5. **API Responses**: Consistent error response format
6. **Monitoring**: Structured errors enable better alerting