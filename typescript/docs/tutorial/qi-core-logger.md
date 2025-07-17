# Tool: Logger

## What It Does

Logger provides a complete structured logging system with three main concepts:
- **Structured Logging**: Every log entry includes structured data for better observability
- **Context Management**: Add context that flows through operations automatically
- **Result<T> Integration**: Log success and failure cases consistently

**See [Complete API Documentation](../api/core/logger.md) for all available functions.**

## Why You Need This

You need to observe what's happening in your application, especially errors. Logger provides structured logging that integrates seamlessly with Result<T> patterns.

## Core Concepts

### 1. Structured Logging
Every log entry includes structured data:

```typescript
// Instead of plain string logging
console.log('User updated')
console.error('Failed to update user:', error.message)

// Use structured logging
logger.info('User updated', undefined, { userId: '123', operation: 'update' })
logger.error('User update failed', error, { userId: '123', operation: 'update' })
```

### 2. Context Management
Add context that flows through operations:

```typescript
// Create logger with context
const requestLogger = logger.withContext({
  requestId: 'req_123',
  userId: 'user_456'
})

requestLogger.info('Processing request')  // Includes requestId and userId automatically

// Add more context for specific operations
const operationLogger = requestLogger.child({ operation: 'validation' })
operationLogger.debug('Validating input')  // Includes requestId, userId, and operation
```

### 3. Result<T> Integration
Log success and failure cases consistently:

```typescript
const result = await processOrder(orderData)

match(
  order => logger.info('Order processed', undefined, { 
    orderId: order.id, 
    amount: order.total 
  }),
  error => logger.error('Order failed', error, { 
    orderId: orderData.id, 
    stage: error.context?.stage 
  }),
  result
)
```

```typescript
// Instead of basic console logging:
console.log('User updated')
console.error('Failed to update user:', error.message)

// Use structured logging:
logger.info('User updated', undefined, { userId: '123', operation: 'update' })
logger.error('User update failed', error, { userId: '123', operation: 'update' })
```

## Basic Usage

### Import What You Need

```typescript
import { createLogger, type Result, type LoggerError } from '@qi/core'
import { match } from '@qi/base'
```

### Create a Logger

```typescript
const loggerResult = createLogger({
  level: 'info',
  name: 'app',
  pretty: true  // Pretty print for development
})

match(
  logger => {
    logger.info('Logger created successfully')
  },
  error => {
    console.error('Failed to create logger:', error.message)
  },
  loggerResult
)
```

### Log Messages

```typescript
const logger = createLogger({ level: 'info' }).value

// Basic logging
logger.info('Application started')
logger.warn('This is a warning')
logger.error('An error occurred')

// With context
logger.info('User action', undefined, { 
  userId: '123', 
  action: 'login',
  timestamp: Date.now()
})

// With error and context
logger.error('Database connection failed', dbError, {
  host: 'localhost',
  port: 5432,
  retryAttempt: 3
})
```

## Log Levels

Available levels (in order): `trace`, `debug`, `info`, `warn`, `error`, `fatal`

```typescript
const logger = createLogger({ level: 'info' }).value

logger.trace('Very detailed info')  // Won't appear (below 'info')
logger.debug('Debug information')   // Won't appear (below 'info')
logger.info('General information')  // Will appear
logger.warn('Warning message')      // Will appear  
logger.error('Error occurred')      // Will appear
logger.fatal('Critical failure')    // Will appear
```

## Context Management

Add context that gets included in all log messages:

```typescript
const logger = createLogger({ level: 'info' }).value

// Add context to logger instance
const requestLogger = logger.withContext({
  requestId: 'req_123',
  userId: 'user_456'
})

requestLogger.info('Processing request')
// Logs with requestId and userId automatically included

// Create child logger with additional context
const operationLogger = requestLogger.child({ operation: 'validation' })
operationLogger.debug('Validating input')
// Logs with requestId, userId, and operation
```

## Result<T> Integration

Logger integrates perfectly with Result<T> patterns:

```typescript
const logger = createLogger({ level: 'info' }).value

async function processOrder(orderData: OrderData): Promise<Result<Order, OrderError>> {
  logger.info('Processing order', undefined, { orderId: orderData.id })
  
  const result = await validateAndCreateOrder(orderData)
  
  // Log based on Result<T> outcome
  match(
    order => logger.info('Order processed successfully', undefined, {
      orderId: order.id,
      amount: order.total,
      status: order.status
    }),
    error => logger.error('Order processing failed', error, {
      orderId: orderData.id,
      stage: error.context?.stage,
      reason: error.category
    }),
    result
  )
  
  return result
}
```

## Environment-Based Configuration

Use different configurations for different environments:

```typescript
import { getLoggerEnvironmentConfig } from '@qi/core'

// Automatically selects config based on NODE_ENV
const config = getLoggerEnvironmentConfig()
const logger = createLogger(config).value

// Development: debug level, pretty printed
// Production: info level, JSON format  
// Test: warn level, minimal output
```

## Real Example

```typescript
import { createLogger, ConfigBuilder } from '@qi/core'
import { match, flatMap } from '@qi/base'

// Load logger configuration
const configResult = ConfigBuilder
  .fromYamlFile('./config.yaml')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .build()

match(
  config => {
    // Create logger with configuration
    const loggerResult = createLogger({
      level: config.get('logging.level', 'info'),
      name: config.get('app.name', 'app'),
      pretty: config.get('logging.pretty', false)
    })
    
    match(
      logger => {
        logger.info('Application starting', undefined, {
          name: config.get('app.name'),
          version: config.get('app.version'),
          environment: config.get('app.environment')
        })
        
        // Use logger throughout your application
        startApplication(config, logger)
      },
      error => {
        console.error('Logger creation failed:', error.message)
        process.exit(1)
      },
      loggerResult
    )
  },
  error => {
    console.error('Configuration error:', error.message)
    process.exit(1)
  },
  configResult
)

async function startApplication(config: Config, logger: Logger) {
  // Create request-specific logger
  const requestHandler = (req, res, next) => {
    const requestLogger = logger.withContext({
      requestId: req.headers['x-request-id'] || generateId(),
      method: req.method,
      path: req.path
    })
    
    requestLogger.info('Request started')
    
    // Add logger to request for use in handlers
    req.logger = requestLogger
    
    res.on('finish', () => {
      requestLogger.info('Request completed', undefined, {
        statusCode: res.statusCode,
        duration: Date.now() - req.startTime
      })
    })
    
    next()
  }
  
  // Use in route handlers
  app.get('/users/:id', async (req, res) => {
    const result = await getUserById(req.params.id)
    
    match(
      user => {
        req.logger.info('User retrieved', undefined, { userId: user.id })
        res.json(user)
      },
      error => {
        req.logger.error('Failed to get user', error, { userId: req.params.id })
        res.status(500).json({ error: error.message })
      },
      result
    )
  })
}
```

## Error Logging Best Practices

```typescript
const logger = createLogger({ level: 'info' }).value

// Log errors with full context
async function processPayment(paymentData: PaymentData): Promise<Result<Payment, PaymentError>> {
  const logger = requestLogger.child({ operation: 'processPayment' })
  
  logger.info('Starting payment processing', undefined, {
    amount: paymentData.amount,
    currency: paymentData.currency,
    paymentMethod: paymentData.method
  })
  
  const result = await chargePayment(paymentData)
  
  match(
    payment => {
      logger.info('Payment processed successfully', undefined, {
        paymentId: payment.id,
        amount: payment.amount,
        status: payment.status,
        processingTime: payment.metadata.processingTime
      })
    },
    error => {
      // Log error with structured context
      logger.error('Payment processing failed', error, {
        amount: paymentData.amount,
        paymentMethod: paymentData.method,
        errorCategory: error.category,
        shouldRetry: error.category === 'NETWORK',
        vendorCode: error.context?.vendorCode
      })
    },
    result
  )
  
  return result
}
```

## Integration with Other Tools

Logger works seamlessly with Config and Cache:

```typescript
// Logger configured from Config
const logger = createLogger({
  level: config.get('logging.level'),
  name: config.get('app.name')
}).value

// Log cache operations
const cache = createCache({ backend: 'redis' }).value

async function getCachedUser(id: string): Promise<Result<User, CacheError>> {
  logger.debug('Checking cache for user', undefined, { userId: id })
  
  const result = await cache.get(`user:${id}`)
  
  match(
    user => logger.debug('Cache hit', undefined, { userId: id }),
    error => logger.debug('Cache miss', undefined, { userId: id, reason: error.message }),
    result
  )
  
  return result
}
```

## Key Benefits

1. **Structured data** - Every log entry includes relevant context
2. **Result<T> integration** - Log success and failure cases consistently
3. **Context accumulation** - Add context that flows through operations
4. **Environment awareness** - Different configs for dev/prod/test
5. **Type safety** - Logger creation returns Result<T>

## Working Example

Try the complete working example:

```bash
cd typescript/app/config-example
bun run dev
```

This shows Logger integrated with Config loading, demonstrating structured logging with context.

## Next Steps

- [Cache Tool](./qi-core-cache.md) - Add performance optimization to your application
- [Config Tool](./qi-core-config.md) - Review configuration management
- [Back to qi/base](./qi-base.md) - Review the framework patterns