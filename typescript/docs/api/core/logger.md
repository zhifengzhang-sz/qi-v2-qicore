# Logger API Reference

The Logger module provides structured logging with Pino integration using functional patterns. It offers high-performance event-driven logging with OpenTelemetry integration.

## Type Definitions

```typescript
type LogLevel = 'debug' | 'info' | 'warn' | 'error' | 'fatal'

interface LogEntry {
  readonly level: LogLevel
  readonly message: string
  readonly timestamp: Date
  readonly context?: Record<string, unknown>
  readonly error?: Error
  readonly traceId?: string
  readonly spanId?: string
}

interface LoggerConfig {
  readonly level: LogLevel
  readonly name?: string
  readonly pretty?: boolean
  readonly destination?: string
  readonly redact?: string[]
  readonly serializers?: Record<string, (obj: unknown) => unknown>
  readonly hooks?: {
    logMethod?: (inputArgs: unknown[], method: unknown) => void
  }
}

type LoggerContext = Record<string, unknown>

interface LoggerEvents {
  log: (entry: LogEntry) => void
  error: (error: LoggerError) => void
  level: (level: LogLevel) => void
}

interface LoggerError extends QiError {
  readonly category: 'LOGGER'
  readonly context: {
    readonly operation?: string
    readonly level?: LogLevel
    readonly logger?: string
  }
}
```

## Core Class

### Logger

Event-driven logger with Pino integration and structured JSON logging capabilities.

```typescript
class Logger {
  constructor(config: LoggerConfig)
  
  // Logging methods
  debug(message: string, context?: LoggerContext): void
  info(message: string, context?: LoggerContext): void
  warn(message: string, context?: LoggerContext): void
  error(message: string, error?: Error, context?: LoggerContext): void
  fatal(message: string, error?: Error, context?: LoggerContext): void
  
  // Child logger
  child(context: LoggerContext): Logger
  
  // Level management
  isLevelEnabled(level: LogLevel): boolean
  getLevel(): LogLevel
  setLevel(level: LogLevel): void
  
  // Configuration
  getConfig(): LoggerConfig
  
  // Event system
  on<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  once<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  off<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  
  // Lifecycle
  flush(): Promise<void>
  close(): void
}
```

## Factory Functions

### createLogger(config: LoggerConfig): Result<Logger, LoggerError>

Creates a new logger instance with the specified configuration.

```typescript
const result = createLogger({
  level: 'info',
  name: 'app',
  pretty: true
})

if (result.tag === 'success') {
  const logger = result.value
  logger.info('Application started')
}
```

### createFromEnv(): Result<Logger, LoggerError>

Creates a logger from environment variables (NODE_ENV and LOG_LEVEL).

```typescript
// Uses NODE_ENV to determine config and LOG_LEVEL for level override
const result = createFromEnv()

if (result.tag === 'success') {
  const logger = result.value
  logger.info('Logger created from environment')
}
```

## Utility Functions

### loggerError(message: string, context?: LoggerError['context']): LoggerError

Creates a logger-specific error.

```typescript
const error = loggerError('Failed to write log', { 
  operation: 'log',
  level: 'error'
})
```

### formatError(error: Error): Record<string, unknown>

Formats an Error object for structured logging.

```typescript
try {
  throw new Error('Something went wrong')
} catch (err) {
  const formatted = formatError(err)
  logger.error('Operation failed', err, { formatted })
}
```

### createRequestLogger(logger: Logger): object

Creates request logging utilities (returns object with methods, not Express middleware).

```typescript
const logger = createLogger({ level: 'info' }).value
const requestLogger = createRequestLogger(logger)

// Object with logging methods
requestLogger.logRequest(req, { userId: '123' })
requestLogger.logResponse(req, res, duration, { operation: 'getUser' })
requestLogger.logError(error, req, { step: 'validation' })
```

## Configuration Presets

### developmentConfig: LoggerConfig

Development-friendly configuration with pretty printing.

```typescript
const config = developmentConfig
// { level: 'debug', name: 'qicore-dev', pretty: true }

const logger = createLogger(config).value
```

### productionConfig: LoggerConfig

Production-optimized configuration with redaction.

```typescript
const config = productionConfig
// { level: 'warn', name: 'qicore-prod', pretty: false, redact: ['password', 'token', ...] }

const logger = createLogger(config).value
```

### testConfig: LoggerConfig

Test environment configuration with minimal output.

```typescript
const config = testConfig
// { level: 'fatal', name: 'qicore-test', pretty: false }

const logger = createLogger(config).value
```

### getEnvironmentConfig(): LoggerConfig

Gets configuration based on NODE_ENV with LOG_LEVEL override.

```typescript
const config = getEnvironmentConfig()
// Returns development, production, or test config based on NODE_ENV
// LOG_LEVEL environment variable overrides the default level

const logger = createLogger(config).value
```

## Usage Examples

### Basic Logging

```typescript
import { createLogger } from '@qi/core'

const loggerResult = createLogger({ 
  level: 'info',
  name: 'app',
  pretty: true
})

if (loggerResult.tag === 'success') {
  const logger = loggerResult.value
  
  logger.info('Application started')
  logger.warn('This is a warning', { userId: '123' })
  
  try {
    throw new Error('Something went wrong')
  } catch (error) {
    logger.error('An error occurred', error, { operation: 'startup' })
  }
}
```

### Child Loggers with Context

```typescript
const logger = createLogger({ level: 'info' }).value

// Create child logger with persistent context
const requestLogger = logger.child({ 
  requestId: 'req_123', 
  userId: 'user_456' 
})

requestLogger.info('Processing request')
// Logs with requestId and userId automatically included

// Create nested child logger
const operationLogger = requestLogger.child({ operation: 'validation' })
operationLogger.debug('Validating input')
// Logs with requestId, userId, and operation
```

### Event System

```typescript
const logger = createLogger({ level: 'debug' }).value

// Monitor logging events
logger.on('log', (entry) => {
  console.log(`Logged at ${entry.level}: ${entry.message}`)
})

logger.on('error', (error) => {
  console.error('Logger error:', error.message)
})

logger.on('level', (level) => {
  console.log(`Log level changed to: ${level}`)
})

// Trigger events
logger.info('This will trigger the log event')
logger.setLevel('warn') // This will trigger the level event
```

### Level Management

```typescript
const logger = createLogger({ level: 'info' }).value

// Check if level is enabled
if (logger.isLevelEnabled('debug')) {
  logger.debug('Debug information', { expensive: computeExpensiveData() })
}

// Get current level
console.log('Current level:', logger.getLevel()) // 'info'

// Change level at runtime
logger.setLevel('debug')
console.log('New level:', logger.getLevel()) // 'debug'
```

### Request Logging

```typescript
import express from 'express'
import { createLogger, createRequestLogger } from '@qi/core'

const app = express()
const logger = createLogger({ level: 'info' }).value
const requestLogger = createRequestLogger(logger)

app.use((req, res, next) => {
  const start = Date.now()
  
  // Log incoming request
  requestLogger.logRequest(req, { 
    userAgent: req.headers['user-agent'] 
  })
  
  res.on('finish', () => {
    const duration = Date.now() - start
    
    // Log response
    requestLogger.logResponse(req, res, duration, {
      route: req.route?.path
    })
  })
  
  next()
})

app.get('/users/:id', (req, res) => {
  const requestLogger = logger.child({ 
    requestId: req.headers['x-request-id'],
    userId: req.params.id
  })
  
  try {
    requestLogger.info('Fetching user')
    // Business logic...
    requestLogger.info('User fetched successfully')
    res.json({ user: 'data' })
  } catch (error) {
    requestLogger.error('Failed to fetch user', error)
    res.status(500).json({ error: 'Internal server error' })
  }
})
```

### Environment-Based Configuration

```typescript
// Automatically configures based on NODE_ENV and LOG_LEVEL
const config = getEnvironmentConfig()
const logger = createLogger(config).value

// In development: debug level, pretty printed
// In production: warn level, JSON format, redacted fields
// In test: fatal level, minimal output
// LOG_LEVEL env var overrides default levels

logger.info('Logger configured for environment')
```

### Error Logging with Context

```typescript
const logger = createLogger({ level: 'info' }).value

async function processOrder(orderId: string) {
  const orderLogger = logger.child({ orderId, operation: 'processOrder' })
  
  try {
    orderLogger.info('Starting order processing')
    
    // Simulate operation
    await validateOrder(orderId)
    orderLogger.debug('Order validated')
    
    await chargePayment(orderId)
    orderLogger.debug('Payment charged')
    
    orderLogger.info('Order processed successfully')
    
  } catch (error) {
    orderLogger.error('Order processing failed', error, {
      stage: 'payment',
      retryable: error.code === 'NETWORK_ERROR'
    })
    throw error
  }
}
```

### Configuration Options

```typescript
// Custom logger configuration
const logger = createLogger({
  level: 'info',
  name: 'my-service',
  pretty: process.env.NODE_ENV !== 'production',
  destination: './logs/app.log', // Log to file
  redact: ['password', 'secret', 'token'], // Redact sensitive fields
  serializers: {
    user: (user) => ({ id: user.id, name: user.name }) // Custom serialization
  }
}).value

// Using with custom destination
const fileLogger = createLogger({
  level: 'info',
  destination: '/var/log/app.log'
}).value

// Logging to stderr for errors
const errorLogger = createLogger({
  level: 'error',
  destination: 'stderr'
}).value
```

### Integration with Result<T>

```typescript
import { Result, success, failure, match } from '@qi/base'

const logger = createLogger({ level: 'info' }).value

async function processOrder(orderData: OrderData): Promise<Result<Order, OrderError>> {
  const orderLogger = logger.child({ orderId: orderData.id })
  
  orderLogger.info('Processing order')
  
  try {
    const order = await processOrderInternal(orderData)
    orderLogger.info('Order processed successfully', { orderId: order.id })
    return success(order)
  } catch (error) {
    const orderError = orderError('Order processing failed', { orderId: orderData.id })
    orderLogger.error('Order processing failed', error, { 
      originalError: error.message 
    })
    return failure(orderError)
  }
}

// Log based on Result<T> outcome
const result = await processOrder(orderData)
match(
  (order) => logger.info('Order completed', { orderId: order.id }),
  (error) => logger.error('Order failed', undefined, { 
    errorCode: error.code,
    context: error.context 
  }),
  result
)
```

## Best Practices

1. **Create once, use everywhere**: Create logger at application startup
2. **Use child loggers**: Add context with `child()` for request/operation scoping
3. **Log errors properly**: Use error parameter for debug/info/warn, error object for error/fatal
4. **Use appropriate levels**: debug/info for development, warn+ for production
5. **Add meaningful context**: Include relevant business data and IDs
6. **Monitor events**: Use event system for metrics and monitoring integration
7. **Don't log sensitive data**: Use redact configuration for passwords/tokens
8. **Use environment configs**: Different settings for dev/prod/test environments
9. **Handle Result<T>**: Log both success and failure cases appropriately
10. **Performance**: Check `isLevelEnabled()` before expensive context computation

## Performance Considerations

- Pino backend provides high-performance JSON logging
- Level checking is optimized for disabled levels
- Child loggers share configuration and event emitters efficiently
- Pretty printing is disabled automatically in production presets
- Event system uses EventEmitter3 for minimal overhead
- Redaction and serialization happen at log time, not logger creation

## Error Handling

Logger creation returns Result<T> but log calls are fire-and-forget:

```typescript
// Factory functions return Result<T>
const loggerResult = createLogger(config)
if (loggerResult.tag === 'failure') {
  console.error('Failed to create logger:', loggerResult.error.message)
}

// Log calls never fail or throw (fire-and-forget)
logger.info('This always succeeds')
logger.error('Even this succeeds', someError)
```

## Integration with OpenTelemetry

The logger automatically includes trace context when available:

```typescript
// LogEntry includes traceId and spanId when OpenTelemetry is active
logger.info('Operation completed') 
// Logs: { level: 'info', message: 'Operation completed', traceId: '...', spanId: '...' }
```