# Logger API Reference

The Logger module provides structured logging with context accumulation and Result<T> integration.

## Core Class

### Logger

Immutable logger instance with structured logging capabilities.

```typescript
class Logger {
  trace(message: string, error?: QiError, context?: LoggerContext): void
  debug(message: string, error?: QiError, context?: LoggerContext): void
  info(message: string, error?: QiError, context?: LoggerContext): void
  warn(message: string, error?: QiError, context?: LoggerContext): void
  error(message: string, error?: QiError, context?: LoggerContext): void
  fatal(message: string, error?: QiError, context?: LoggerContext): void
  
  child(context: LoggerContext): Logger
  withContext(context: LoggerContext): Logger
  level: LogLevel
  config: LoggerConfig
}
```

## Type Definitions

```typescript
type LogLevel = 'trace' | 'debug' | 'info' | 'warn' | 'error' | 'fatal'

interface LogEntry {
  timestamp: Date
  level: LogLevel
  message: string
  error?: QiError
  context?: LoggerContext
  logger?: string
}

interface LoggerConfig {
  level: LogLevel
  name?: string
  pretty?: boolean
  destination?: 'stdout' | 'stderr' | string
  timestamp?: boolean
  context?: LoggerContext
}

type LoggerContext = Record<string, unknown>

interface LoggerEvents {
  log: (entry: LogEntry) => void
  error: (error: LoggerError) => void
}

interface LoggerError extends QiError {
  category: 'LOGGER'
  context: {
    level?: LogLevel
    message?: string
    destination?: string
  }
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

### createFromEnv(prefix?: string): Result<Logger, LoggerError>

Creates a logger from environment variables.

```typescript
// Uses LOG_LEVEL, LOG_NAME, LOG_PRETTY environment variables
const result = createFromEnv()

// Uses APP_LOG_LEVEL, APP_LOG_NAME, etc.
const result = createFromEnv('APP_')
```

## Utility Functions

### formatError(error: QiError): Record<string, unknown>

Formats QiError for structured logging.

```typescript
const formattedError = formatError(validationError('Invalid input'))
logger.error('Validation failed', undefined, { error: formattedError })
```

### createRequestLogger(logger: Logger): (req: Request, res: Response, next: NextFunction) => void

Creates Express.js middleware for request logging.

```typescript
import express from 'express'

const app = express()
const logger = createLogger({ level: 'info' }).value

app.use(createRequestLogger(logger))
```

## Error Factory

### loggerError(message: string, context?: Record<string, unknown>): LoggerError

Creates a logger-specific error.

```typescript
const error = loggerError('Failed to write log', { 
  destination: '/var/log/app.log',
  error: 'EACCES: permission denied'
})
```

## Configuration Presets

### loggerDevelopmentConfig: LoggerConfig

Development-friendly configuration.

```typescript
const config = loggerDevelopmentConfig
// { level: 'debug', pretty: true, timestamp: true }
```

### loggerProductionConfig: LoggerConfig

Production-optimized configuration.

```typescript
const config = loggerProductionConfig
// { level: 'info', pretty: false, timestamp: true }
```

### loggerTestConfig: LoggerConfig

Testing environment configuration.

```typescript
const config = loggerTestConfig
// { level: 'warn', pretty: false, timestamp: false }
```

### getLoggerEnvironmentConfig(): LoggerConfig

Gets configuration based on NODE_ENV.

```typescript
const config = getLoggerEnvironmentConfig()
// Returns development, production, or test config based on NODE_ENV
```

## Usage Examples

### Basic Logging

```typescript
import { createLogger } from '@qi/qicore-foundation'

const loggerResult = createLogger({ 
  level: 'info',
  name: 'app',
  pretty: true
})

if (loggerResult.tag === 'success') {
  const logger = loggerResult.value
  
  logger.info('Application started')
  logger.warn('This is a warning', undefined, { userId: '123' })
  logger.error('An error occurred', validationError('Invalid input'))
}
```

### Contextual Logging

```typescript
const logger = createLogger({ level: 'info' }).value
const requestLogger = logger.withContext({ 
  requestId: 'req_123', 
  userId: 'user_456' 
})

requestLogger.info('Processing request')
// Logs with requestId and userId automatically included

const childLogger = requestLogger.child({ operation: 'validation' })
childLogger.debug('Validating input')
// Logs with requestId, userId, and operation
```

### Error Logging with QiError

```typescript
const result = await processData(input)

match(
  data => logger.info('Data processed successfully', undefined, { 
    recordCount: data.length 
  }),
  error => logger.error('Data processing failed', error, {
    inputSize: input.length,
    operation: 'processData'
  }),
  result
)
```

### Request Logging Middleware

```typescript
import express from 'express'
import { createLogger, createRequestLogger } from '@qi/qicore-foundation'

const app = express()
const logger = createLogger({ level: 'info' }).value

// Automatic request/response logging
app.use(createRequestLogger(logger))

app.get('/users/:id', (req, res) => {
  const requestLogger = logger.withContext({ 
    requestId: req.headers['x-request-id'],
    userId: req.params.id
  })
  
  requestLogger.info('Fetching user')
  // Business logic...
  requestLogger.info('User fetched successfully')
})
```

### Structured Error Context

```typescript
const logger = createLogger({ level: 'info' }).value

try {
  await database.connect()
} catch (err) {
  const dbError = systemError('Database connection failed', {
    host: 'localhost',
    port: 5432,
    database: 'myapp'
  })
  
  logger.error('Failed to connect to database', dbError, {
    retryAttempt: 3,
    nextRetryIn: 5000
  })
}
```

### Environment-Based Configuration

```typescript
// Automatically configures based on NODE_ENV
const config = getLoggerEnvironmentConfig()
const logger = createLogger(config).value

// In development: debug level, pretty printed
// In production: info level, JSON format
// In test: warn level, minimal output
```

### Custom Destinations

```typescript
const logger = createLogger({
  level: 'info',
  destination: '/var/log/app.log'
}).value

// Or to stderr
const errorLogger = createLogger({
  level: 'error',
  destination: 'stderr'
}).value
```

## Integration with Result<T>

The logger integrates seamlessly with Result<T> patterns:

```typescript
const logger = createLogger({ level: 'info' }).value

const processResult = await processOrder(orderData)

// Log based on Result<T> outcome
match(
  order => logger.info('Order processed', undefined, { 
    orderId: order.id,
    amount: order.total 
  }),
  error => logger.error('Order processing failed', error, {
    orderId: orderData.id,
    stage: error.context?.stage
  }),
  processResult
)
```

## Performance Considerations

- Logging is synchronous by default for reliability
- Structured context is merged efficiently
- Child loggers share configuration and minimize overhead
- Pretty printing is disabled automatically in production presets
- Log levels filter messages at the logger level (not per call)

## Error Handling

Logger operations return Result<T> for creation but log calls are fire-and-forget:

```typescript
// Factory functions return Result<T>
const loggerResult = createLogger(config)
if (loggerResult.tag === 'failure') {
  console.error('Failed to create logger:', loggerResult.error.message)
}

// Log calls are fire-and-forget (no return value)
logger.info('This always succeeds')
```

## Best Practices

1. **Create once, use everywhere**: Create logger at application startup
2. **Use child loggers**: Add context with `withContext()` or `child()`
3. **Log errors with QiError**: Preserve error structure and context
4. **Use appropriate levels**: trace/debug for development, info+ for production
5. **Add meaningful context**: Include relevant business data
6. **Don't log sensitive data**: Avoid passwords, tokens, personal data
7. **Use environment configs**: Different settings for dev/prod/test
8. **Log Result<T> outcomes**: Always log both success and failure cases