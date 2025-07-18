# Logger Implementation Guide

## Overview

The Logger module provides structured logging with Pino integration using functional patterns. It offers high-performance event-driven logging with OpenTelemetry integration and comprehensive context management.

## Design Approach

**Simple immutable records with pure functions**

### Rationale
- Logging is a side effect, not a transformation
- Simple console backend for zero dependencies
- Context accumulation through immutable updates
- Performance-conscious level checking

### Key Decisions
- Logger is just data (level + context)
- Logging functions are effects (return void)
- No async logging (keep it simple)
- Structured output by default

## Core Interface

### Logger Configuration
```typescript
export interface LoggerConfig {
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
```

### Logger Instance
```typescript
export class Logger {
  debug(message: string, context?: LoggerContext): void
  info(message: string, context?: LoggerContext): void
  warn(message: string, context?: LoggerContext): void
  error(message: string, error?: Error, context?: LoggerContext): void
  fatal(message: string, error?: Error, context?: LoggerContext): void
  
  isLevelEnabled(level: LogLevel): boolean
  getLevel(): LogLevel
  setLevel(level: LogLevel): void
  
  child(context: LoggerContext): Logger
  getConfig(): LoggerConfig
  
  on<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  once<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  off<K extends keyof LoggerEvents>(event: K, listener: LoggerEvents[K]): void
  
  flush(): Promise<void>
  close(): void
}
```

### Log Levels
```typescript
export type LogLevel = 'debug' | 'info' | 'warn' | 'error' | 'fatal'

// Log levels in order of severity
// DEBUG < INFO < WARN < ERROR < FATAL
```

### Log Level Usage Guidelines

#### When to Use Each Level

- **DEBUG**: Development debugging information, detailed execution flow
- **INFO**: Normal application behavior, important events, startup/shutdown
- **WARN**: Unexpected conditions that don't stop processing, deprecated usage
- **ERROR**: Error conditions that affect specific operations but application continues
- **FATAL**: Critical system failures that prevent application from continuing

#### Fatal Level Examples
```typescript
// System cannot start due to missing critical resources
logger.fatal('Database connection pool exhausted', error, {
  maxConnections: 10,
  activeConnections: 10,
  waitingRequests: 50
})

// Critical configuration missing
logger.fatal('Required environment variable missing', undefined, {
  variable: 'DATABASE_URL',
  component: 'startup',
  action: 'terminating_process'
})

// Unrecoverable system state
logger.fatal('Memory allocation failed', error, {
  requestedBytes: 1048576,
  availableMemory: 512,
  component: 'cache_manager'
})
```

## Factory Functions

### Core Factories
- `createLogger(config: LoggerConfig): Result<Logger, LoggerError>` - Create logger with configuration
- `createFromEnv(): Result<Logger, LoggerError>` - Create logger from environment variables

### Usage Examples

```typescript
// Basic logger
const loggerResult = createLogger({
  level: 'info',
  pretty: true
})

if (loggerResult.tag === 'success') {
  const logger = loggerResult.value
  logger.info('Application started')
}

// Production logger
const logger = createLogger({
  level: 'info',
  name: 'api-server',
  pretty: false,
  destination: './logs/app.log',
  redact: ['password', 'token', 'secret']
})

// Environment-based logger
const envLogger = createFromEnv()
```

## Contract Compliance

### Factory Operations
- `create: LoggerConfig → Result<Logger>` - ✅ Implemented

### Logging Operations
- `debug: Message → Context? → Logger → Effect<Void>` - ✅ Implemented
- `info: Message → Context? → Logger → Effect<Void>` - ✅ Implemented
- `warn: Message → Context? → Logger → Effect<Void>` - ✅ Implemented
- `error: Message → Error? → Context? → Logger → Effect<Void>` - ✅ Implemented

### Utility Operations
- `isLevelEnabled: LogLevel → Logger → Boolean` - ✅ Implemented
- `withContext: Context → Logger → Logger` - ✅ Implemented (as child)

## Pino Integration

### Max-Min Principle
The logger implementation follows the 70% pino package, 30% custom logic principle:

#### 70% Pino Package Usage
- Core logging functionality
- Level checking and filtering
- Output formatting and serialization
- Performance optimizations
- Transport system (pino-pretty, file outputs)

#### 30% Custom Logic
- Result<T> wrapper for logger creation
- Event system for custom handling
- QiCore-specific error types
- Child logger management
- Context merging logic

### Pino Features Leveraged
```typescript
// Pino's efficient level checking
if (logger.isLevelEnabled('debug')) {
  logger.debug('Expensive operation', { data: computeExpensiveData() })
}

// Pino's child logger functionality
const childLogger = logger.child({ requestId: '123' })
childLogger.info('Processing request') // Includes requestId in all logs

// Pino's serializers
const logger = createLogger({
  level: 'info',
  serializers: {
    error: (error) => ({
      message: error.message,
      stack: error.stack,
      name: error.name
    })
  }
})
```

## Structured Logging

### Log Entry Structure
```typescript
export interface LogEntry {
  readonly level: LogLevel
  readonly message: string
  readonly timestamp: Date
  readonly context?: Record<string, unknown>
  readonly error?: Error
  readonly traceId?: string
  readonly spanId?: string
}
```

### Context Management
```typescript
// Base logger
const logger = createLogger({ level: 'info' })

// Child logger with persistent context
const requestLogger = logger.child({
  requestId: '123',
  userId: 'user456'
})

// All subsequent logs include the context
requestLogger.info('User action', { action: 'login' })
// Output: { level: 'info', requestId: '123', userId: 'user456', action: 'login', message: 'User action' }
```

### Structured Data
```typescript
// Rich context logging
logger.info('Database query executed', {
  query: 'SELECT * FROM users',
  duration: 145,
  rows: 25,
  database: 'production'
})

// Error logging with context
logger.error('Database connection failed', error, {
  host: 'db.example.com',
  port: 5432,
  retryCount: 3
})

// Fatal logging for critical system failures
logger.fatal('System cannot continue', error, {
  component: 'database',
  operation: 'startup',
  criticalResource: 'primary_db_connection'
})
```

## Performance Features

### Level Checking
```typescript
// Efficient level checking prevents expensive operations
if (logger.isLevelEnabled('debug')) {
  logger.debug('Complex debug info', {
    expensiveData: computeComplexData(),
    state: getCurrentState()
  })
}
```

### Lazy Context Merging
```typescript
// Context is only merged when logging actually occurs
const logger = createLogger({ level: 'warn' })
const debugLogger = logger.child({ debugInfo: expensiveComputation() })

// expensiveComputation() is called, but logs are filtered out
debugLogger.debug('This will not be logged')
```

### Object Pooling
```typescript
// Minimal object allocation for high-frequency logging
for (let i = 0; i < 1000000; i++) {
  logger.info('Processing item', { index: i })
  // Optimized for minimal GC pressure
}
```

## Event System

### Logger Events
```typescript
export interface LoggerEvents {
  log: (entry: LogEntry) => void
  error: (error: LoggerError) => void
  level: (level: LogLevel) => void
}
```

### Event Handling
```typescript
// Monitor all log entries
logger.on('log', (entry) => {
  if (entry.level === 'error') {
    sendToErrorTracking(entry)
  }
})

// Handle logger errors
logger.on('error', (error) => {
  console.error('Logger error:', error)
})

// React to level changes
logger.on('level', (level) => {
  console.log('Log level changed to:', level)
})
```

## Error Handling

### Error Types
```typescript
export type LoggerError = QiError & {
  readonly category: 'LOGGER'
  readonly context: {
    readonly operation?: string
    readonly level?: LogLevel
    readonly logger?: string
  }
}
```

### Error Creation
```typescript
export const loggerError = (
  message: string, 
  context: LoggerError['context'] = {}
): LoggerError => 
  createError('LOGGER_ERROR', message, 'LOGGER', context) as LoggerError
```

### Error Handling Examples
```typescript
// Handle logger creation errors
const loggerResult = createLogger({ level: 'invalid' as LogLevel })

if (loggerResult.tag === 'failure') {
  console.error('Failed to create logger:', loggerResult.error.message)
}

// Handle runtime logging errors
logger.on('error', (error) => {
  // Logger failed to write to destination
  console.error('Logger error:', error.message)
})
```

## Configuration Options

### Common Configurations

#### Development Configuration
```typescript
export const developmentConfig: LoggerConfig = {
  level: 'debug',
  name: 'qicore-dev',
  pretty: true,
}

const devLogger = createLogger(developmentConfig)
```

#### Production Configuration
```typescript
export const productionConfig: LoggerConfig = {
  level: 'warn',
  name: 'qicore-prod',
  pretty: false,
  redact: ['password', 'token', 'secret', 'key', 'authorization'],
}

const prodLogger = createLogger(productionConfig)
```

#### Test Configuration
```typescript
export const testConfig: LoggerConfig = {
  level: 'fatal',
  name: 'qicore-test',
  pretty: false,
}

const testLogger = createLogger(testConfig)
```

### Environment-Based Configuration
```typescript
export const getEnvironmentConfig = (): LoggerConfig => {
  const env = process.env.NODE_ENV || 'development'
  const logLevel = (process.env.LOG_LEVEL as LogLevel) || undefined

  const baseConfig = (() => {
    switch (env) {
      case 'production':
        return productionConfig
      case 'test':
        return testConfig
      default:
        return developmentConfig
    }
  })()

  return logLevel ? { ...baseConfig, level: logLevel } : baseConfig
}
```

## Advanced Features

### Request Logger Helper
```typescript
export const createRequestLogger = (logger: Logger) => {
  return {
    logRequest: (
      req: { method?: string; url?: string; headers?: Record<string, string> },
      context?: LoggerContext
    ) => {
      logger.info('Request received', {
        method: req.method,
        url: req.url,
        userAgent: req.headers?.['user-agent'],
        ...context,
      })
    },

    logResponse: (
      req: { method?: string; url?: string },
      res: { statusCode?: number },
      duration: number,
      context?: LoggerContext
    ) => {
      const level = (res.statusCode ?? 200) >= 400 ? 'error' : 'info'
      const logContext = {
        method: req.method,
        url: req.url,
        statusCode: res.statusCode,
        duration,
        ...context,
      }

      if (level === 'error') {
        logger.error('Request completed', undefined, logContext)
      } else {
        logger[level]('Request completed', logContext)
      }
    },

    logError: (
      error: Error, 
      req?: { method?: string; url?: string }, 
      context?: LoggerContext
    ) => {
      logger.error('Request error', error, {
        method: req?.method,
        url: req?.url,
        ...formatError(error),
        ...context,
      })
    },
  }
}
```

### Error Formatting
```typescript
export const formatError = (error: Error): Record<string, unknown> => {
  const result: Record<string, unknown> = {
    name: error.name,
    message: error.message,
    stack: error.stack,
  }

  if (error.cause) {
    result.cause = error.cause
  }

  return result
}
```

## Integration Examples

### Express Application
```typescript
import express from 'express'
import { createLogger, createRequestLogger } from '@qi/core'

const app = express()
const logger = createLogger({ level: 'info', pretty: true }).value
const requestLogger = createRequestLogger(logger)

// Request logging middleware
app.use((req, res, next) => {
  const start = Date.now()
  requestLogger.logRequest(req, { requestId: req.id })
  
  res.on('finish', () => {
    const duration = Date.now() - start
    requestLogger.logResponse(req, res, duration)
  })
  
  next()
})

// Error handling middleware
app.use((error, req, res, next) => {
  requestLogger.logError(error, req)
  res.status(500).json({ error: 'Internal server error' })
})
```

### Service Layer
```typescript
class UserService {
  private logger: Logger

  constructor(logger: Logger) {
    this.logger = logger.child({ service: 'user' })
  }

  async createUser(userData: CreateUserRequest): Promise<Result<User, UserError>> {
    const start = Date.now()
    this.logger.info('Creating user', { email: userData.email })

    try {
      const user = await this.userRepository.create(userData)
      const duration = Date.now() - start
      
      this.logger.info('User created successfully', {
        userId: user.id,
        email: user.email,
        duration
      })

      return success(user)
    } catch (error) {
      this.logger.error('Failed to create user', error, {
        email: userData.email,
        duration: Date.now() - start
      })
      
      return failure(userError('Failed to create user', { email: userData.email }))
    }
  }
}
```

### Background Jobs
```typescript
class JobProcessor {
  private logger: Logger

  constructor(logger: Logger) {
    this.logger = logger.child({ component: 'job-processor' })
  }

  async processJob(job: Job): Promise<void> {
    const jobLogger = this.logger.child({ 
      jobId: job.id, 
      jobType: job.type 
    })

    jobLogger.info('Starting job processing')
    
    try {
      await job.execute()
      jobLogger.info('Job completed successfully')
    } catch (error) {
      jobLogger.error('Job failed', error)
      throw error
    }
  }
}
```

## Testing

### Unit Testing
```typescript
describe('Logger', () => {
  it('should create logger with valid config', () => {
    const result = createLogger({ level: 'info' })
    expect(result.tag).toBe('success')
  })

  it('should reject invalid log level', () => {
    const result = createLogger({ level: 'invalid' as LogLevel })
    expect(result.tag).toBe('failure')
  })

  it('should create child logger with context', () => {
    const logger = createLogger({ level: 'info' }).value
    const child = logger.child({ requestId: '123' })
    
    // Child logger should include parent context
    expect(child.getConfig().name).toBe(logger.getConfig().name)
  })
})
```

### Integration Testing
```typescript
describe('Logger Integration', () => {
  it('should log to file', async () => {
    const logFile = './test-logs.log'
    const logger = createLogger({
      level: 'info',
      destination: logFile
    }).value

    logger.info('Test message')
    await logger.flush()

    const logContent = await fs.readFile(logFile, 'utf-8')
    expect(logContent).toContain('Test message')
  })

  it('should emit events', (done) => {
    const logger = createLogger({ level: 'info' }).value
    
    logger.on('log', (entry) => {
      expect(entry.message).toBe('Test message')
      done()
    })

    logger.info('Test message')
  })
})
```

## Best Practices

### Performance Optimization
- Use level checking for expensive operations
- Leverage child loggers for context
- Avoid synchronous logging in production
- Use structured logging consistently

### Security Considerations
- Always redact sensitive information
- Use appropriate log levels for different environments
- Secure log file permissions
- Implement log rotation

### Monitoring and Observability
- Set up log aggregation (ELK, Splunk)
- Monitor log volume and patterns
- Alert on error rates and patterns
- Use distributed tracing integration

### Error Handling
- Never let logging errors crash your application
- Always handle logger creation failures
- Use fallback logging strategies
- Monitor logger health

## Troubleshooting

### Common Issues
1. **Performance degradation**: Check log level and volume
2. **Memory leaks**: Ensure proper logger cleanup
3. **Missing logs**: Verify log level and destination
4. **Formatting issues**: Check serializers and pretty printing

### Debug Information
```typescript
// Enable debug logging
const logger = createLogger({ level: 'debug' }).value

// Log logger configuration
console.log('Logger config:', logger.getConfig())

// Monitor logger events
logger.on('log', (entry) => {
  console.log('Log entry:', entry)
})

logger.on('error', (error) => {
  console.error('Logger error:', error)
})
```