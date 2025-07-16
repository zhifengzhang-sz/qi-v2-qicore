# QiCore Foundation Example Applications

This directory contains comprehensive example applications demonstrating how to use QiCore Foundation in real-world scenarios.

## Examples Overview

### 1. Basic Result Usage (`basic-result/`)
Simple examples showing fundamental Result<T> operations:
- Creating and handling Results
- Basic transformations and error handling
- Pattern matching and safe extraction

### 2. Error Handling (`error-handling/`)
Comprehensive error management examples:
- Structured error creation with QiError
- Error chaining and context
- Validation patterns and aggregate errors

### 3. Web Service (`web-service/`)
Complete web service implementation:
- Configuration management
- Structured logging
- Cache integration
- Request/response handling with Result<T>

### 4. CLI Tool (`cli-tool/`)
Command-line application example:
- Configuration from files and environment
- Logging setup for CLI applications
- File processing with error handling

### 5. Microservice (`microservice/`)
Microservice architecture patterns:
- Service composition
- Circuit breaker implementation
- Retry patterns and resilience
- OpenTelemetry integration

## Running the Examples

Each example directory contains:
- `README.md` - Detailed explanation and usage
- `package.json` - Dependencies and scripts
- `src/` - Source code
- `tests/` - Test files (where applicable)

### Prerequisites

- Node.js 18+ or Bun 1.0+
- TypeScript 5.8+

### Setup

```bash
# Navigate to any example directory
cd web-service

# Install dependencies
bun install

# Run the example
bun run dev

# Or with npm
npm install
npm run dev
```

## Key Learning Points

### Result<T> Patterns
- Functional error handling without exceptions
- Composable operations with map/flatMap
- Safe value extraction and pattern matching

### Error Management
- Structured errors with QiError
- Error categorization and retry strategies
- Context preservation and error chaining

### Infrastructure Integration
- Configuration management with validation
- Structured logging with context
- Caching strategies (memory and Redis)

### Real-World Patterns
- Service composition and dependency injection
- Retry and circuit breaker patterns
- API integration with proper error handling
- Testing strategies with Result<T>

## Example Architecture

Most examples follow this structure:

```
src/
├── config/          # Configuration management
├── services/        # Business logic services
├── utils/           # Utility functions
├── types/           # TypeScript type definitions
└── index.ts         # Application entry point
```

## Common Patterns Demonstrated

### 1. Service Layer Pattern
```typescript
class UserService {
  async getUser(id: number): Promise<Result<User>> {
    // Implementation with Result<T>
  }
}
```

### 2. Configuration Pattern
```typescript
const config = ConfigBuilder.create()
  .fromFile('config.json')
  .fromEnv()
  .build()
```

### 3. Logging Pattern
```typescript
const logger = createLogger({
  name: 'app',
  level: 'info'
})
```

### 4. Cache Pattern
```typescript
const cache = createCache({
  backend: 'redis',
  redis: { host: 'localhost', port: 6379 }
})
```

## Testing Examples

Each example includes comprehensive tests showing:
- Unit testing with Result<T>
- Error scenario testing
- Integration testing patterns
- Mock and stub strategies

## Next Steps

1. Start with [basic-result](./basic-result/) for fundamental concepts
2. Progress to [error-handling](./error-handling/) for error management
3. Explore [web-service](./web-service/) for complete application patterns
4. Review [microservice](./microservice/) for advanced architectural patterns

## Contributing

Feel free to contribute additional examples or improvements to existing ones. Each example should:
- Follow QiCore Foundation best practices
- Include comprehensive documentation
- Provide clear setup instructions
- Include relevant tests