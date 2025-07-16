# Installation and Setup

## Installation

QiCore Foundation is available as a single npm package with modular exports:

```bash
# Using npm
npm install @qi/qicore-foundation

# Using yarn
yarn add @qi/qicore-foundation

# Using bun
bun add @qi/qicore-foundation

# Using pnpm
pnpm add @qi/qicore-foundation
```

## Package Structure

QiCore Foundation provides two main modules:

- **`@qi/qicore-foundation/base`** - Mathematical foundation types (Result<T>, QiError)
- **`@qi/qicore-foundation`** - Complete package (base + core infrastructure)

## Import Patterns

### Option 1: Modular Imports (Recommended)

```typescript
// Import only base types for lightweight usage
import { Ok, Err, type Result, type QiError } from '@qi/qicore-foundation/base'

// Import specific core services
import { createLogger, createCache, ConfigBuilder } from '@qi/qicore-foundation'
```

### Option 2: Full Package Import

```typescript
// Import everything from the main package
import { 
  Ok, Err, Result, QiError,
  createLogger, createCache, ConfigBuilder 
} from '@qi/qicore-foundation'
```

### Option 3: Namespace Imports

```typescript
// Namespace imports for cleaner organization
import * as Qi from '@qi/qicore-foundation/base'
import * as QiCore from '@qi/qicore-foundation'

const result: Qi.Result<number> = Qi.Ok(42)
const logger = QiCore.createLogger({ name: 'app' })
```

## TypeScript Configuration

### Required TypeScript Settings

Add these settings to your `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2023",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": false,
    "verbatimModuleSyntax": true,
    "strict": true,
    "skipLibCheck": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "lib": ["ES2023", "DOM"]
  }
}
```

### Path Aliases (Optional but Recommended)

Set up path aliases for cleaner imports:

```json
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@qi/base": ["node_modules/@qi/qicore-foundation/dist/base"],
      "@qi/core": ["node_modules/@qi/qicore-foundation/dist/index"]
    }
  }
}
```

With aliases, you can import like:

```typescript
import { Ok, Err, Result } from '@qi/base'
import { createLogger, createCache } from '@qi/core'
```

## Runtime Requirements

### Node.js Environment

QiCore Foundation requires:
- **Node.js 18.0.0** or higher
- **ES2023** support
- **ESM** module system

### Bun Environment

For Bun users:
- **Bun 1.0.0** or higher
- Native TypeScript support
- Better performance for development

## Development Setup

### 1. Project Structure

```
your-project/
├── src/
│   ├── config/
│   ├── services/
│   ├── types/
│   └── index.ts
├── examples/
├── tests/
├── package.json
└── tsconfig.json
```

### 2. Package.json Configuration

```json
{
  "name": "your-project",
  "type": "module",
  "engines": {
    "node": ">=18.0.0"
  },
  "scripts": {
    "dev": "tsx watch src/index.ts",
    "build": "tsc",
    "test": "vitest",
    "typecheck": "tsc --noEmit"
  },
  "dependencies": {
    "@qi/qicore-foundation": "^0.3.4"
  },
  "devDependencies": {
    "typescript": "^5.8.0",
    "tsx": "^4.0.0",
    "vitest": "^1.6.0"
  }
}
```

### 3. Basic Setup Example

Create `src/index.ts`:

```typescript
import { Ok, Err, type Result } from '@qi/qicore-foundation/base'
import { createLogger, createCache } from '@qi/qicore-foundation'

// Initialize core services
const logger = createLogger({ name: 'app' })
const cache = createCache({ backend: 'memory', maxSize: 1000 })

// Your first Result operation
function divide(a: number, b: number): Result<number> {
  if (b === 0) {
    return Err(new Error('Division by zero'))
  }
  return Ok(a / b)
}

// Usage
const result = divide(10, 2)
if (result.tag === 'success') {
  logger.info('Division successful', { result: result.value })
} else {
  logger.error('Division failed', { error: result.error })
}
```

## Environment Configuration

### Development Environment

```bash
# .env.development
NODE_ENV=development
LOG_LEVEL=debug
CACHE_BACKEND=memory
```

### Production Environment

```bash
# .env.production
NODE_ENV=production
LOG_LEVEL=info
CACHE_BACKEND=redis
REDIS_HOST=localhost
REDIS_PORT=6379
```

### Environment Loading

```typescript
import { fromEnv } from '@qi/qicore-foundation'

const config = fromEnv({
  NODE_ENV: process.env.NODE_ENV || 'development',
  LOG_LEVEL: process.env.LOG_LEVEL || 'info',
  CACHE_BACKEND: process.env.CACHE_BACKEND || 'memory'
})
```

## Docker Setup

### Basic Dockerfile

```dockerfile
FROM node:18-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./
RUN npm ci --only=production

# Copy source code
COPY src ./src
COPY tsconfig.json ./

# Build the application
RUN npm run build

# Run the application
CMD ["node", "dist/index.js"]
```

### Docker Compose with Redis

```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
      - CACHE_BACKEND=redis
      - REDIS_HOST=redis
    depends_on:
      - redis

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
```

## Verification

Test your setup with this verification script:

```typescript
// verify-setup.ts
import { Ok, Err, map, type Result } from '@qi/qicore-foundation/base'
import { createLogger, createCache } from '@qi/qicore-foundation'

async function verifySetup() {
  console.log('🔍 Verifying QiCore Foundation setup...')
  
  // Test Result operations
  const numberResult: Result<number> = Ok(42)
  const doubled = map(numberResult, x => x * 2)
  
  console.log('✅ Result<T> operations working:', doubled)
  
  // Test logger
  const logger = createLogger({ name: 'verification' })
  logger.info('Logger initialized successfully')
  
  // Test cache
  const cache = createCache({ backend: 'memory', maxSize: 100 })
  await cache.set('test', 'value')
  const cached = await cache.get('test')
  
  console.log('✅ Cache operations working:', cached)
  
  console.log('🎉 QiCore Foundation setup complete!')
}

verifySetup().catch(console.error)
```

Run the verification:

```bash
# With tsx
npx tsx verify-setup.ts

# With bun
bun run verify-setup.ts
```

## Common Issues and Solutions

### Issue: Module Resolution Errors

**Problem**: `Cannot find module '@qi/qicore-foundation/base'`

**Solution**: Ensure your `tsconfig.json` has `"moduleResolution": "bundler"` and install the latest version.

### Issue: Type Errors with Result<T>

**Problem**: TypeScript can't infer Result types properly

**Solution**: Use explicit type annotations:

```typescript
// Instead of
const result = someFunction()

// Use
const result: Result<MyType> = someFunction()
```

### Issue: ESM Import Errors

**Problem**: `require() is not defined` or similar ESM errors

**Solution**: Ensure your `package.json` has `"type": "module"` and use ESM imports consistently.

## Next Steps

Now that you have QiCore Foundation installed and configured, proceed to:

- [Understanding Result<T> - The Foundation](./02-result-basics.md)
- [Error Handling with QiError](./03-error-handling.md)

## Resources

- [API Documentation](../api/README.md)
- [Example Applications](../../app/README.md)
- [TypeScript Configuration Guide](https://www.typescriptlang.org/docs/handbook/tsconfig-json.html)