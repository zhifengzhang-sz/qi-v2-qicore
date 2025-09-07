# Project Setup Guide

**How to use QiCore Foundation in your TypeScript project.**

## Important Note

`@qi/base` and `@qi/core` are **workspace packages** in this repository, **not published to npm**. They must be used locally via file dependencies or workspace setup.

### Option 1: Use as Workspace Dependencies (Recommended for Development)

If your project is inside this workspace, use workspace dependencies:

```json
{
  "dependencies": {
    "@qi/base": "workspace:*",
    "@qi/core": "workspace:*"
  }
}
```

### Option 2: External Projects (Use Built Packages)

For projects outside this workspace, you need to build the packages first, then reference them:

#### Step 1: Build QiCore packages
```bash
# In the QiCore typescript directory
cd /path/to/qi-v2-qicore/typescript
bun run build
```

This creates distributable packages in:
- `lib/base/dist/` - Contains `.js`, `.cjs`, and `.d.ts` files
- `lib/core/dist/` - Contains `.js`, `.cjs`, and `.d.ts` files

#### Step 2: Add file dependencies to your project
```bash
# In your external project (replace with actual absolute paths)
bun add file:/absolute/path/to/qi-v2-qicore/typescript/lib/base
bun add file:/absolute/path/to/qi-v2-qicore/typescript/lib/core
```

Or in your project's package.json:

```json
{
  "dependencies": {
    "@qi/base": "file:/absolute/path/to/qi-v2-qicore/typescript/lib/base",
    "@qi/core": "file:/absolute/path/to/qi-v2-qicore/typescript/lib/core"
  }
}
```

#### Step 3: Install dependencies
```bash
bun install
```

**Important Notes:**
- You must run `bun run build` in QiCore **before** adding file dependencies
- Use **absolute paths** (not relative paths like `../qi-v2-qicore/...`)
- Rebuild QiCore packages when you pull updates from git

## Quick Start

### Prerequisites

Before using QiCore, ensure you have the required services running:

```bash
# Start Redis (required for @qi/core cache integration tests)
cd ../../services
docker compose up -d redis

# Verify Redis is running on localhost:6379
docker ps | grep redis
```

### 1. Create Your Project in the Workspace (Recommended)

The easiest way is to create your project inside the `app/` directory:

```bash
# From the QiCore typescript directory
mkdir app/my-project
cd app/my-project

# Initialize package.json with workspace dependencies
cat > package.json << 'EOF'
{
  "name": "my-project",
  "version": "1.0.0",
  "type": "module",
  "dependencies": {
    "@qi/base": "workspace:*",
    "@qi/core": "workspace:*"
  }
}
EOF

# Install dependencies (from the typescript root)
cd ../..
bun install
```

### 2. Import and Use

```typescript
// Import from @qi/base for Result<T> and error handling
import { success, failure, Result, QiError } from '@qi/base'
import { map, flatMap, match } from '@qi/base'

// Import from @qi/core for infrastructure services
import { ConfigBuilder, fromEnv } from '@qi/core'
import { createLogger } from '@qi/core'
import { createCache } from '@qi/core'

// Example usage
const config = fromEnv('APP')
const logger = createLogger({ level: 'info' })

const result: Result<string, QiError> = success("Hello QiCore!")
const mapped = map((value) => value.toUpperCase(), result)
```

## Package Structure

QiCore Foundation consists of two separate packages:

### `@qi/base` - Mathematical Foundation
Core types and functional patterns:
- `Result<T, E>` - Type-safe error handling
- `QiError` - Structured error types  
- Monadic operations (`map`, `flatMap`, `match`, etc.)
- Async helpers for `Promise<Result<T>>`

### `@qi/core` - Infrastructure Services
Practical tools that use Result<T> patterns:
- **Config** - Multi-source configuration loading
- **Logger** - Structured logging with context
- **Cache** - Memory and Redis caching

## TypeScript Configuration

### Basic tsconfig.json

For most projects, standard TypeScript configuration works:

```json
{
  "compilerOptions": {
    "target": "ES2023",
    "lib": ["ES2023"],
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "strict": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
```

### For Node.js Projects

Add to your `package.json`:

```json
{
  "type": "module",
  "engines": {
    "node": ">=18.0.0"
  }
}
```

### For CommonJS Projects

If you need CommonJS, both packages provide `.cjs` exports:

```typescript
// Works in CommonJS
const { success, failure } = require('@qi/base')
const { createLogger } = require('@qi/core')
```

## Example Project Setup

Here's a complete example of using QiCore in a real project:

### package.json (Workspace Project)
```json
{
  "name": "my-app",
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "dev": "tsx src/index.ts",
    "build": "tsc",
    "start": "node dist/index.js"
  },
  "dependencies": {
    "@qi/base": "workspace:*",
    "@qi/core": "workspace:*"
  },
  "devDependencies": {
    "typescript": "^5.8.3",
    "tsx": "^4.19.2"
  }
}
```

### package.json (External Project)
```json
{
  "name": "my-app",
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "dev": "tsx src/index.ts",
    "build": "tsc",
    "start": "node dist/index.js"
  },
  "dependencies": {
    "@qi/base": "file:/absolute/path/to/qi-v2-qicore/typescript/lib/base",
    "@qi/core": "file:/absolute/path/to/qi-v2-qicore/typescript/lib/core"
  },
  "devDependencies": {
    "typescript": "^5.8.3",
    "tsx": "^4.19.2"
  }
}
```

### src/index.ts
```typescript
import { success, failure, match } from '@qi/base'
import { ConfigBuilder, createLogger } from '@qi/core'

async function main() {
  // Load configuration
  const configResult = await ConfigBuilder
    .fromEnv('APP')
    .build()

  await match(
    async (config) => {
      // Initialize logger
      const loggerResult = createLogger({ 
        level: 'info',
        pretty: true 
      })
      
      await match(
        (logger) => {
          logger.info('Application started successfully!', { 
            port: config.getOr('port', 3000) 
          })
          // Your application logic here
        },
        (error) => {
          console.error('Logger failed:', error.message)
        },
        loggerResult
      )
    },
    (error) => {
      console.error('Config failed:', error.message)
    },
    configResult
  )
}

main()
```

## Advanced Usage

### Using Zod Schemas with Config

```typescript
import { z } from 'zod'
import { ConfigBuilder, validateConfig } from '@qi/core'

const AppSchema = z.object({
  port: z.coerce.number().default(3000),
  debug: z.coerce.boolean().default(false),
  database: z.object({
    host: z.string(),
    port: z.coerce.number().default(5432)
  })
})

async function loadConfig() {
  const configResult = await ConfigBuilder
    .fromYamlFile('config.yaml')
    .merge(ConfigBuilder.fromEnv('APP'))
    .build()
    
  return match(
    (config) => validateConfig(config, AppSchema),
    (error) => failure(error),
    configResult
  )
}
```

### Error Handling Patterns

```typescript
import { Result, map, flatMap, match } from '@qi/base'

// Chain operations that might fail
const processUser = (userId: string): Result<User, QiError> =>
  validateUserId(userId)
    |> flatMap(fetchUser)
    |> flatMap(enrichUserData)
    |> map(formatUser)

// Handle the result
match(
  (user) => console.log('Success:', user),
  (error) => console.error('Failed:', error.message),
  processUser('123')
)
```

## Publishing Your Own QiCore-Style Package

If you want to create packages with similar architecture:

1. **Use workspace structure** like QiCore
2. **Publish separate packages** (`@yourname/base`, `@yourname/core`)
3. **Follow Result<T> patterns** for consistency
4. **Provide TypeScript declarations** for full type safety

See the QiCore source code structure as a reference implementation.

## Validation

### Test Your QiCore Setup

After setup, validate everything works:

```bash
# In workspace projects
cd qi-v2-qicore/typescript
bun run check  # Runs typecheck + format + lint + tests

# In external projects  
cd your-project
bun run build  # Should compile without errors
```

### Running QiCore Tests

To run the full QiCore test suite:

```bash
cd qi-v2-qicore/typescript

# Make sure Redis is running first
cd ../../services
docker compose up -d redis

# Run tests
cd ../typescript
bun run test           # Basic tests
bun run test:coverage  # With coverage report
bun run check          # Full validation
```

## Troubleshooting

### "Cannot find module '@qi/base'"

1. **Workspace projects**: Run `bun install` from the typescript root
2. **External projects**: Verify file paths are absolute and correct
3. **File dependencies**: Make sure you ran `bun run build` in QiCore first
4. Check Node.js version: `node --version` (requires >=18)
5. Clear cache: `bun pm cache rm`

### "Type errors with Result<T>"

1. Enable strict TypeScript: `"strict": true`
2. Use explicit types: `const result: Result<string, QiError> = success("value")`
3. Handle both success and failure cases with `match()`

### "ESM import issues"

1. Add `"type": "module"` to package.json
2. Use `.js` extensions in imports if required by your setup
3. Check `moduleResolution: "bundler"` in tsconfig.json

## Next Steps

Once you have QiCore installed:

1. **Learn the Foundation**: Read [qi/base tutorial](./qi-base.md) 
2. **Add Configuration**: Learn [Config tool](./qi-core-config.md)
3. **Add Logging**: Learn [Logger tool](./qi-core-logger.md)
4. **Add Caching**: Learn [Cache tool](./qi-core-cache.md)

The tutorial will guide you through using Result<T> patterns and infrastructure tools effectively.