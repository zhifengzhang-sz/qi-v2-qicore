# QiCore Foundation Design Improvements Proposal

## Overview

This document outlines comprehensive improvements to QiCore Foundation based on documentation writing experience and usage analysis.

## 🔥 **Critical Issues to Fix**

### 1. Import Path Aliasing

**Current Problem:**
```typescript
// Unwieldy and inconsistent
import { Ok, Err } from '@qi/qicore-foundation/base'
import { createLogger } from '@qi/qicore-foundation'
```

**Proposed Solution:**
```typescript
// Clean, intuitive aliasing
import { Ok, Err, Result } from '@qi/base'
import { createLogger, createCache } from '@qi/core'
import { Config, Logger, Cache } from '@qi/core'
```

**Implementation:**
- Update package.json exports to support `@qi/base` and `@qi/core`
- Maintain backward compatibility with current imports
- Update all documentation and examples

### 2. Missing Result Utilities

**Current Problem:**
```typescript
// Verbose pattern matching
if (result.tag === 'success') {
  console.log(result.value)
} else {
  console.error(result.error)
}
```

**Proposed Solution:**
```typescript
// Convenient utilities
import { isOk, isErr, expect, expectSuccess, expectFailure } from '@qi/base'

const value = isOk(result) ? result.value : defaultValue
const safeValue = expect(result, 'Expected success')
```

### 3. Async Composition Improvements

**Current Problem:**
```typescript
// Nested async chains are unwieldy
const result = await asyncAndThen(userResult, async user => {
  const profile = await fetchProfile(user.id)
  return asyncAndThen(profile, async profileData => {
    return Ok({ user, profile: profileData })
  })
})
```

**Proposed Solution:**
```typescript
// Async pipe utility
const result = await asyncPipe(
  fetchUser,
  user => fetchProfile(user.id),
  profile => fetchSettings(profile.userId)
)(userId)

// Or async do-notation style
const result = await asyncDo(function* () {
  const user = yield* fetchUser(userId)
  const profile = yield* fetchProfile(user.id)
  const settings = yield* fetchSettings(user.id)
  return { user, profile, settings }
})
```

## 📋 **Detailed Implementation Plan**

### Phase 1: Core Utilities (High Priority)

#### 1.1 Result Utilities
```typescript
// Add to @qi/base
export const isOk = <T>(result: Result<T>): result is Success<T> => 
  result.tag === 'success'

export const isErr = <T>(result: Result<T>): result is Failure => 
  result.tag === 'failure'

export const expect = <T>(result: Result<T>, message?: string): T => {
  if (result.tag === 'success') return result.value
  throw new Error(message || `Expected success but got: ${result.error}`)
}

export const expectSuccess = <T>(result: Result<T>): T => {
  if (result.tag === 'success') return result.value
  throw new Error(`Expected success but got error: ${result.error}`)
}

export const expectFailure = <T>(result: Result<T>): QiError => {
  if (result.tag === 'failure') return result.error
  throw new Error(`Expected failure but got success: ${result.value}`)
}
```

#### 1.2 Async Composition
```typescript
// Add to @qi/base
export const asyncPipe = <T>(...fns: AsyncResultFunction<any, any>[]) => 
  async (input: T): Promise<Result<any>> => {
    let current: Result<any> = Ok(input)
    
    for (const fn of fns) {
      if (current.tag === 'failure') break
      current = await fn(current.value)
    }
    
    return current
  }

export const asyncDo = async function<T>(
  generator: () => AsyncGenerator<Result<any>, T>
): Promise<Result<T>> {
  const gen = generator()
  let current = await gen.next()
  
  while (!current.done) {
    if (current.value.tag === 'failure') {
      return current.value
    }
    current = await gen.next(current.value.value)
  }
  
  return Ok(current.value)
}
```

#### 1.3 Collection Utilities
```typescript
// Add to @qi/base
export const combine3 = <T, U, V>(
  r1: Result<T>, 
  r2: Result<U>, 
  r3: Result<V>
): Result<[T, U, V]> => {
  if (r1.tag === 'failure') return r1
  if (r2.tag === 'failure') return r2
  if (r3.tag === 'failure') return r3
  return Ok([r1.value, r2.value, r3.value])
}

export const combineN = <T>(results: Result<T>[]): Result<T[]> => 
  sequence(results)

export const asyncTraverse = <T, U>(
  items: T[], 
  fn: (item: T) => Promise<Result<U>>
): Promise<Result<U[]>> => 
  asyncSequence(items.map(fn))
```

### Phase 2: Testing Support (High Priority)

#### 2.1 Jest Matchers
```typescript
// Add to @qi/testing (new package)
declare global {
  namespace jest {
    interface Matchers<R> {
      toBeSuccess(expected?: any): R
      toBeFailure(expected?: any): R
      toBeSuccessWithValue(expected: any): R
      toBeFailureWithError(expected: any): R
    }
  }
}

export const matchers = {
  toBeSuccess(received: Result<any>) {
    const pass = received.tag === 'success'
    return {
      pass,
      message: () => pass 
        ? `Expected failure but got success: ${received.value}`
        : `Expected success but got failure: ${received.error}`
    }
  },
  // ... other matchers
}
```

#### 2.2 Test Utilities
```typescript
// Add to @qi/testing
export const mockResult = <T>(value: T): Result<T> => Ok(value)
export const mockError = (error: QiError): Result<never> => Err(error)
export const resultSpy = <T>(fn: () => Result<T>) => jest.fn(fn)
```

### Phase 3: Framework Integration (Medium Priority)

#### 3.1 Express Integration
```typescript
// Add to @qi/express (new package)
export const resultHandler = <T>(
  fn: (req: Request, res: Response) => Promise<Result<T>>
) => async (req: Request, res: Response, next: NextFunction) => {
  try {
    const result = await fn(req, res)
    
    if (result.tag === 'success') {
      res.json(result.value)
    } else {
      const status = getStatusFromError(result.error)
      res.status(status).json({
        error: result.error.message,
        code: result.error.code
      })
    }
  } catch (error) {
    next(error)
  }
}
```

#### 3.2 Fastify Integration
```typescript
// Add to @qi/fastify (new package)
export const resultPlugin = (fastify: FastifyInstance) => {
  fastify.decorateReply('result', function<T>(result: Result<T>) {
    if (result.tag === 'success') {
      return this.send(result.value)
    } else {
      const status = getStatusFromError(result.error)
      return this.code(status).send({
        error: result.error.message,
        code: result.error.code
      })
    }
  })
}
```

### Phase 4: Type Safety Improvements (Low Priority)

#### 4.1 Generic Error Types
```typescript
// Add to @qi/base
export interface ValidationError<T = unknown> extends QiError {
  category: 'VALIDATION'
  context: {
    field: string
    value: T
    constraints: string[]
  }
}

export interface NetworkError extends QiError {
  category: 'NETWORK'
  context: {
    url: string
    method: string
    statusCode?: number
    timeout?: number
  }
}
```

## 🔧 **Package Structure Changes**

### Current Structure:
```
@qi/qicore-foundation
├── /base (Result, QiError)
└── /core (Config, Logger, Cache)
```

### Proposed Structure:
```
@qi/base                 # Core Result/QiError types
@qi/core                 # Infrastructure services
@qi/testing              # Testing utilities
@qi/express              # Express integration
@qi/fastify              # Fastify integration
@qi/qicore-foundation    # Meta-package (re-exports all)
```

### Package.json Changes:
```json
{
  "name": "@qi/qicore-foundation",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.js",
      "require": "./dist/index.cjs"
    },
    "./base": {
      "types": "./dist/base.d.ts", 
      "import": "./dist/base.js",
      "require": "./dist/base.cjs"
    },
    "./core": {
      "types": "./dist/core.d.ts",
      "import": "./dist/core.js", 
      "require": "./dist/core.cjs"
    }
  },
  "typesVersions": {
    "*": {
      "base": ["./dist/base.d.ts"],
      "core": ["./dist/core.d.ts"]
    }
  }
}
```

## 📱 **Example Applications Required**

### 1. Basic Result Usage (`app/basic-result/`)
- ✅ **Current**: Basic operations
- ❌ **Missing**: Advanced patterns, error handling, testing

### 2. Error Handling (`app/error-handling/`)
- ❌ **Missing**: Complete error handling patterns
- ❌ **Missing**: Error categorization examples
- ❌ **Missing**: Retry strategies implementation

### 3. Configuration Management (`app/config-example/`)
- ❌ **Missing**: Real-world config with schema validation
- ❌ **Missing**: Multiple environment support
- ❌ **Missing**: Configuration watching and hot-reload

### 4. Logger Integration (`app/logger-example/`)
- ❌ **Missing**: Structured logging examples
- ❌ **Missing**: OpenTelemetry integration
- ❌ **Missing**: Log aggregation patterns

### 5. Cache Strategies (`app/cache-example/`)
- ❌ **Missing**: Redis integration example
- ❌ **Missing**: Cache-aside patterns
- ❌ **Missing**: Cache warming strategies

### 6. Web Service (`app/web-service/`)
- ❌ **Missing**: Complete Express/Fastify integration
- ❌ **Missing**: Middleware patterns
- ❌ **Missing**: API error handling

### 7. CLI Tool (`app/cli-tool/`)
- ❌ **Missing**: Command-line argument handling
- ❌ **Missing**: Configuration from multiple sources
- ❌ **Missing**: Progress reporting

### 8. Microservice (`app/microservice/`)
- ❌ **Missing**: Service composition
- ❌ **Missing**: Circuit breaker implementation
- ❌ **Missing**: Health checks and monitoring

## 🎯 **Implementation Priority**

### Phase 1 (Immediate - Week 1)
1. Fix import aliasing to `@qi/base` and `@qi/core`
2. Add missing Result utilities
3. Create comprehensive config example with schema validation
4. Create error handling example

### Phase 2 (Short-term - Week 2)
1. Implement async composition utilities
2. Add testing utilities and Jest matchers
3. Create logger and cache examples
4. Build web service example

### Phase 3 (Medium-term - Week 3)
1. Framework integration packages
2. CLI tool example
3. Microservice example
4. Performance optimizations

### Phase 4 (Long-term - Week 4)
1. Type safety improvements
2. Documentation updates
3. Migration guide
4. Performance benchmarks

## 📊 **Success Metrics**

- **Developer Experience**: Reduce boilerplate by 50%
- **Adoption**: Clear import paths and examples
- **Testing**: 100% test coverage with utilities
- **Integration**: Framework plugins for major Node.js frameworks
- **Performance**: No regression in core operations

## 🔒 **Backward Compatibility**

- All existing imports must continue to work
- New utilities are additive, not breaking
- Clear migration path with deprecation warnings
- Version bump strategy: 0.3.4 → 0.4.0 for new features

This proposal addresses all major pain points while maintaining the solid mathematical foundation of QiCore.