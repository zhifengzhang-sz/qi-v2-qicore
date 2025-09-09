# lib/base Architecture

The `lib/base` module is the **foundation** of QiCore Foundation, providing the core functional programming abstractions used throughout the entire system.

## Overview

This module implements the Result<T> monad and QiError system that enables functional error handling across all QiCore modules. It serves as the **reference implementation** for functional programming patterns in the codebase.

## Module Structure

```
lib/base/src/
├── index.ts          # Main exports and re-exports
├── result.ts         # Result<T> monad implementation
├── error.ts          # QiError system and factory functions
└── async.ts          # Async Result<T> combinators
```

## Core Abstractions

### 1. Result<T> Monad

The Result<T> type represents computations that can fail:

```typescript
type Result<T, E = QiError> = 
  | { readonly tag: 'success'; readonly value: T }
  | { readonly tag: 'failure'; readonly error: E }
```

**Key Properties:**
- **Type Safety**: Errors are explicit in function signatures
- **Composable**: Operations chain without nested error handling
- **Functional**: No exceptions, all errors are values

### 2. QiError System

Structured error handling with categorization:

```typescript
interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
  readonly timestamp: Date
  readonly stack?: string
}
```

**Error Categories:**
- `VALIDATION` - Input validation failures (never retry)
- `NETWORK` - Network issues (retry with backoff)
- `BUSINESS` - Business rule violations (never retry)
- `AUTHENTICATION` - Login failures (never retry)
- `AUTHORIZATION` - Permission denied (never retry)
- `SYSTEM` - Infrastructure failures (limited retry)
- `CONFIGURATION` - Config issues (never retry)
- `RESOURCE` - Resource constraints (limited retry)
- `CONCURRENCY` - Race conditions (retry with backoff)
- `TIMEOUT` - Operation timeouts (retry with backoff)
- `LOGGER` - Logging failures (never retry)

## Mathematical Foundations

The Result<T> implementation follows mathematical laws:

### Functor Laws
- **Identity**: `map(id, result) === result`
- **Composition**: `map(f ∘ g, result) === map(f, map(g, result))`

### Monad Laws
- **Left Identity**: `flatMap(f, success(a)) === f(a)`
- **Right Identity**: `flatMap(success, m) === m`
- **Associativity**: `flatMap(g, flatMap(f, m)) === flatMap(x => flatMap(g, f(x)), m)`

### Applicative Laws
- **Identity**: `apply(success(id), v) === v`
- **Composition**: `apply(apply(apply(success(compose), u), v), w) === apply(u, apply(v, w))`

## Core Operations

### Factory Functions
- `success<T>(value: T)` - Creates successful Result
- `failure<E>(error: E)` - Creates failed Result
- `fromTryCatch` - Converts throwing functions to Result<T>
- `fromAsyncTryCatch` - Converts async throwing functions to Result<T>
- `fromMaybe` - Converts nullable values to Result<T>

### Transformation Functions
- `map<T, U>(fn: T => U, result: Result<T>)` - Transform success values
- `flatMap<T, U>(fn: T => Result<U>, result: Result<T>)` - Chain operations
- `match<T>(onSuccess, onFailure, result)` - Handle both cases

### Query Functions
- `isSuccess<T>(result: Result<T>)` - Type guard for success
- `isFailure<T>(result: Result<T>)` - Type guard for failure
- `getValue<T>(result: Result<T>)` - Extract value (unsafe)
- `getError<T>(result: Result<T>)` - Extract error (unsafe)

### Async Operations
- `mapAsync` - Async map over Result<T>
- `flatMapAsync` - Async flatMap over Result<T>
- `collect` - Transform arrays of Results
- `sequence` - Convert array of Results to Result of array

## Design Principles

### 1. Functional Purity
- No side effects in core operations
- All functions are pure and referentially transparent
- Immutable data structures throughout

### 2. Type Safety
- Compiler-enforced error handling
- No runtime type casting (`as any` forbidden)
- Generic constraints prevent misuse

### 3. Composability
- Operations compose naturally with function composition
- Async operations integrate seamlessly with sync operations
- Error handling is automatic in composed operations

### 4. Performance
- All core operations are O(1)
- No unnecessary allocations
- Efficient async handling

## Usage Patterns

### Basic Usage
```typescript
// Create Results
const good = success(42)
const bad = failure(validationError('Invalid input'))

// Transform values
const doubled = map(x => x * 2, good)

// Chain operations
const result = flatMap(
  x => x > 0 ? success(Math.sqrt(x)) : failure(validationError('Must be positive')),
  success(16)
)

// Handle results
match(
  value => console.log(`Success: ${value}`),
  error => console.log(`Error: ${error.message}`),
  result
)
```

### Async Composition
```typescript
const fetchUser = async (id: string): Promise<Result<User, QiError>> => {
  return fromAsyncTryCatch(
    async () => await api.get(`/users/${id}`),
    error => networkError('Failed to fetch user')
  )
}

const updateUser = async (user: User): Promise<Result<User, QiError>> => {
  return fromAsyncTryCatch(
    async () => await api.put(`/users/${user.id}`, user),
    error => networkError('Failed to update user')
  )
}

// Compose operations - errors propagate automatically
const updateUserName = async (id: string, name: string): Promise<Result<User, QiError>> => {
  const userResult = await fetchUser(id)
  return flatMapAsync(
    user => updateUser({ ...user, name }),
    userResult
  )
}
```

### Error Category Handling
```typescript
match(
  user => console.log('Success:', user),
  error => {
    switch (error.category) {
      case 'VALIDATION':
        console.log('Fix input:', error.message)
        break
      case 'NETWORK':
        console.log('Will retry:', error.message)
        // Implement retry logic
        break
      case 'AUTHENTICATION':
        console.log('Login required:', error.message)
        // Redirect to login
        break
    }
  },
  result
)
```

## Integration Points

The base module is used by all other QiCore modules:

- **lib/core** - All operations return Result<T>
- **lib/amsg** - Message queue operations use Result<T>
- **lib/cli** - Command processing uses Result<T>
- **Applications** - All business logic uses Result<T>

## Quality Guarantees

### Testing
- **Property-based testing** with 1000+ iterations per property
- **Mathematical law verification** for Functor/Monad/Applicative laws
- **Comprehensive unit tests** for all operations
- **Integration tests** with other modules

### Performance
- All operations are **O(1)** complexity
- **Zero allocations** in hot paths
- **Efficient async** handling with Promise integration

### Type Safety
- **100% TypeScript coverage**
- **No `any` types** in public API
- **Strict null checks** enforced
- **Generic constraints** prevent misuse

## Anti-Patterns Eliminated

The base module is **100% clean** of anti-patterns:

✅ **Zero inappropriate throw statements** (only unwrap() throws by design)
✅ **Zero unsafe type casting** (`as any` forbidden)
✅ **Zero magic numbers** (all constants named)
✅ **Zero raw try/catch** (only in functional primitives)

This module serves as the **gold standard** for functional programming patterns in QiCore Foundation.

## Future Considerations

- **Parallel processing** combinators (traverse, sequence variants)
- **Resource management** with bracket patterns
- **Streaming** Result<T> operations
- **Interop** with other functional programming libraries

The base module provides the solid mathematical foundation that makes the entire QiCore system reliable, composable, and maintainable.