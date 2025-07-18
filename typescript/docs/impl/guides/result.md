# Result Implementation Guide

## Overview

The Result<T> module provides functional error handling through a discriminated union type that makes error handling explicit and composable. It implements mathematical laws (Functor, Monad, Applicative) for reliable composition and provides TypeScript-native patterns for error handling.

## Design Approach

**Pure functional with discriminated unions**

### Rationale
- Discriminated unions provide exhaustive type checking
- Pure functions enable easy testing and composition
- Matches the mathematical nature of Result<T>
- No surprises or hidden state

### Key Decisions
- Functions take Result as last parameter (enables partial application)
- No methods on types (pure data structures)
- Factory functions return frozen objects
- Type guards for runtime safety

## Core Types

### Result<T, E> Structure
```typescript
export type Result<T, E = QiError> = Success<T> | Failure<E>

export interface Success<T> {
  readonly tag: 'success'
  readonly value: T
}

export interface Failure<E> {
  readonly tag: 'failure'
  readonly error: E
}
```

### Type Guards
```typescript
// Type guards for runtime checking
export const isSuccess = <T, E>(result: Result<T, E>): result is Success<T> =>
  result.tag === 'success'

export const isFailure = <T, E>(result: Result<T, E>): result is Failure<E> =>
  result.tag === 'failure'
```

## Factory Functions

### Core Constructors
```typescript
// Create success result
export const success = <T>(value: T): Result<T, never> => ({
  tag: 'success',
  value
})

// Create failure result
export const failure = <E>(error: E): Result<never, E> => ({
  tag: 'failure',
  error
})

// Create from potentially throwing function
export const fromTryCatch = <T, E = Error>(
  fn: () => T,
  onError?: (error: unknown) => E
): Result<T, E>

// Create from async function
export const fromAsyncTryCatch = async <T, E = Error>(
  fn: () => Promise<T>,
  onError?: (error: unknown) => E
): Promise<Result<T, E>>
```

### Usage Examples
```typescript
// Basic construction
const successResult = success(42)
const failureResult = failure(new Error('Something went wrong'))

// From potentially throwing code
const parseResult = fromTryCatch(
  () => JSON.parse(jsonString),
  (error) => new Error(`Parse error: ${error}`)
)

// From async operations
const fetchResult = await fromAsyncTryCatch(
  () => fetch('/api/data').then(res => res.json()),
  (error) => new Error(`Network error: ${error}`)
)
```

## Functor Operations

### Map Function
```typescript
// Transform success values, preserve failures
export const map = <T, U, E>(
  fn: (value: T) => U,
  result: Result<T, E>
): Result<U, E>

// Usage examples
const numbers = success(5)
const doubled = map(x => x * 2, numbers) // Success(10)

const failed = failure(new Error('error'))
const stillFailed = map(x => x * 2, failed) // Failure(Error('error'))
```

### Map with Error Transformation
```typescript
// Transform both success and failure cases
export const bimap = <T, U, E, F>(
  onSuccess: (value: T) => U,
  onFailure: (error: E) => F,
  result: Result<T, E>
): Result<U, F>

// Usage
const transformed = bimap(
  x => x.toString(),
  err => new Error(`Wrapped: ${err.message}`),
  result
)
```

## Monad Operations

### FlatMap (Bind)
```typescript
// Chain operations that might fail
export const flatMap = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  result: Result<T, E>
): Result<U, E>

// Usage example
const validateThenProcess = (input: string): Result<ProcessedData, ValidationError> =>
  flatMap(
    processData,
    validateInput(input)
  )

// Chaining multiple operations
const pipeline = (data: RawData): Result<FinalData, ProcessingError> =>
  flatMap(
    step3,
    flatMap(
      step2,
      step1(data)
    )
  )
```

### Async FlatMap
```typescript
// Chain async operations
export const flatMapAsync = async <T, U, E>(
  fn: (value: T) => Promise<Result<U, E>>,
  result: Result<T, E>
): Promise<Result<U, E>>

// Usage
const asyncPipeline = async (id: string): Promise<Result<User, UserError>> =>
  await flatMapAsync(
    enrichUserData,
    await flatMapAsync(
      validateUser,
      await fetchUser(id)
    )
  )
```

## Applicative Operations

### Apply Function
```typescript
// Apply a function wrapped in Result to a value wrapped in Result
export const apply = <T, U, E>(
  fn: Result<(value: T) => U, E>,
  result: Result<T, E>
): Result<U, E>

// Usage
const add = (a: number) => (b: number) => a + b
const addResult = success(add)
const result1 = success(5)
const result2 = success(3)

const sum = apply(apply(addResult, result1), result2) // Success(8)
```

### Combine Multiple Results
```typescript
// Combine 2 results
export const combine2 = <T1, T2, E>(
  result1: Result<T1, E>,
  result2: Result<T2, E>
): Result<[T1, T2], E>

// Combine 3 results
export const combine3 = <T1, T2, T3, E>(
  result1: Result<T1, E>,
  result2: Result<T2, E>,
  result3: Result<T3, E>
): Result<[T1, T2, T3], E>

// Usage
const userResult = fetchUser(id)
const permissionsResult = fetchPermissions(id)
const preferencesResult = fetchPreferences(id)

const combinedResult = combine3(userResult, permissionsResult, preferencesResult)
// Result<[User, Permissions, Preferences], Error>
```

## Pattern Matching

### Match Function
```typescript
// Pattern match on Result
export const match = <T, U, E>(
  onSuccess: (value: T) => U,
  onFailure: (error: E) => U,
  result: Result<T, E>
): U

// Usage
const message = match(
  (user) => `Welcome, ${user.name}!`,
  (error) => `Error: ${error.message}`,
  userResult
)
```

### Conditional Operations
```typescript
// Execute side effects based on result
export const ifSuccess = <T, E>(
  fn: (value: T) => void,
  result: Result<T, E>
): void

export const ifFailure = <T, E>(
  fn: (error: E) => void,
  result: Result<T, E>
): void

// Usage
ifSuccess(user => console.log(`User loaded: ${user.name}`), userResult)
ifFailure(error => console.error(`Failed to load user: ${error.message}`), userResult)
```

## Collection Operations

### Sequence
```typescript
// Convert array of Results to Result of array
export const sequence = <T, E>(
  results: Result<T, E>[]
): Result<T[], E>

// Usage
const userIds = ['1', '2', '3']
const userResults = userIds.map(id => fetchUser(id))
const allUsersResult = sequence(userResults) // Result<User[], Error>
```

### Traverse
```typescript
// Map and sequence in one operation
export const traverse = <T, U, E>(
  fn: (value: T) => Result<U, E>,
  array: T[]
): Result<U[], E>

// Usage
const processedUsers = traverse(
  user => validateAndProcessUser(user),
  rawUsers
) // Result<ProcessedUser[], ValidationError>
```

### Partition
```typescript
// Separate successes and failures
export const partition = <T, E>(
  results: Result<T, E>[]
): [T[], E[]]

// Usage
const [successes, failures] = partition(userResults)
console.log(`Loaded ${successes.length} users, ${failures.length} failures`)
```

## Error Handling Patterns

### Or Operations
```typescript
// Use first success or last failure
export const or = <T, E>(
  result1: Result<T, E>,
  result2: Result<T, E>
): Result<T, E>

// Provide default value on failure
export const orElse = <T, E>(
  defaultValue: T,
  result: Result<T, E>
): T

// Chain alternative operations
export const orElseGet = <T, E>(
  fn: () => Result<T, E>,
  result: Result<T, E>
): Result<T, E>

// Usage
const userResult = or(
  fetchUserFromCache(id),
  fetchUserFromDatabase(id)
)

const user = orElse(
  { id: 'unknown', name: 'Anonymous' },
  userResult
)
```

### Error Recovery
```typescript
// Recover from specific errors
export const recover = <T, E>(
  fn: (error: E) => Result<T, E>,
  result: Result<T, E>
): Result<T, E>

// Usage
const recoveredResult = recover(
  (error) => {
    if (error.code === 'NOT_FOUND') {
      return success(createDefaultUser())
    }
    return failure(error)
  },
  userResult
)
```

## Async Operations

### Async Result Helpers
```typescript
// Map over async Result
export const mapAsync = async <T, U, E>(
  fn: (value: T) => Promise<U>,
  result: Result<T, E>
): Promise<Result<U, E>>

// Apply async function to Result
export const applyAsync = async <T, U, E>(
  fn: (value: T) => Promise<U>,
  result: Result<T, E>
): Promise<Result<U, E>>

// Usage
const enrichedUser = await mapAsync(
  async (user) => ({
    ...user,
    avatar: await generateAvatar(user.id)
  }),
  userResult
)
```

### Async Combinators
```typescript
// All async operations must succeed
export const allAsync = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T[], E>>

// First async operation to succeed
export const raceAsync = async <T, E>(
  promises: Promise<Result<T, E>>[]
): Promise<Result<T, E>>

// Usage
const allData = await allAsync([
  fetchUserData(id),
  fetchUserPreferences(id),
  fetchUserPermissions(id)
])
```

## Mathematical Laws

### Functor Laws
```typescript
// Identity: map(identity, result) === result
const identityLaw = <T, E>(result: Result<T, E>): boolean =>
  map(x => x, result) === result

// Composition: map(f, map(g, result)) === map(x => f(g(x)), result)
const compositionLaw = <T, U, V, E>(
  f: (u: U) => V,
  g: (t: T) => U,
  result: Result<T, E>
): boolean =>
  map(f, map(g, result)) === map(x => f(g(x)), result)
```

### Monad Laws
```typescript
// Left identity: flatMap(f, success(a)) === f(a)
const leftIdentityLaw = <T, U, E>(
  a: T,
  f: (t: T) => Result<U, E>
): boolean =>
  flatMap(f, success(a)) === f(a)

// Right identity: flatMap(success, result) === result
const rightIdentityLaw = <T, E>(result: Result<T, E>): boolean =>
  flatMap(success, result) === result

// Associativity: flatMap(g, flatMap(f, result)) === flatMap(x => flatMap(g, f(x)), result)
const associativityLaw = <T, U, V, E>(
  f: (t: T) => Result<U, E>,
  g: (u: U) => Result<V, E>,
  result: Result<T, E>
): boolean =>
  flatMap(g, flatMap(f, result)) === flatMap(x => flatMap(g, f(x)), result)
```

## Integration Examples

### API Response Handling
```typescript
// Transform API responses to Results
const apiCall = async <T>(url: string): Promise<Result<T, ApiError>> =>
  await fromAsyncTryCatch(
    async () => {
      const response = await fetch(url)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`)
      }
      return response.json()
    },
    (error) => new ApiError(error.message)
  )

// Chain API calls
const getUserWithProfile = async (id: string): Promise<Result<UserProfile, ApiError>> =>
  await flatMapAsync(
    async (user) => {
      const profileResult = await apiCall<Profile>(`/api/profiles/${user.id}`)
      return map(
        profile => ({ ...user, profile }),
        profileResult
      )
    },
    await apiCall<User>(`/api/users/${id}`)
  )
```

### Form Validation
```typescript
// Validation functions
const validateEmail = (email: string): Result<string, ValidationError> =>
  email.includes('@') 
    ? success(email)
    : failure(new ValidationError('Invalid email format'))

const validateAge = (age: number): Result<number, ValidationError> =>
  age >= 18
    ? success(age)
    : failure(new ValidationError('Must be 18 or older'))

// Combine validations
const validateUser = (data: UserData): Result<ValidatedUser, ValidationError> => {
  const emailResult = validateEmail(data.email)
  const ageResult = validateAge(data.age)
  
  return map(
    ([email, age]) => ({ email, age }),
    combine2(emailResult, ageResult)
  )
}
```

### Database Operations
```typescript
// Database query wrapper
const query = async <T>(sql: string, params: unknown[]): Promise<Result<T[], DatabaseError>> =>
  await fromAsyncTryCatch(
    () => database.query(sql, params),
    (error) => new DatabaseError(error.message)
  )

// Repository pattern with Result
class UserRepository {
  async findById(id: string): Promise<Result<User, DatabaseError>> {
    const result = await query<User>('SELECT * FROM users WHERE id = ?', [id])
    return flatMap(
      (users) => users.length > 0 
        ? success(users[0])
        : failure(new DatabaseError('User not found')),
      result
    )
  }
  
  async create(userData: CreateUserData): Promise<Result<User, DatabaseError>> {
    return await flatMapAsync(
      async (validatedData) => {
        const result = await query<User>(
          'INSERT INTO users (email, name) VALUES (?, ?) RETURNING *',
          [validatedData.email, validatedData.name]
        )
        return map(users => users[0], result)
      },
      validateUserData(userData)
    )
  }
}
```

## Testing Result Operations

### Unit Testing
```typescript
describe('Result Operations', () => {
  it('should transform success values with map', () => {
    const result = success(5)
    const doubled = map(x => x * 2, result)
    
    expect(doubled).toEqual(success(10))
  })
  
  it('should preserve failures with map', () => {
    const error = new Error('test error')
    const result = failure(error)
    const mapped = map(x => x * 2, result)
    
    expect(mapped).toEqual(failure(error))
  })
  
  it('should chain operations with flatMap', () => {
    const divide = (a: number, b: number): Result<number, Error> =>
      b === 0 ? failure(new Error('Division by zero')) : success(a / b)
    
    const result = flatMap(
      x => divide(x, 2),
      success(10)
    )
    
    expect(result).toEqual(success(5))
  })
  
  it('should combine multiple results', () => {
    const result1 = success(1)
    const result2 = success(2)
    const result3 = success(3)
    
    const combined = combine3(result1, result2, result3)
    
    expect(combined).toEqual(success([1, 2, 3]))
  })
})
```

### Property-Based Testing
```typescript
import { fc } from 'fast-check'

describe('Result Laws', () => {
  it('should satisfy functor identity law', () => {
    fc.assert(fc.property(
      fc.integer(),
      (value) => {
        const result = success(value)
        const mapped = map(x => x, result)
        expect(mapped).toEqual(result)
      }
    ))
  })
  
  it('should satisfy monad left identity law', () => {
    fc.assert(fc.property(
      fc.integer(),
      (value) => {
        const f = (x: number) => success(x * 2)
        const result1 = flatMap(f, success(value))
        const result2 = f(value)
        expect(result1).toEqual(result2)
      }
    ))
  })
})
```

## Performance Considerations

### Optimization Strategies
```typescript
// Lazy evaluation for expensive operations
const lazyMap = <T, U, E>(
  fn: (value: T) => U,
  result: Result<T, E>
): Result<() => U, E> =>
  map(value => () => fn(value), result)

// Memoization for repeated computations
const memoizedMap = <T, U, E>(
  fn: (value: T) => U,
  result: Result<T, E>
): Result<U, E> => {
  const memo = new Map<T, U>()
  return map(value => {
    if (memo.has(value)) {
      return memo.get(value)!
    }
    const computed = fn(value)
    memo.set(value, computed)
    return computed
  }, result)
}
```

### Memory Management
```typescript
// Avoid creating intermediate Results in loops
const processItems = <T, U, E>(
  items: T[],
  processor: (item: T) => Result<U, E>
): Result<U[], E> => {
  const results: U[] = []
  
  for (const item of items) {
    const result = processor(item)
    if (result.tag === 'failure') {
      return result
    }
    results.push(result.value)
  }
  
  return success(results)
}
```

## Best Practices

### Error Handling
- Always handle both success and failure cases
- Use pattern matching for exhaustive handling
- Prefer Result<T> over throwing exceptions
- Chain operations with flatMap for complex flows

### Performance
- Use type guards for runtime checks
- Avoid unnecessary Result wrapping
- Leverage lazy evaluation for expensive operations
- Consider memoization for repeated computations

### Type Safety
- Use specific error types instead of generic Error
- Leverage TypeScript's type inference
- Provide type annotations at API boundaries
- Use branded types for domain-specific values

### Composition
- Prefer small, composable functions
- Use combinators for common patterns
- Leverage mathematical laws for reasoning
- Test compositions with property-based testing

## Common Pitfalls

### Avoiding Anti-Patterns
```typescript
// DON'T: Nested Result unwrapping
const badExample = (result: Result<Result<string, Error>, Error>) => {
  if (result.tag === 'success') {
    const inner = result.value
    if (inner.tag === 'success') {
      return inner.value
    }
  }
  // Complex error handling...
}

// DO: Flatten with flatMap
const goodExample = (result: Result<Result<string, Error>, Error>) =>
  flatMap(x => x, result)
```

### Error Accumulation
```typescript
// DON'T: Lose error information
const badValidation = (data: UserData): Result<ValidatedUser, ValidationError> => {
  const emailResult = validateEmail(data.email)
  if (emailResult.tag === 'failure') {
    return emailResult
  }
  // Lost subsequent validation errors...
}

// DO: Accumulate all errors
const goodValidation = (data: UserData): Result<ValidatedUser, ValidationError[]> => {
  const emailResult = validateEmail(data.email)
  const ageResult = validateAge(data.age)
  const nameResult = validateName(data.name)
  
  const errors = [emailResult, ageResult, nameResult]
    .filter(isFailure)
    .map(r => r.error)
  
  if (errors.length > 0) {
    return failure(errors)
  }
  
  return success({
    email: emailResult.value,
    age: ageResult.value,
    name: nameResult.value
  })
}
```

## Migration Guide

### From Exception-Based Code
```typescript
// Before: Exception-based
function parseJSON(json: string): object {
  return JSON.parse(json) // Can throw
}

// After: Result-based
function parseJSON(json: string): Result<object, Error> {
  return fromTryCatch(
    () => JSON.parse(json),
    (error) => new Error(`Parse error: ${error}`)
  )
}
```

### From Promise-Based Code
```typescript
// Before: Promise-based
async function fetchUser(id: string): Promise<User> {
  const response = await fetch(`/api/users/${id}`)
  return response.json()
}

// After: Result-based
async function fetchUser(id: string): Promise<Result<User, Error>> {
  return await fromAsyncTryCatch(
    async () => {
      const response = await fetch(`/api/users/${id}`)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }
      return response.json()
    },
    (error) => new Error(`Fetch error: ${error}`)
  )
}
```

This comprehensive guide covers all aspects of the Result<T> implementation, from basic usage to advanced patterns and integration examples. The Result type provides a solid foundation for functional error handling in TypeScript applications.