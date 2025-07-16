# Understanding Result<T> - The Foundation

Result<T> is the cornerstone of QiCore Foundation, providing a functional approach to error handling that eliminates exceptions and makes error states explicit in your type system.

## What is Result<T>?

Result<T> is a discriminated union type that represents either a successful value or an error:

```typescript
type Result<T, E = QiError> = 
  | { tag: 'success'; value: T }
  | { tag: 'failure'; error: E }
```

This design ensures:
- **Explicit error handling**: Errors are part of the type system
- **No exceptions**: All errors are handled through the type system
- **Composable operations**: Chain operations without nested try-catch blocks
- **Mathematical guarantees**: Follows functor, applicative, and monad laws

## Creating Results

### Success Values

```typescript
import { Ok, type Result } from '@qi/qicore-foundation/base'

// Create successful results
const number: Result<number> = Ok(42)
const string: Result<string> = Ok('hello')
const object: Result<User> = Ok({ id: 1, name: 'Alice' })

console.log(number) // { tag: 'success', value: 42 }
```

### Error Values

```typescript
import { Err, type Result } from '@qi/qicore-foundation/base'

// Create error results
const error: Result<never> = Err(new Error('Something went wrong'))
const customError: Result<never> = Err('Custom error message')

console.log(error) // { tag: 'failure', error: Error('Something went wrong') }
```

### From Nullable Values

```typescript
import { fromNullable, createError } from '@qi/qicore-foundation/base'

const maybeUser: User | null = getUser()
const userResult: Result<User> = fromNullable(
  maybeUser,
  createError('USER_NOT_FOUND', 'User not found')
)
```

### From Try-Catch

```typescript
import { tryCatch, asyncTryCatch } from '@qi/qicore-foundation/base'

// Synchronous operations
const parseResult = tryCatch(() => JSON.parse(jsonString))

// Asynchronous operations
const fetchResult = await asyncTryCatch(async () => {
  const response = await fetch('/api/data')
  return await response.json()
})
```

## Checking Results

### Type Guards

```typescript
import { isSuccess, isFailure } from '@qi/qicore-foundation/base'

const result: Result<number> = Ok(42)

if (isSuccess(result)) {
  // TypeScript knows this is Success<number>
  console.log(result.value) // 42
}

if (isFailure(result)) {
  // TypeScript knows this is Failure<QiError>
  console.log(result.error)
}
```

### Pattern Matching

```typescript
import { match } from '@qi/qicore-foundation/base'

const result: Result<number> = Ok(42)

const output = match(result, {
  success: (value) => `Success: ${value}`,
  failure: (error) => `Error: ${error.message}`
})

console.log(output) // "Success: 42"
```

### Direct Property Access

```typescript
const result: Result<number> = Ok(42)

// Check the tag
if (result.tag === 'success') {
  console.log(result.value) // TypeScript knows this is safe
}

if (result.tag === 'failure') {
  console.log(result.error) // TypeScript knows this is safe
}
```

## Extracting Values

### Safe Extraction

```typescript
import { getValue, getError, unwrapOr } from '@qi/qicore-foundation/base'

const successResult: Result<number> = Ok(42)
const errorResult: Result<number> = Err(new Error('fail'))

// Get value safely (returns undefined if error)
const value = getValue(successResult) // 42
const noValue = getValue(errorResult) // undefined

// Get error safely (returns undefined if success)
const error = getError(errorResult) // Error('fail')
const noError = getError(successResult) // undefined

// Provide default value
const safeValue = unwrapOr(errorResult, 0) // 0
```

### Unsafe Extraction (Use Carefully)

```typescript
import { unwrap } from '@qi/qicore-foundation/base'

const result: Result<number> = Ok(42)

// Only use when you're certain the result is successful
const value = unwrap(result) // 42 - throws if result is failure
```

## Transforming Results

### Map (Transform Success Values)

```typescript
import { map } from '@qi/qicore-foundation/base'

const number: Result<number> = Ok(42)

// Transform the success value
const doubled = map(number, x => x * 2)
// Result<number> = Ok(84)

const formatted = map(number, x => `Value: ${x}`)
// Result<string> = Ok("Value: 42")

// Map chains naturally
const result = map(
  map(number, x => x * 2),
  x => `Result: ${x}`
)
// Result<string> = Ok("Result: 84")
```

### Map Errors

```typescript
import { mapError } from '@qi/qicore-foundation/base'

const error: Result<number> = Err(new Error('original'))

// Transform the error
const mappedError = mapError(error, e => new Error(`Enhanced: ${e.message}`))
// Result<number> = Err(Error('Enhanced: original'))
```

### FlatMap (Chain Operations)

```typescript
import { flatMap } from '@qi/qicore-foundation/base'

function divide(a: number, b: number): Result<number> {
  if (b === 0) return Err(new Error('Division by zero'))
  return Ok(a / b)
}

function sqrt(x: number): Result<number> {
  if (x < 0) return Err(new Error('Negative square root'))
  return Ok(Math.sqrt(x))
}

// Chain operations that might fail
const result = flatMap(divide(16, 4), sqrt)
// Result<number> = Ok(2)

const errorResult = flatMap(divide(16, 0), sqrt)
// Result<number> = Err(Error('Division by zero'))
```

### Alternative: AndThen

```typescript
import { andThen } from '@qi/qicore-foundation/base'

// andThen is an alias for flatMap
const result = andThen(divide(16, 4), sqrt)
```

## Chaining Operations

### Sequential Operations

```typescript
import { map, flatMap } from '@qi/qicore-foundation/base'

// Real-world example: user processing pipeline
function processUser(id: number): Result<ProcessedUser> {
  return flatMap(
    fetchUser(id),
    user => flatMap(
      validateUser(user),
      validUser => flatMap(
        enrichUser(validUser),
        enrichedUser => map(
          enrichedUser,
          user => ({ ...user, processed: true })
        )
      )
    )
  )
}
```

### Using Do-Notation Style

```typescript
// Create a helper for more readable chaining
function resultDo<T>(generator: () => Generator<Result<any>, T>): Result<T> {
  // Implementation would handle the generator protocol
  // This is a conceptual example
}

// Usage (conceptual)
const result = resultDo(function* () {
  const user = yield* fetchUser(id)
  const validated = yield* validateUser(user)
  const enriched = yield* enrichUser(validated)
  return { ...enriched, processed: true }
})
```

## Error Handling Patterns

### Providing Defaults

```typescript
import { orElse } from '@qi/qicore-foundation/base'

const primaryResult: Result<string> = Err(new Error('primary failed'))
const fallbackResult: Result<string> = Ok('fallback value')

const result = orElse(primaryResult, () => fallbackResult)
// Result<string> = Ok('fallback value')
```

### Filtering Results

```typescript
import { filter } from '@qi/qicore-foundation/base'

const number: Result<number> = Ok(42)

const evenOnly = filter(
  number,
  x => x % 2 === 0,
  () => new Error('Number is not even')
)
// Result<number> = Ok(42)

const oddOnly = filter(
  number,
  x => x % 2 === 1,
  () => new Error('Number is not odd')
)
// Result<number> = Err(Error('Number is not odd'))
```

## Async Operations

### Async Map

```typescript
import { asyncMap } from '@qi/qicore-foundation/base'

const number: Result<number> = Ok(42)

const asyncResult = await asyncMap(number, async x => {
  const response = await fetch(`/api/data/${x}`)
  return await response.json()
})
```

### Async FlatMap

```typescript
import { asyncAndThen } from '@qi/qicore-foundation/base'

async function fetchUserData(id: number): Promise<Result<UserData>> {
  const userResult = await fetchUser(id)
  
  return asyncAndThen(userResult, async user => {
    const profileResult = await fetchProfile(user.id)
    return asyncAndThen(profileResult, async profile => {
      const settingsResult = await fetchSettings(user.id)
      return map(settingsResult, settings => ({
        user,
        profile,
        settings
      }))
    })
  })
}
```

### From Promises

```typescript
import { fromPromise } from '@qi/qicore-foundation/base'

// Convert Promise to Result
const resultFromPromise = await fromPromise(
  fetch('/api/data').then(r => r.json())
)
```

### To Promises

```typescript
import { toPromise } from '@qi/qicore-foundation/base'

const result: Result<number> = Ok(42)

// Convert Result to Promise (rejects if error)
const promise = toPromise(result)
```

## Real-World Example

Here's a complete example showing Result<T> in action:

```typescript
import { 
  Ok, Err, map, flatMap, asyncAndThen, fromPromise,
  type Result 
} from '@qi/qicore-foundation/base'

interface User {
  id: number
  email: string
  name: string
}

interface UserProfile {
  user: User
  preferences: Record<string, any>
  permissions: string[]
}

// API functions that return Results
async function fetchUser(id: number): Promise<Result<User>> {
  if (id <= 0) {
    return Err(new Error('Invalid user ID'))
  }
  
  return fromPromise(fetch(`/api/users/${id}`).then(r => r.json()))
}

async function fetchPreferences(userId: number): Promise<Result<Record<string, any>>> {
  return fromPromise(fetch(`/api/users/${userId}/preferences`).then(r => r.json()))
}

async function fetchPermissions(userId: number): Promise<Result<string[]>> {
  return fromPromise(fetch(`/api/users/${userId}/permissions`).then(r => r.json()))
}

// Business logic using Result composition
async function getUserProfile(id: number): Promise<Result<UserProfile>> {
  const userResult = await fetchUser(id)
  
  return asyncAndThen(userResult, async user => {
    const preferencesResult = await fetchPreferences(user.id)
    
    return asyncAndThen(preferencesResult, async preferences => {
      const permissionsResult = await fetchPermissions(user.id)
      
      return map(permissionsResult, permissions => ({
        user,
        preferences,
        permissions
      }))
    })
  })
}

// Usage
async function main() {
  const profileResult = await getUserProfile(123)
  
  if (profileResult.tag === 'success') {
    console.log('User profile:', profileResult.value)
  } else {
    console.error('Failed to load profile:', profileResult.error)
  }
}
```

## Key Benefits

1. **Type Safety**: Errors are explicit in the type system
2. **No Exceptions**: All error paths are handled through types
3. **Composable**: Operations chain naturally without nested try-catch
4. **Predictable**: Functions always return Result<T>, never throw
5. **Testable**: Easy to test both success and failure cases

## Best Practices

1. **Always handle both cases**: Never ignore the possibility of failure
2. **Use type guards**: Leverage TypeScript's type narrowing
3. **Compose operations**: Use map and flatMap for cleaner code
4. **Provide meaningful errors**: Use structured error types
5. **Prefer early returns**: Handle errors as soon as possible

## Next Steps

Now that you understand Result<T> basics, learn about:
- [Error Handling with QiError](./03-error-handling.md)
- [Advanced Result Operations](./04-advanced-result.md)

## Common Patterns Reference

```typescript
// Pattern 1: Simple transformation
const result = map(getValue(), transform)

// Pattern 2: Chaining operations
const result = flatMap(step1(), step2)

// Pattern 3: Error handling with defaults
const result = orElse(risky(), () => safe())

// Pattern 4: Async operations
const result = await asyncMap(value, asyncTransform)

// Pattern 5: Pattern matching
const output = match(result, {
  success: handleSuccess,
  failure: handleError
})
```