# QiCore Foundation Tutorial

## Stop Fighting Your Code

**The Problem**: Your TypeScript code is full of invisible error paths, null checks everywhere, and configuration chaos.

**The Solution**: QiCore Foundation provides mathematical guarantees for error handling, configuration, and infrastructure services.

This tutorial shows you how to replace painful patterns with clean, composable code.

---

## Part 1: Replace Error Handling Hell

### The Pain: Invisible Errors

```typescript
// ❌ What errors can this throw? Nobody knows!
async function fetchUserProfile(id: string): Promise<UserProfile> {
  const user = await fetchUser(id)         // Can throw NetworkError, ValidationError, AuthError
  const preferences = await fetchPrefs(id) // Can throw DatabaseError, PermissionError
  const avatar = await fetchAvatar(id)     // Can throw S3Error, NotFoundError
  
  return { user, preferences, avatar }
}

// Caller is flying blind
try {
  const profile = await fetchUserProfile("123")
  // Hope nothing goes wrong... 🤞
} catch (error) {
  // What type of error is this? How should I handle it?
  console.error("Something went wrong:", error)
}
```

### The Fix: Explicit Error Handling

```typescript
// ✅ Errors are visible in the type system
import { Result, success, failure, match, flatMap, map } from '@qi/base'

async function fetchUserProfile(id: string): Promise<Result<UserProfile, QiError>> {
  const userResult = await fetchUser(id)
  if (userResult.tag === 'failure') return userResult
  
  const prefsResult = await fetchPrefs(id)
  if (prefsResult.tag === 'failure') return prefsResult
  
  const avatarResult = await fetchAvatar(id)
  if (avatarResult.tag === 'failure') return avatarResult
  
  return success({
    user: userResult.value,
    preferences: prefsResult.value,
    avatar: avatarResult.value
  })
}

// Caller MUST handle both success and failure
const result = await fetchUserProfile("123")
match(
  profile => {
    // Success case: profile is guaranteed to be UserProfile
    renderProfile(profile)
  },
  error => {
    // Failure case: error is guaranteed to be QiError
    switch (error.category) {
      case 'NETWORK': 
        showRetryButton()
        break
      case 'AUTHENTICATION': 
        redirectToLogin()
        break
      case 'AUTHORIZATION':
        showPermissionError()
        break
      default: 
        showGenericError(error.message)
    }
  },
  result
)
```

**What just happened?**
1. **Errors are visible**: Function signature shows `Result<UserProfile, QiError>`
2. **Forced handling**: TypeScript compiler ensures you handle both success and failure
3. **Structured errors**: `QiError` has categories that tell you how to respond
4. **No exceptions**: Your code can't crash from unhandled errors

> 🚀 **See it working**: `cd app/basic-result && bun run dev` - Real divide(), map(), and flatMap() examples

---

## Part 2: Composable Error Handling

### The Pain: Repetitive Error Checking

That manual error checking gets repetitive fast:

```typescript
// ❌ This gets old quickly
async function processOrder(orderData: OrderData): Promise<Result<Order, QiError>> {
  const validationResult = await validateOrder(orderData)
  if (validationResult.tag === 'failure') return validationResult
  
  const paymentResult = await processPayment(validationResult.value)
  if (paymentResult.tag === 'failure') return paymentResult
  
  const inventoryResult = await reserveInventory(paymentResult.value)
  if (inventoryResult.tag === 'failure') return inventoryResult
  
  const fulfillmentResult = await scheduleFulfillment(inventoryResult.value)
  if (fulfillmentResult.tag === 'failure') return fulfillmentResult
  
  return success(fulfillmentResult.value)
}
```

### The Fix: Automatic Error Propagation

```typescript
// ✅ flatMap automatically handles error propagation
async function processOrder(orderData: OrderData): Promise<Result<Order, QiError>> {
  const result = await validateOrder(orderData)
  
  return flatMap(
    valid => processPayment(valid),
    await flatMap(
      payment => reserveInventory(payment),
      await flatMap(
        inventory => scheduleFulfillment(inventory),
        result
      )
    )
  )
}

// Or even cleaner with a helper function
async function processOrder(orderData: OrderData): Promise<Result<Order, QiError>> {
  const result = await validateOrder(orderData)
  
  return pipeline(
    result,
    processPayment,
    reserveInventory,
    scheduleFulfillment
  )
}

// Helper function for chaining async operations
async function pipeline<T, U, V, W, E>(
  initial: Result<T, E>,
  step1: (value: T) => Promise<Result<U, E>>,
  step2: (value: U) => Promise<Result<V, E>>,
  step3: (value: V) => Promise<Result<W, E>>
): Promise<Result<W, E>> {
  if (initial.tag === 'failure') return initial
  
  const result1 = await step1(initial.value)
  if (result1.tag === 'failure') return result1
  
  const result2 = await step2(result1.value)
  if (result2.tag === 'failure') return result2
  
  return step3(result2.value)
}
```

**What just happened?**
1. **Automatic propagation**: First error stops the chain
2. **Clean composition**: Each step is a pure function
3. **No boilerplate**: No repetitive error checking
4. **Mathematical guarantee**: This works because Result<T> follows monad laws

---

## Part 3: Form Validation That Makes Sense

### The Pain: Mixed Validation Logic

```typescript
// ❌ Validation mixed with business logic
async function registerUser(formData: FormData): Promise<User> {
  // Email validation
  if (!formData.email || !isValidEmail(formData.email)) {
    throw new Error('Invalid email') // What about the field name?
  }
  
  // Password validation
  if (!formData.password || formData.password.length < 8) {
    throw new Error('Password too short') // How short? What are the rules?
  }
  
  // Business logic validation
  const existingUser = await getUserByEmail(formData.email)
  if (existingUser) {
    throw new Error('Email already exists') // Should this retry? Redirect?
  }
  
  // More validation scattered throughout...
  
  return createUser(formData)
}
```

### The Fix: Composable Validation Pipeline

```typescript
// ✅ Each validation step is clear and testable
import { Result, success, failure, flatMap, validationError } from '@qi/base'

// Pure validation functions
const validateEmail = (data: FormData): Result<FormData, QiError> =>
  isValidEmail(data.email)
    ? success(data)
    : failure(validationError('Invalid email format', { 
        field: 'email', 
        value: data.email 
      }))

const validatePassword = (data: FormData): Result<FormData, QiError> =>
  data.password.length >= 8
    ? success(data)
    : failure(validationError('Password must be at least 8 characters', { 
        field: 'password',
        minLength: 8,
        actualLength: data.password.length 
      }))

// Async business validation
const validateEmailUnique = async (data: FormData): Promise<Result<FormData, QiError>> => {
  const existingUser = await getUserByEmail(data.email)
  return existingUser
    ? failure(validationError('Email already registered', { 
        field: 'email',
        conflictId: existingUser.id 
      }))
    : success(data)
}

// Compose validation pipeline
const registerUser = async (formData: FormData): Promise<Result<User, QiError>> => {
  const validationResult = validateEmail(formData)
  
  return flatMap(
    data => validatePassword(data),
    await flatMap(
      data => validateEmailUnique(data),
      await flatMap(
        data => createUser(data),
        validationResult
      )
    )
  )
}

// Usage provides specific error context
const registrationResult = await registerUser(formData)
match(
  user => {
    showSuccessMessage('Registration complete!')
    redirectToWelcome(user)
  },
  error => {
    // Every error has structured context
    if (error.context?.field) {
      showFieldError(error.context.field, error.message)
    } else {
      showGenericError(error.message)
    }
  },
  registrationResult
)
```

**What just happened?**
1. **Single responsibility**: Each validator does one thing
2. **Testable**: Each function can be tested independently
3. **Composable**: Chain validators with flatMap
4. **Rich context**: Errors include specific field information
5. **Predictable**: Same pattern for all validation

> 🚀 **See real examples**: `cd app/error-extension && bun run dev` - UserError, PaymentError, OrderError types with full error composition!

---

## Part 4: Configuration Without Chaos

### The Pain: Configuration Nightmare

```typescript
// ❌ Configuration loading is a mess
function loadConfig(): Config {
  let config: any = {}
  
  // Try to load from file
  try {
    const fileContent = fs.readFileSync('./config.json', 'utf-8')
    config = JSON.parse(fileContent)
  } catch (error) {
    console.warn('No config file found, using defaults')
  }
  
  // Override with environment variables
  if (process.env.API_URL) {
    config.api = { url: process.env.API_URL }
  }
  if (process.env.DB_HOST) {
    config.database = { host: process.env.DB_HOST }
  }
  
  // Validation is an afterthought
  if (!config.api?.url) {
    throw new Error('API URL is required')
  }
  
  return config as Config // Hope for the best!
}
```

### The Fix: Fluent Configuration Pipeline

```typescript
// ✅ Clear, declarative configuration loading
import { ConfigBuilder } from '@qi/core'
import { z } from 'zod'

// Define what your config should look like
const configSchema = z.object({
  api: z.object({
    url: z.string().url(),
    timeout: z.number().default(5000)
  }),
  database: z.object({
    host: z.string(),
    port: z.number().default(5432)
  }),
  features: z.object({
    monitoring: z.boolean().default(true)
  })
})

// Load configuration with clear precedence
const config = ConfigBuilder
  .fromJsonFile('./config.json')
  .merge(ConfigBuilder.fromEnv('APP_'))
  .merge(ConfigBuilder.fromObject({
    // Defaults
    api: { timeout: 5000 },
    database: { port: 5432 },
    features: { monitoring: true }
  }))
  .validateWith(configSchema)
  .build()

// Type-safe access
const apiUrl = config.get<string>('api.url')           // Required, will throw if missing
const timeout = config.getOr('api.timeout', 5000)     // Optional with default
const dbPort = config.getOptional<number>('database.port') // Optional, returns undefined if missing
```

**What just happened?**
1. **Clear precedence**: JSON file < Environment < Defaults (left to right)
2. **Schema validation**: Zod ensures your config is correct
3. **Type safety**: TypeScript knows the exact shape of your config
4. **Descriptive errors**: Know exactly what's missing or invalid
5. **Testable**: Each source can be tested independently

> 🚀 **See it working**: `cd app/config-example && bun run dev` - Real YAML loading, env merging, schema validation, + structured logging!

---

## Part 5: Why This Actually Works

### The Mathematical Foundation

You might wonder: "Why does this `flatMap` thing work so reliably?"

It's because `Result<T>` follows **mathematical laws** that guarantee predictable behavior:

```typescript
// Law 1: Identity - wrapping and unwrapping does nothing
const value = 42
const wrapped = success(value)
const unwrapped = flatMap(x => success(x), wrapped)
// wrapped === unwrapped (guaranteed)

// Law 2: Composition - order of operations doesn't matter
const addOne = (x: number) => success(x + 1)
const double = (x: number) => success(x * 2)

// These produce identical results:
const composed = flatMap(double, flatMap(addOne, success(5)))
const direct = flatMap(x => flatMap(double, addOne(x)), success(5))
```

These aren't just nice properties - they're **mathematical guarantees** verified by automated testing:

```typescript
// Property-based testing ensures laws hold for ALL inputs
import { test } from '@fast-check/vitest'

test.prop([fc.integer()])(
  'flatMap identity law',
  (value) => {
    const result = success(value)
    const identity = x => success(x)
    expect(flatMap(identity, result)).toEqual(result)
  }
)
```

**What this means for you:**
1. **Predictable refactoring**: You can rearrange code without changing behavior
2. **Composable functions**: Small functions combine reliably into large ones
3. **No surprises**: The math guarantees your code works the same way every time

---

## Part 6: Real-World Integration

### Express.js API with Result<T>

```typescript
// Middleware that automatically handles Result<T> responses
const resultMiddleware = (
  handler: (req: Request) => Promise<Result<any, QiError>>
) => async (req: Request, res: Response) => {
  const result = await handler(req)
  
  match(
    data => res.json(data),
    error => {
      const statusCode = errorToHttpStatus(error.category)
      res.status(statusCode).json({
        error: error.message,
        code: error.code,
        details: error.context
      })
    },
    result
  )
}

// Clean route handlers
app.get('/users/:id', resultMiddleware(async (req) => {
  return flatMap(
    user => hasPermission(user, req.user, 'read')
      ? success(user)
      : failure(authorizationError('Cannot view user')),
    await findUser(req.params.id)
  )
}))

// Utility function for HTTP status codes
function errorToHttpStatus(category: ErrorCategory): number {
  switch (category) {
    case 'VALIDATION': return 400
    case 'AUTHENTICATION': return 401
    case 'AUTHORIZATION': return 403
    case 'BUSINESS': return 422
    case 'NETWORK': return 502
    default: return 500
  }
}
```

### React Component Error Handling

```typescript
// Custom hook for Result<T> data fetching
function useUserData(id: string): Result<User, QiError> | null {
  const [result, setResult] = useState<Result<User, QiError> | null>(null)
  
  useEffect(() => {
    fetchUser(id).then(setResult)
  }, [id])
  
  return result
}

// Component that handles all error states
function UserProfile({ id }: { id: string }) {
  const userResult = useUserData(id)
  
  if (!userResult) {
    return <LoadingSpinner />
  }
  
  return match(
    user => <UserDetails user={user} />,
    error => <ErrorBoundary 
      error={error} 
      onRetry={() => window.location.reload()}
    />,
    userResult
  )
}

// Reusable error boundary component
function ErrorBoundary({ error, onRetry }: { error: QiError, onRetry: () => void }) {
  switch (error.category) {
    case 'NETWORK':
      return <NetworkErrorMessage onRetry={onRetry} />
    case 'AUTHENTICATION':
      return <LoginPrompt />
    case 'AUTHORIZATION':
      return <PermissionDenied />
    default:
      return <GenericError message={error.message} />
  }
}
```

---

## Part 7: Testing Your Result<T> Code

### Testing Individual Functions

```typescript
// Pure functions are easy to test
describe('validateEmail', () => {
  it('accepts valid emails', () => {
    const result = validateEmail({ email: 'test@example.com' })
    expect(result.tag).toBe('success')
  })
  
  it('rejects invalid emails', () => {
    const result = validateEmail({ email: 'invalid-email' })
    expect(result.tag).toBe('failure')
    expect(result.error.category).toBe('VALIDATION')
    expect(result.error.context?.field).toBe('email')
  })
})
```

### Testing Error Compositions

```typescript
// Test that errors propagate correctly
describe('processOrder', () => {
  it('stops at first validation error', async () => {
    const invalidData = { email: 'invalid', password: 'short' }
    
    const result = await registerUser(invalidData)
    
    expect(result.tag).toBe('failure')
    expect(result.error.context?.field).toBe('email') // First error wins
  })
  
  it('handles all validation steps when valid', async () => {
    const validData = { email: 'test@example.com', password: 'longpassword' }
    
    const result = await registerUser(validData)
    
    expect(result.tag).toBe('success')
    expect(result.value.email).toBe('test@example.com')
  })
})
```

### Testing Configuration

```typescript
// Test configuration loading
describe('config loading', () => {
  it('merges sources correctly', () => {
    const config = ConfigBuilder
      .fromObject({ api: { url: 'default' } })
      .merge(ConfigBuilder.fromObject({ api: { url: 'override' } }))
      .build()
    
    expect(config.get('api.url')).toBe('override')
  })
  
  it('validates required fields', () => {
    expect(() => {
      ConfigBuilder
        .fromObject({ api: { timeout: 5000 } })
        .validateWith(configSchema)
        .build()
    }).toThrow('api.url is required')
  })
})
```

---

## Part 8: Migration Strategy

### Start Small: Replace One Function

```typescript
// Before: Traditional error handling
async function getUser(id: string): Promise<User> {
  const user = await db.findUser(id)
  if (!user) {
    throw new Error('User not found')
  }
  return user
}

// After: Result<T> error handling
async function getUser(id: string): Promise<Result<User, QiError>> {
  const user = await db.findUser(id)
  return user
    ? success(user)
    : failure(resourceError('User not found', { userId: id }))
}
```

### Gradually Expand: Convert Call Sites

```typescript
// Before: try/catch everywhere
try {
  const user = await getUser(id)
  console.log('Found user:', user.name)
} catch (error) {
  console.error('Error:', error.message)
}

// After: explicit error handling
const userResult = await getUser(id)
match(
  user => console.log('Found user:', user.name),
  error => console.error('Error:', error.message),
  userResult
)
```

### Scale Up: Convert Entire Modules

```typescript
// Convert a whole service module
export class UserService {
  async findById(id: string): Promise<Result<User, QiError>> {
    return flatMap(
      user => this.validatePermissions(user),
      await this.getUser(id)
    )
  }
  
  async update(id: string, data: UpdateData): Promise<Result<User, QiError>> {
    return flatMap(
      user => this.saveUser(user, data),
      await this.findById(id)
    )
  }
}
```

---

## Summary: What You've Learned

### 1. **Problem-Solving Patterns**
- Replace invisible errors with explicit `Result<T, QiError>`
- Use `flatMap` for automatic error propagation
- Structure errors with categories for different handling strategies

### 2. **Composable Architecture**
- Small, pure functions combine reliably
- Mathematical laws guarantee predictable behavior
- Same patterns work everywhere (validation, API calls, data processing)

### 3. **Production-Ready Patterns**
- Express.js middleware for consistent API error handling
- React error boundaries with structured error types
- Configuration loading with clear precedence and validation

### 4. **Migration Strategy**
- Start with one function, expand gradually
- Convert call sites as you go
- Scale up to entire modules when ready

### 5. **Testing Benefits**
- Pure functions are easy to test
- Error cases are explicit and testable
- Mathematical laws are verified automatically

---

## Working Examples - Progressive Learning Path

Instead of fictional code snippets, **try the actual working examples** that demonstrate each concept:

### 🎯 **Progressive Package Learning:**

```bash
# 1. Start Here: Basic Result<T> patterns (qi/base)
cd app/basic-result && bun run dev

# 2. Add Domain Errors: Error extension patterns (qi/base)  
cd app/error-extension && bun run dev

# 3. Add Logging: Configuration + Logger integration (qi/core)
cd app/config-example && bun run dev

# 4. Add Caching: Performance optimization (qi/core)
cd app/cache-example && bun run dev
```

### 📈 **This Shows the Package Progression:**

1. **`Result<T>` + `QiError`** (qi/base - most used) → `basic-result/`
2. **Domain-specific errors** (qi/base) → `error-extension/`  
3. **Logger integration** (qi/core - second most used) → `config-example/`
4. **Cache strategies** (qi/core - project-specific) → `cache-example/`

### ✅ **Why These Examples Work:**

- **Real implementations**: All code uses actual v-0.3.6 API
- **Runnable immediately**: No setup required (except Redis for cache)
- **Production patterns**: Copy-paste ready for your projects
- **Progressive complexity**: Each builds on the previous
- **No fake code**: Everything actually works

### 🚀 **Quick Start:**

```bash
# Clone and run first example
git clone <repo>
cd typescript/app/basic-result
bun run dev

# See Result<T> patterns in action immediately!
```

### 📚 **Each Example Teaches:**

| Example | Concepts | Usage Pattern |
|---------|----------|---------------|
| `basic-result/` | Result<T>, map, flatMap, match | Most fundamental - use everywhere |
| `error-extension/` | Domain errors, error categories | Organize errors by business domain |
| `config-example/` | Config + Logger integration | Essential for any production app |
| `cache-example/` | Memory + Redis caching | Add when performance matters |

### 🎯 **Next Steps:**

1. **Follow the progression** - Start with basic-result, work through each example
2. **Run them all** - See how the concepts connect
3. **Apply to your project** - Use patterns as templates
4. **Scale gradually** - Convert one function at a time

The mathematical foundation ensures your code remains predictable and composable as you add each layer.

**Welcome to reliable TypeScript!** 🎉