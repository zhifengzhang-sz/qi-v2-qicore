# TypeScript Implementation Usage Examples

## Real-World Integration Examples

### Why Result<T> Over Try/Catch

**Problem: Invisible Error Paths**
```typescript
// ❌ Traditional approach - errors are invisible in types
async function fetchUserProfile(id: string): Promise<UserProfile> {
  const user = await fetchUser(id)         // Can throw
  const preferences = await fetchPrefs(id) // Can throw
  const avatar = await fetchAvatar(id)     // Can throw
  
  return { user, preferences, avatar }
}

// Caller has no idea what errors to handle
const profile = await fetchUserProfile("123") // 🤷 What could go wrong?
```

**Solution: Explicit Error Handling**
```typescript
// ✅ With Result<T> - errors are visible and typed
function fetchUserProfile(id: string): Promise<Result<UserProfile>> {
  return pipe(
    fetchUser(id),
    flatMap(user => 
      fetchPrefs(id).map(prefs => ({ user, prefs }))
    ),
    flatMap(data => 
      fetchAvatar(id).map(avatar => ({ ...data, avatar }))
    )
  )
}

// Caller must handle errors explicitly
const result = await fetchUserProfile("123")
match(
  profile => renderProfile(profile),
  error => {
    switch (error.category) {
      case 'NETWORK': return showRetryButton()
      case 'AUTHENTICATION': return redirectToLogin()
      default: return showGenericError()
    }
  },
  result
)
```

### Express/Next.js Integration

**Problem: Inconsistent Error Responses**
```typescript
// ❌ Error handling scattered everywhere
app.get('/api/user/:id', async (req, res) => {
  try {
    const user = await db.findUser(req.params.id)
    if (!user) {
      return res.status(404).json({ error: 'User not found' })
    }
    
    const permissions = await getPermissions(user)
    if (!permissions.canView) {
      return res.status(403).json({ error: 'Forbidden' })
    }
    
    res.json(user)
  } catch (error) {
    console.error(error)
    res.status(500).json({ error: 'Internal server error' })
  }
})
```

**Solution: Composable Error Handling**
```typescript
// ✅ With Result<T> - consistent error handling
const resultMiddleware = (
  handler: (req: Request) => Promise<Result<any>>
) => async (req: Request, res: Response) => {
  const result = await handler(req)
  
  match(
    data => res.json(data),
    error => {
      const status = errorToStatus(error.category)
      res.status(status).json({
        error: error.message,
        code: error.code,
        details: error.context
      })
    },
    result
  )
}

app.get('/api/user/:id', resultMiddleware(async (req) => {
  return pipe(
    findUser(req.params.id),
    flatMap(user => 
      hasPermission(user, 'view')
        ? success(user)
        : failure(authorizationError('Cannot view user'))
    )
  )
}))
```

### React Component Error States

**Problem: Boilerplate Error Handling**
```typescript
// ❌ Traditional approach - lots of boilerplate
function UserProfile({ id }: { id: string }) {
  const [user, setUser] = useState<User | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<Error | null>(null)
  
  useEffect(() => {
    setLoading(true)
    setError(null)
    
    fetchUser(id)
      .then(setUser)
      .catch(setError)
      .finally(() => setLoading(false))
  }, [id])
  
  if (loading) return <Spinner />
  if (error) return <ErrorMessage error={error} />
  if (!user) return null
  
  return <UserCard user={user} />
}
```

**Solution: Result-Based Hook**
```typescript
// ✅ With Result<T> - cleaner component code
function useUser(id: string) {
  const [result, setResult] = useState<Result<User> | null>(null)
  
  useEffect(() => {
    fetchUser(id).then(setResult)
  }, [id])
  
  return result
}

function UserProfile({ id }: { id: string }) {
  const userResult = useUser(id)
  
  if (!userResult) return <Spinner />
  
  return match(
    user => <UserCard user={user} />,
    error => <ErrorBoundary error={error} onRetry={() => window.location.reload()} />,
    userResult
  )
}
```

### Form Validation Flow

**Problem: Mixed Validation Logic**
```typescript
// ❌ Validation mixed with business logic
async function submitRegistration(data: FormData) {
  // Validation scattered throughout
  if (!data.email || !isValidEmail(data.email)) {
    throw new Error('Invalid email')
  }
  
  if (data.password.length < 8) {
    throw new Error('Password too short')
  }
  
  const existing = await checkEmailExists(data.email)
  if (existing) {
    throw new Error('Email already registered')
  }
  
  // More validation...
  
  return await createUser(data)
}
```

**Solution: Composable Validation Pipeline**
```typescript
// ✅ With Result<T> - clear validation pipeline
const validateEmail = (data: FormData): Result<FormData> =>
  isValidEmail(data.email)
    ? success(data)
    : failure(validationError('Invalid email format'))

const validatePassword = (data: FormData): Result<FormData> =>
  data.password.length >= 8
    ? success(data)
    : failure(validationError('Password must be at least 8 characters'))

const checkEmailAvailable = async (data: FormData): Promise<Result<FormData>> => {
  const exists = await emailExists(data.email)
  return exists
    ? failure(validationError('Email already registered'))
    : success(data)
}

// Compose validation pipeline
const submitRegistration = (data: FormData): Promise<Result<User>> =>
  pipe(
    success(data),
    flatMap(validateEmail),
    flatMap(validatePassword),
    flatMap(checkEmailAvailable),
    flatMap(createUser)
  )

// Usage shows all possible errors
const result = await submitRegistration(formData)
match(
  user => showSuccessMessage('Registration complete!'),
  error => showFieldError(error), // Specific field errors
  result
)
```

### Database Repository Pattern

**Problem: Null Checks Everywhere**
```typescript
// ❌ Traditional repository - nulls and exceptions
class UserRepository {
  async findById(id: string): Promise<User | null> {
    const user = await db.query('SELECT * FROM users WHERE id = ?', [id])
    return user.rows[0] || null
  }
  
  async updateUser(id: string, data: UpdateData): Promise<User> {
    const user = await this.findById(id)
    if (!user) {
      throw new Error('User not found')
    }
    
    const updated = await db.query('UPDATE users SET ...', [data])
    if (updated.rowCount === 0) {
      throw new Error('Update failed')
    }
    
    return updated.rows[0]
  }
}
```

**Solution: Result-Based Repository**
```typescript
// ✅ With Result<T> - explicit error handling
class UserRepository {
  async findById(id: string): Promise<Result<User>> {
    try {
      const result = await db.query('SELECT * FROM users WHERE id = ?', [id])
      return result.rows[0]
        ? success(result.rows[0])
        : failure(resourceError(`User ${id} not found`))
    } catch (error) {
      return failure(systemError('Database query failed', { error }))
    }
  }
  
  async updateUser(id: string, data: UpdateData): Promise<Result<User>> {
    return pipe(
      await this.findById(id),
      flatMap(async user => {
        try {
          const updated = await db.query('UPDATE users SET ...', [data])
          return updated.rowCount > 0
            ? success(updated.rows[0])
            : failure(systemError('Update failed'))
        } catch (error) {
          return failure(systemError('Database error', { error }))
        }
      })
    )
  }
}

// Usage is much clearer
const updateResult = await repo.updateUser(id, { name: 'New Name' })
match(
  user => console.log('Updated:', user),
  error => {
    if (error.code === 'USER_NOT_FOUND') {
      // Handle missing user
    } else {
      // Handle database errors
    }
  },
  updateResult
)
```

### API Client with Retry Logic

**Problem: Complex Retry Logic**
```typescript
// ❌ Traditional approach - retry logic mixed with business logic
async function fetchWithRetry(url: string, maxRetries = 3): Promise<Response> {
  let lastError: Error | null = null
  
  for (let i = 0; i < maxRetries; i++) {
    try {
      const response = await fetch(url)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }
      return response
    } catch (error) {
      lastError = error as Error
      if (i < maxRetries - 1) {
        await sleep(Math.pow(2, i) * 1000) // exponential backoff
      }
    }
  }
  
  throw lastError || new Error('Failed after retries')
}
```

**Solution: Declarative Retry with Result**
```typescript
// ✅ With Result<T> - clean separation of concerns
const fetchApi = async (url: string): Promise<Result<Response>> => {
  try {
    const response = await fetch(url)
    return response.ok
      ? success(response)
      : failure(networkError(`HTTP ${response.status}`))
  } catch (error) {
    return failure(networkError('Network request failed', { error }))
  }
}

// Retry logic is separate and reusable
const withRetry = async <T>(
  operation: () => Promise<Result<T>>,
  maxRetries = 3
): Promise<Result<T>> => {
  let result = await operation()
  
  for (let i = 0; i < maxRetries && isFailure(result); i++) {
    const error = getError(result)!
    
    // Only retry certain categories
    if (getRetryStrategy(error.category).shouldRetry) {
      await sleep(Math.pow(2, i) * 1000)
      result = await operation()
    } else {
      break // Don't retry validation errors, etc.
    }
  }
  
  return result
}

// Clean usage
const result = await withRetry(() => fetchApi('/api/data'))
```

### Configuration Loading Pattern

**Problem: Configuration Chaos**
```typescript
// ❌ Traditional approach - mixed concerns
function loadConfig(): Config {
  let config: any = {}
  
  // Try to load from file
  try {
    const fileContent = fs.readFileSync('./config.json', 'utf-8')
    config = JSON.parse(fileContent)
  } catch (error) {
    console.warn('No config file found, using defaults')
  }
  
  // Override with env vars
  if (process.env.API_URL) {
    config.api = { url: process.env.API_URL }
  }
  
  // Validate
  if (!config.api?.url) {
    throw new Error('API URL is required')
  }
  
  return config as Config
}
```

**Solution: Fluent Configuration Pipeline**
```typescript
// ✅ With fluent builder - clear, procedural flow
const config = ConfigBuilder
  .fromFile('./config.json')
  .merge(
    ConfigBuilder.fromEnvironment('APP_'),
    ConfigBuilder.fromObject(defaultConfig)
  )
  .validate(configSchema)
  .build()

// Each step is clear and testable
// Errors thrown at the exact step that fails
// Natural precedence: file < environment < defaults
```

## Key Benefits Demonstrated

1. **Type Safety**: Errors are visible in function signatures
2. **Composability**: Complex operations built from simple pieces
3. **Consistency**: Same error handling pattern everywhere
4. **Testability**: Each piece can be tested in isolation
5. **Developer Experience**: Clear what can fail and why

These patterns show how Result<T> provides systematic error handling that scales from simple functions to complex applications.