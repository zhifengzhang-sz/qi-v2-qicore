#!/usr/bin/env node

/**
 * QiCore Foundation - Error Handling Example
 *
 * This example demonstrates comprehensive error handling patterns including:
 * - Structured error creation with QiError
 * - Error categorization and context
 * - Error chaining and causality
 * - Retry strategies and circuit breakers
 * - Service layer error handling
 * - API error responses
 */

import {
  Err,
  Ok,
  type QiError,
  type Result,
  type ErrorCategory,
  authorizationError,
  businessError,
  getRetryStrategy,
  match,
  networkError,
  systemError,
  timeoutError,
  validationError,
  withContext,
} from '@qi/base'

// Utility functions for error handling (implementation-specific, not in contracts)
const createAggregateError = (
  message: string,
  errors: QiError[],
  _category: ErrorCategory = 'SYSTEM'
): QiError => {
  return systemError(message, {
    aggregatedErrors: errors.map((e) => ({ code: e.code, message: e.message })),
    errorCount: errors.length,
  })
}

const chainError = (parentError: QiError, childError: QiError): QiError => {
  return withContext({ cause: childError }, parentError)
}

const formatErrorChain = (error: QiError): string => {
  const chain: string[] = []
  const current = error

  chain.push(`${current.code}: ${current.message}`)

  // Look for cause in context (this is implementation-specific)
  if (current.context.cause && typeof current.context.cause === 'object') {
    const cause = current.context.cause as QiError
    chain.push(`  Caused by: ${cause.code}: ${cause.message}`)
  }

  return chain.join('\n')
}

const getRootCause = (error: QiError): QiError => {
  let current = error

  // Keep following the cause chain
  while (current.context.cause && typeof current.context.cause === 'object') {
    current = current.context.cause as QiError
  }

  return current
}

// Setup logger for demonstrations
interface SimpleLogger {
  info: (message: string, ...args: unknown[]) => void
  error: (message: string, ...args: unknown[]) => void
  warn: (message: string, ...args: unknown[]) => void
}

const logger: SimpleLogger = {
  info: console.log,
  error: console.error,
  warn: console.warn,
}

/**
 * User interface for examples
 */
interface User {
  id: number
  name: string
  email: string
  age: number
}

/**
 * Example 1: Validation Error Handling
 */
function demonstrateValidationErrors() {
  console.log('\n📝 Validation Error Handling')
  console.log('='.repeat(40))

  const validateUser = (data: unknown): Result<User, QiError> => {
    const errors: QiError[] = []
    const userData = data as Record<string, unknown>

    if (!userData.name || typeof userData.name !== 'string') {
      errors.push(
        validationError('Name is required and must be a string', {
          field: 'name',
          value: userData.name,
          type: typeof userData.name,
        })
      )
    }

    if (!userData.email || typeof userData.email !== 'string') {
      errors.push(
        validationError('Email is required and must be a string', {
          field: 'email',
          value: userData.email,
          type: typeof userData.email,
        })
      )
    } else if (!userData.email.includes('@')) {
      errors.push(
        validationError('Email must contain @ symbol', {
          field: 'email',
          value: userData.email,
          pattern: 'email',
        })
      )
    }

    if (userData.age && (typeof userData.age !== 'number' || userData.age < 0)) {
      errors.push(
        validationError('Age must be a positive number', {
          field: 'age',
          value: userData.age,
          type: typeof userData.age,
        })
      )
    }

    if (errors.length > 0) {
      return Err(createAggregateError('User validation failed', errors, 'VALIDATION'))
    }

    return Ok(userData as unknown as User)
  }

  // Test with valid data
  const validUser = validateUser({ name: 'Alice', email: 'alice@example.com', age: 30 })
  match(
    (user: User) => console.log('✅ Valid user:', user),
    (error: QiError) => console.log('❌ Validation failed:', error.message),
    validUser
  )

  // Test with invalid data
  const invalidUser = validateUser({ name: '', email: 'invalid-email', age: -5 })
  match(
    (user: User) => console.log('✅ Valid user:', user),
    (error: QiError) => {
      console.log('❌ Validation failed:', error.message)
      if (error.context?.errors) {
        const errors = error.context.errors as QiError[]
        errors.forEach((err, index) => {
          console.log(`  ${index + 1}. ${err.message}`)
        })
      }
    },
    invalidUser
  )
}

/**
 * Example 2: Network Error Handling
 */
async function demonstrateNetworkErrors() {
  console.log('\n🌐 Network Error Handling')
  console.log('='.repeat(40))

  const fetchUserData = async (id: number): Promise<Result<User, QiError>> => {
    try {
      // Simulate different network scenarios
      if (id === 404) {
        return Err(
          networkError('User not found', {
            endpoint: `/api/users/${id}`,
            statusCode: 404,
            method: 'GET',
          })
        )
      }

      if (id === 500) {
        return Err(
          networkError('Internal server error', {
            endpoint: `/api/users/${id}`,
            statusCode: 500,
            method: 'GET',
          })
        )
      }

      if (id === 0) {
        throw new Error('Network connection failed')
      }

      // Simulate successful response
      const user: User = {
        id,
        name: `User ${id}`,
        email: `user${id}@example.com`,
        age: 25,
      }

      return Ok(user)
    } catch (error) {
      return Err(
        networkError('Network request failed', {
          endpoint: `/api/users/${id}`,
          originalError: error instanceof Error ? error.message : 'Unknown error',
        })
      )
    }
  }

  // Test different scenarios
  const scenarios = [
    { id: 1, description: 'Valid user' },
    { id: 404, description: 'User not found' },
    { id: 500, description: 'Server error' },
    { id: 0, description: 'Network failure' },
  ]

  for (const { id, description } of scenarios) {
    console.log(`\n🔍 Testing: ${description}`)
    const result = await fetchUserData(id)

    match(
      (user: User) => console.log('✅ Success:', user),
      (error: QiError) => {
        console.log('❌ Error:', error.message)
        console.log('   Category:', error.category)
        console.log('   Context:', error.context)
      },
      result
    )
  }
}

/**
 * Example 3: Error Chaining
 */
function demonstrateErrorChaining() {
  console.log('\n🔗 Error Chaining')
  console.log('='.repeat(40))

  // Simulate a chain of operations that can fail
  const connectToDatabase = (): Result<string, QiError> => {
    return Err(
      systemError('Database connection failed', {
        host: 'localhost',
        port: 5432,
        database: 'myapp',
      })
    )
  }

  const fetchUserFromDatabase = (id: number): Result<User, QiError> => {
    const connectionResult = connectToDatabase()

    if (connectionResult.tag === 'failure') {
      return Err(
        chainError(
          businessError('Failed to fetch user from database', {
            userId: id,
            operation: 'fetchUser',
          }),
          connectionResult.error
        )
      )
    }

    // This won't be reached in this example
    return Ok({ id, name: `User ${id}`, email: `user${id}@example.com`, age: 30 })
  }

  const getUserProfile = (id: number): Result<User, QiError> => {
    const userResult = fetchUserFromDatabase(id)

    if (userResult.tag === 'failure') {
      return Err(
        chainError(
          businessError('Failed to fetch user profile', {
            userId: id,
            operation: 'getUserProfile',
          }),
          userResult.error
        )
      )
    }

    return userResult
  }

  // Test error chaining
  const result = getUserProfile(123)

  match(
    (user: User) => console.log('✅ Success:', user),
    (error: QiError) => {
      console.log('❌ Error Chain:')
      console.log(formatErrorChain(error))

      const rootCause = getRootCause(error)
      console.log('\n🔍 Root Cause:', rootCause.message)
      console.log('   Category:', rootCause.category)
    },
    result
  )
}

/**
 * Example 4: Retry Strategies
 */
function demonstrateRetryStrategies() {
  console.log('\n🔄 Retry Strategies')
  console.log('='.repeat(40))

  let attemptCount = 0

  const unstableOperation = (): Result<string, QiError> => {
    attemptCount++

    if (attemptCount < 3) {
      return Err(
        networkError('Temporary network failure', {
          attempt: attemptCount,
          maxAttempts: 3,
        })
      )
    }

    return Ok('Operation succeeded!')
  }

  const withRetry = <T>(
    operation: () => Result<T, QiError>,
    maxRetries = 3
  ): Result<T, QiError> => {
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      console.log(`🔄 Attempt ${attempt}/${maxRetries}`)

      const result = operation()

      if (result.tag === 'success') {
        console.log('✅ Operation succeeded')
        return result
      }

      const strategy = getRetryStrategy(result.error.category)

      if (strategy.strategy === 'never') {
        console.log('❌ Error not retryable:', result.error.message)
        return result
      }

      if (attempt >= maxRetries) {
        console.log('❌ Max retries exceeded')
        return Err(
          systemError('Maximum retry attempts exceeded', {
            maxRetries,
            lastError: result.error,
          })
        )
      }

      const backoffMs = strategy.strategy === 'exponential_backoff' ? attempt * 1000 : 500
      console.log(`⏳ Retrying in ${backoffMs}ms... (${strategy.strategy})`)
      // In real implementation, you'd wait here
    }

    return Err(systemError('Unexpected error in retry logic'))
  }

  // Test retry with network error (retryable)
  console.log('\n🔄 Testing retryable network error:')
  attemptCount = 0
  const networkResult = withRetry(unstableOperation)

  match(
    (value: string) => console.log('✅ Final result:', value),
    (error: QiError) => console.log('❌ Final error:', error.message),
    networkResult
  )

  // Test retry with validation error (not retryable)
  console.log('\n🔄 Testing non-retryable validation error:')
  const validationResult = withRetry(() => Err(validationError('Invalid input data')))

  match(
    (value: string) => console.log('✅ Final result:', value),
    (error: QiError) => console.log('❌ Final error:', error.message),
    validationResult
  )
}

/**
 * Example 5: Service Layer Error Handling
 */
async function demonstrateServiceLayerErrors() {
  console.log('\n🏗️  Service Layer Error Handling')
  console.log('='.repeat(40))

  class UserService {
    private users: User[] = [
      { id: 1, name: 'Alice', email: 'alice@example.com', age: 30 },
      { id: 2, name: 'Bob', email: 'bob@example.com', age: 25 },
    ]

    async findById(id: number): Promise<Result<User, QiError>> {
      // Simulate database lookup delay
      await new Promise((resolve) => setTimeout(resolve, 100))

      const user = this.users.find((u) => u.id === id)

      if (!user) {
        return Err(
          businessError('User not found', {
            userId: id,
            operation: 'findById',
          })
        )
      }

      return Ok(user)
    }

    async updateUser(id: number, updates: Partial<User>): Promise<Result<User, QiError>> {
      const userResult = await this.findById(id)

      if (userResult.tag === 'failure') {
        return Err(
          chainError(
            businessError('Failed to update user', {
              userId: id,
              operation: 'updateUser',
            }),
            userResult.error
          )
        )
      }

      // Validate updates
      if (updates.email && !updates.email.includes('@')) {
        return Err(
          validationError('Email must contain @ symbol', {
            field: 'email',
            value: updates.email,
          })
        )
      }

      if (updates.age && updates.age < 0) {
        return Err(
          validationError('Age must be positive', {
            field: 'age',
            value: updates.age,
          })
        )
      }

      // Update user
      const updatedUser = { ...userResult.value, ...updates }
      const userIndex = this.users.findIndex((u) => u.id === id)
      this.users[userIndex] = updatedUser

      return Ok(updatedUser)
    }

    async deleteUser(id: number): Promise<Result<void, QiError>> {
      const userResult = await this.findById(id)

      if (userResult.tag === 'failure') {
        return Err(
          chainError(
            businessError('Failed to delete user', {
              userId: id,
              operation: 'deleteUser',
            }),
            userResult.error
          )
        )
      }

      this.users = this.users.filter((u) => u.id !== id)
      return Ok(undefined)
    }
  }

  // Test service operations
  const userService = new UserService()

  const testOperations = async () => {
    console.log('\n🔍 Testing user service operations:')

    // Test successful find
    const findResult = await userService.findById(1)
    match(
      (user: User) => console.log('✅ Found user:', user.name),
      (error: QiError) => console.log('❌ Find failed:', error.message),
      findResult
    )

    // Test user not found
    const notFoundResult = await userService.findById(999)
    match(
      (user: User) => console.log('✅ Found user:', user.name),
      (error: QiError) => console.log('❌ Find failed:', error.message),
      notFoundResult
    )

    // Test successful update
    const updateResult = await userService.updateUser(1, { name: 'Alice Updated' })
    match(
      (user: User) => console.log('✅ Updated user:', user.name),
      (error: QiError) => console.log('❌ Update failed:', error.message),
      updateResult
    )

    // Test update validation error
    const invalidUpdateResult = await userService.updateUser(1, { email: 'invalid-email' })
    match(
      (user: User) => console.log('✅ Updated user:', user.name),
      (error: QiError) => console.log('❌ Update failed:', error.message),
      invalidUpdateResult
    )

    // Test update non-existent user
    const updateNonExistentResult = await userService.updateUser(999, { name: 'New Name' })
    match(
      (user: User) => console.log('✅ Updated user:', user.name),
      (error: QiError) => {
        console.log('❌ Update failed:', error.message)
        console.log('   Error chain:')
        console.log(formatErrorChain(error))
      },
      updateNonExistentResult
    )
  }

  await testOperations()
}

/**
 * Example 6: Error Context and Logging
 */
function demonstrateErrorLogging() {
  console.log('\n📋 Error Context and Logging')
  console.log('='.repeat(40))

  const processUserRequest = (userId: number, action: string): Result<string, QiError> => {
    // Simulate different error scenarios
    if (userId <= 0) {
      return Err(
        validationError('User ID must be positive', {
          userId,
          action,
          timestamp: new Date().toISOString(),
        })
      )
    }

    if (action === 'forbidden') {
      return Err(
        authorizationError('User not authorized for this action', {
          userId,
          action,
          requiredPermission: 'admin',
          userPermissions: ['read', 'write'],
        })
      )
    }

    if (action === 'timeout') {
      return Err(
        timeoutError('Request processing timed out', {
          userId,
          action,
          timeoutMs: 5000,
          elapsedMs: 5100,
        })
      )
    }

    return Ok(`Successfully processed ${action} for user ${userId}`)
  }

  const scenarios = [
    { userId: 1, action: 'read', description: 'Valid request' },
    { userId: -1, action: 'read', description: 'Invalid user ID' },
    { userId: 1, action: 'forbidden', description: 'Forbidden action' },
    { userId: 1, action: 'timeout', description: 'Request timeout' },
  ]

  for (const { userId, action, description } of scenarios) {
    console.log(`\n🔍 Testing: ${description}`)
    const result = processUserRequest(userId, action)

    match(
      (message: string) => {
        console.log('✅ Success:', message)
        logger.info('Request processed successfully', {
          userId,
          action,
          result: message,
        })
      },
      (error: QiError) => {
        console.log('❌ Error:', error.message)
        console.log('   Category:', error.category)
        console.log('   Context:', error.context)

        // Log error with structured context
        logger.error('Request processing failed', {
          errorCode: error.code,
          errorMessage: error.message,
          errorCategory: error.category,
          errorContext: error.context,
          userId,
          action,
        })
      },
      result
    )
  }
}

/**
 * Main application
 */
async function main() {
  console.log('🚀 QiCore Foundation - Error Handling Examples')
  console.log('='.repeat(60))

  const scenario = process.argv[2] || 'all'

  switch (scenario) {
    case 'validation':
      demonstrateValidationErrors()
      break
    case 'network':
      await demonstrateNetworkErrors()
      break
    case 'chain':
      demonstrateErrorChaining()
      break
    case 'retry':
      demonstrateRetryStrategies()
      break
    case 'service':
      await demonstrateServiceLayerErrors()
      break
    case 'logging':
      demonstrateErrorLogging()
      break
    default:
      demonstrateValidationErrors()
      await demonstrateNetworkErrors()
      demonstrateErrorChaining()
      demonstrateRetryStrategies()
      await demonstrateServiceLayerErrors()
      demonstrateErrorLogging()
  }

  console.log('\n✨ Error handling examples completed!')
  console.log('\nTry running specific scenarios:')
  console.log('  bun run dev -- validation')
  console.log('  bun run dev -- network')
  console.log('  bun run dev -- chain')
  console.log('  bun run dev -- retry')
  console.log('  bun run dev -- service')
  console.log('  bun run dev -- logging')
}

main().catch(console.error)
