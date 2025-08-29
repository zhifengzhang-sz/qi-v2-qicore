/**
 * Simulated async services for demonstration
 */

import {
  success,
  failure,
  networkError,
  validationError,
  create,
  type Result,
  type QiError,
} from '@qi/base'
import type { User, Post, AppConfig } from './types.js'

// Simulate network delay
const delay = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms))

// Random failure simulation
const maybeFailure = (failureRate = 0.1) => Math.random() < failureRate

/**
 * Load application configuration
 */
export async function loadConfig(): Promise<Result<AppConfig, QiError>> {
  await delay(100)

  if (maybeFailure(0.05)) {
    return failure(create('CONFIG_LOAD_ERROR', 'Failed to load configuration', 'SYSTEM'))
  }

  // Simulate loaded config (in real app this would come from files/env)
  const config: AppConfig = {
    app: {
      name: 'Async Composition Demo',
      version: '1.0.0',
      environment: 'development',
    },
    api: {
      baseUrl: 'https://jsonplaceholder.typicode.com',
      timeout: 5000,
      retries: 3,
    },
    database: {
      url: 'postgresql://user:pass@localhost/demo',
      poolSize: 10,
    },
    processing: {
      batchSize: 5,
      maxConcurrency: 3,
    },
    logging: {
      level: 'info',
      pretty: true,
    },
  }

  return success(config)
}

/**
 * Fetch user data from API
 */
export async function fetchUser(userId: number): Promise<Result<User, QiError>> {
  await delay(200 + Math.random() * 300) // Simulate variable latency

  if (userId <= 0 || userId > 10) {
    return failure(validationError(`Invalid user ID: ${userId}`))
  }

  if (maybeFailure(0.1)) {
    return failure(networkError(`Failed to fetch user ${userId}`))
  }

  // Simulate fetched user data
  const user: User = {
    id: userId,
    name: `User ${userId}`,
    email: `user${userId}@example.com`,
    phone: `555-000${userId.toString().padStart(4, '0')}`,
    website: `user${userId}.example.com`,
  }

  return success(user)
}

/**
 * Fetch user's posts from API
 */
export async function fetchUserPosts(userId: number): Promise<Result<Post[], QiError>> {
  await delay(300 + Math.random() * 200)

  if (maybeFailure(0.15)) {
    return failure(networkError(`Failed to fetch posts for user ${userId}`))
  }

  // Simulate 2-5 posts per user
  const postCount = Math.floor(Math.random() * 4) + 2
  const posts: Post[] = Array.from({ length: postCount }, (_, i) => ({
    id: userId * 100 + i + 1,
    userId,
    title: `Post ${i + 1} by User ${userId}`,
    body: `This is the body content for post ${i + 1} by user ${userId}. Lorem ipsum dolor sit amet.`,
  }))

  return success(posts)
}

/**
 * Save user data to database
 */
export async function saveUser(user: User): Promise<Result<User, QiError>> {
  await delay(150 + Math.random() * 100)

  if (maybeFailure(0.05)) {
    return failure(create('DATABASE_ERROR', `Failed to save user ${user.id}`, 'SYSTEM'))
  }

  return success(user)
}

/**
 * Process user data (simulate some business logic)
 */
export async function processUserData(user: User): Promise<Result<User, QiError>> {
  await delay(50 + Math.random() * 50)

  // Simulate validation
  if (!user.email.includes('@')) {
    return failure(validationError(`Invalid email format for user ${user.id}`))
  }

  if (maybeFailure(0.08)) {
    return failure(create('PROCESSING_ERROR', `Failed to process user ${user.id}`, 'BUSINESS'))
  }

  // Return processed user (in real app, this might enrich the data)
  const processedUser: User = {
    ...user,
    name: user.name.toUpperCase(), // Example processing
    email: user.email.toLowerCase(),
  }

  return success(processedUser)
}

/**
 * Send notification (simulate external service call)
 */
export async function sendNotification(
  user: User,
  message: string
): Promise<Result<boolean, QiError>> {
  await delay(100 + Math.random() * 200)

  if (maybeFailure(0.1)) {
    return failure(networkError(`Failed to send notification to ${user.email}`))
  }

  console.log(`📧 Notification sent to ${user.email}: ${message}`)
  return success(true)
}

/**
 * Validate configuration
 */
export function validateConfig(config: AppConfig): Result<AppConfig, QiError> {
  if (config.api.timeout <= 0) {
    return failure(validationError('API timeout must be positive'))
  }

  if (config.processing.batchSize <= 0) {
    return failure(validationError('Batch size must be positive'))
  }

  if (!['development', 'production', 'test'].includes(config.app.environment)) {
    return failure(validationError('Invalid environment'))
  }

  return success(config)
}
