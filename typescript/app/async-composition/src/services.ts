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

// Simulation constants
const SIMULATION_CONSTANTS = {
  FAILURE_RATES: {
    CONFIG_LOAD: 0.05,
    USER_FETCH: 0.1,
    POST_FETCH: 0.15,
    USER_SAVE: 0.05,
    NOTIFICATION: 0.1,
  },
  LATENCY_RANGES: {
    CONFIG_LOAD: { base: 100, variance: 0 },
    USER_FETCH: { base: 200, variance: 300 },
    POST_FETCH: { base: 300, variance: 200 },
    USER_SAVE: { base: 150, variance: 100 },
    USER_PROCESS: { base: 50, variance: 50 },
    NOTIFICATION: { base: 100, variance: 200 },
  },
  POST_COUNT: { min: 2, max: 5 },
} as const

// Random failure simulation
const maybeFailure = (failureRate: number) => Math.random() < failureRate

// Random latency simulation
const randomLatency = (base: number, variance: number) => base + Math.random() * variance

// Random count in range
const randomCount = (min: number, max: number) => Math.floor(Math.random() * (max - min + 1)) + min

/**
 * Load application configuration
 */
export async function loadConfig(): Promise<Result<AppConfig, QiError>> {
  await delay(SIMULATION_CONSTANTS.LATENCY_RANGES.CONFIG_LOAD.base)

  if (maybeFailure(SIMULATION_CONSTANTS.FAILURE_RATES.CONFIG_LOAD)) {
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
  const latency = randomLatency(
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_FETCH.base,
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_FETCH.variance
  )
  await delay(latency)

  if (userId <= 0 || userId > 10) {
    return failure(validationError(`Invalid user ID: ${userId}`))
  }

  if (maybeFailure(SIMULATION_CONSTANTS.FAILURE_RATES.USER_FETCH)) {
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
  const latency = randomLatency(
    SIMULATION_CONSTANTS.LATENCY_RANGES.POST_FETCH.base,
    SIMULATION_CONSTANTS.LATENCY_RANGES.POST_FETCH.variance
  )
  await delay(latency)

  if (maybeFailure(SIMULATION_CONSTANTS.FAILURE_RATES.POST_FETCH)) {
    return failure(networkError(`Failed to fetch posts for user ${userId}`))
  }

  // Simulate 2-5 posts per user
  const postCount = randomCount(
    SIMULATION_CONSTANTS.POST_COUNT.min,
    SIMULATION_CONSTANTS.POST_COUNT.max
  )
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
  const latency = randomLatency(
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_SAVE.base,
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_SAVE.variance
  )
  await delay(latency)

  if (maybeFailure(SIMULATION_CONSTANTS.FAILURE_RATES.USER_SAVE)) {
    return failure(create('DATABASE_ERROR', `Failed to save user ${user.id}`, 'SYSTEM'))
  }

  return success(user)
}

/**
 * Process user data (simulate some business logic)
 */
export async function processUserData(user: User): Promise<Result<User, QiError>> {
  const latency = randomLatency(
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_PROCESS.base,
    SIMULATION_CONSTANTS.LATENCY_RANGES.USER_PROCESS.variance
  )
  await delay(latency)

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
  const latency = randomLatency(
    SIMULATION_CONSTANTS.LATENCY_RANGES.NOTIFICATION.base,
    SIMULATION_CONSTANTS.LATENCY_RANGES.NOTIFICATION.variance
  )
  await delay(latency)

  if (maybeFailure(SIMULATION_CONSTANTS.FAILURE_RATES.NOTIFICATION)) {
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
