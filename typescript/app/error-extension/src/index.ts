#!/usr/bin/env node

import {
  type QiError,
  type Result,
  success,
  failure,
  match,
  create,
  getRetryStrategy,
  flatMapAsync,
} from '@qi/base'
import { createLogger } from '@qi/core'

// === Domain-Specific Error Types ===

// User domain errors
interface UserError extends QiError {
  category: 'VALIDATION' | 'BUSINESS' | 'AUTHENTICATION' | 'AUTHORIZATION'
  context: {
    userId?: string
    field?: string
    operation?: string
    permissions?: string[]
    userRole?: string
    requiredPermission?: string
    userPermissions?: string[]
    email?: string
    value?: unknown
    minimumAge?: number
    paymentId?: string
    amount?: number
    orderId?: string
    stage?: string
  }
}

// Payment domain errors
interface PaymentError extends QiError {
  category: 'NETWORK' | 'BUSINESS' | 'VALIDATION'
  context: {
    paymentId?: string
    amount?: number
    currency?: string
    provider?: string
    transactionId?: string
    maxAmount?: number
  }
}

// Order domain errors
interface OrderError extends QiError {
  category: 'BUSINESS' | 'VALIDATION' | 'SYSTEM'
  context: {
    orderId?: string
    customerId?: string
    items?: Array<{ id: string; quantity: number }>
    stage?: 'validation' | 'payment' | 'fulfillment'
    totalAmount?: number
    maxAmount?: number
  }
}

// === Error Factory Functions ===

function createUserError(
  code: string,
  message: string,
  category: UserError['category'],
  context: UserError['context'] = {}
): UserError {
  return create(code, message, category, context) as UserError
}

function createPaymentError(
  code: string,
  message: string,
  category: PaymentError['category'],
  context: PaymentError['context'] = {}
): PaymentError {
  return create(code, message, category, context) as PaymentError
}

function createOrderError(
  code: string,
  message: string,
  category: OrderError['category'],
  context: OrderError['context'] = {}
): OrderError {
  return create(code, message, category, context) as OrderError
}

// === Domain Operations ===

interface UserData {
  email?: string
  age?: number
  password?: string
}

interface AuthenticatedUser {
  id: string
  email: string
  role: string
}

async function validateUser(userData: UserData): Promise<Result<UserData, UserError>> {
  if (!userData.email) {
    return failure(
      createUserError('MISSING_EMAIL', 'Email is required', 'VALIDATION', { field: 'email' })
    )
  }

  if (!userData.email.includes('@')) {
    return failure(
      createUserError('INVALID_EMAIL', 'Email must contain @ symbol', 'VALIDATION', {
        field: 'email',
        value: userData.email,
      })
    )
  }

  if (userData.age && userData.age < 18) {
    return failure(
      createUserError('UNDERAGE_USER', 'User must be at least 18 years old', 'BUSINESS', {
        field: 'age',
        value: userData.age,
        minimumAge: 18,
      })
    )
  }

  return success(userData)
}

async function authenticateUser(
  email: string,
  password: string
): Promise<Result<AuthenticatedUser, UserError>> {
  // Simulate authentication logic
  if (email === 'admin@test.com' && password === 'admin123') {
    return success({ id: '1', email, role: 'admin' })
  }

  if (email === 'user@test.com' && password === 'user123') {
    return success({ id: '2', email, role: 'user' })
  }

  return failure(
    createUserError('INVALID_CREDENTIALS', 'Invalid email or password', 'AUTHENTICATION', {
      email,
      operation: 'login',
    })
  )
}

async function authorizeUser(
  user: AuthenticatedUser,
  operation: string
): Promise<Result<AuthenticatedUser, UserError>> {
  const permissions = {
    admin: ['read', 'write', 'delete'],
    user: ['read'],
  }

  const userPermissions = permissions[user.role as keyof typeof permissions] || []

  if (!userPermissions.includes(operation)) {
    return failure(
      createUserError(
        'INSUFFICIENT_PERMISSIONS',
        `User lacks permission for operation: ${operation}`,
        'AUTHORIZATION',
        {
          userId: user.id,
          operation,
          userRole: user.role,
          requiredPermission: operation,
          userPermissions,
        }
      )
    )
  }

  return success(user)
}

interface PaymentData {
  amount: number
  currency?: string
  simulateFailure?: boolean
}

interface PaymentResult {
  transactionId: string
  amount: number
  status: string
}

async function processPayment(
  paymentData: PaymentData
): Promise<Result<PaymentResult, PaymentError>> {
  if (paymentData.amount <= 0) {
    return failure(
      createPaymentError('INVALID_AMOUNT', 'Payment amount must be greater than 0', 'VALIDATION', {
        amount: paymentData.amount,
      })
    )
  }

  if (paymentData.amount > 10000) {
    return failure(
      createPaymentError('AMOUNT_TOO_HIGH', 'Payment amount exceeds maximum limit', 'BUSINESS', {
        amount: paymentData.amount,
        maxAmount: 10000,
      })
    )
  }

  // Simulate network failure
  if (paymentData.simulateFailure) {
    return failure(
      createPaymentError(
        'PAYMENT_GATEWAY_ERROR',
        'Payment gateway temporarily unavailable',
        'NETWORK',
        { provider: 'stripe', transactionId: `tx_${Date.now()}` }
      )
    )
  }

  return success({
    transactionId: `tx_${Date.now()}`,
    amount: paymentData.amount,
    status: 'completed',
  })
}

interface OrderItem {
  id: string
  price: number
  quantity: number
}

interface OrderData {
  customerId: string
  items: OrderItem[]
}

interface OrderResult {
  orderId: string
  customerId: string
  items: OrderItem[]
  totalAmount: number
  status: string
}

async function createOrder(orderData: OrderData): Promise<Result<OrderResult, OrderError>> {
  if (!orderData.items || orderData.items.length === 0) {
    return failure(
      createOrderError('EMPTY_ORDER', 'Order must contain at least one item', 'VALIDATION', {
        stage: 'validation',
      })
    )
  }

  const totalAmount = orderData.items.reduce(
    (sum: number, item: OrderItem) => sum + item.price * item.quantity,
    0
  )

  if (totalAmount > 50000) {
    return failure(
      createOrderError('ORDER_TOO_LARGE', 'Order exceeds maximum value limit', 'BUSINESS', {
        totalAmount,
        maxAmount: 50000,
        items: orderData.items.map((item: OrderItem) => ({ id: item.id, quantity: item.quantity })),
        stage: 'validation',
      })
    )
  }

  return success({
    orderId: `order_${Date.now()}`,
    customerId: orderData.customerId,
    items: orderData.items,
    totalAmount,
    status: 'created',
  })
}

// === Error Handling Strategies ===

function handleUserError(error: UserError, logger: DemoLogger) {
  logger.error(`User Error [${error.code}]:`, undefined, {
    category: error.category,
    context: error.context,
    error: error.message,
  })

  switch (error.category) {
    case 'VALIDATION':
      return {
        status: 400,
        message: error.message,
        field: error.context.field,
      }
    case 'AUTHENTICATION':
      return {
        status: 401,
        message: 'Please login to continue',
        redirectTo: '/login',
      }
    case 'AUTHORIZATION':
      return {
        status: 403,
        message: 'Access denied',
        requiredPermission: error.context.operation,
      }
    case 'BUSINESS':
      return {
        status: 422,
        message: error.message,
        context: error.context,
      }
    default:
      return {
        status: 500,
        message: 'Internal server error',
      }
  }
}

function handlePaymentError(error: PaymentError, logger: DemoLogger) {
  logger.error(`Payment Error [${error.code}]:`, undefined, {
    category: error.category,
    context: error.context,
    error: error.message,
  })

  const retryStrategy = getRetryStrategy(error.category)

  switch (error.category) {
    case 'NETWORK':
      return {
        status: 502,
        message: 'Payment service temporarily unavailable',
        retryAfter: 30,
        canRetry: retryStrategy.strategy !== 'never',
      }
    case 'VALIDATION':
      return {
        status: 400,
        message: error.message,
        canRetry: false,
      }
    case 'BUSINESS':
      return {
        status: 422,
        message: error.message,
        canRetry: false,
        context: error.context,
      }
    default:
      return {
        status: 500,
        message: 'Payment processing failed',
      }
  }
}

function handleOrderError(error: OrderError, logger: DemoLogger) {
  logger.error(`Order Error [${error.code}]:`, undefined, {
    category: error.category,
    context: error.context,
    error: error.message,
  })

  return {
    status: error.category === 'VALIDATION' ? 400 : error.category === 'BUSINESS' ? 422 : 500,
    message: error.message,
    stage: error.context.stage,
    context: error.context,
  }
}

// === Demonstration Functions ===

interface DemoLogger {
  info: (msg: string, ctx?: Record<string, unknown>) => void
  error: (msg: string, err?: Error, ctx?: Record<string, unknown>) => void
}

async function demonstrateUserFlow(logger: DemoLogger) {
  logger.info('=== User Flow Examples ===')

  // Valid user
  const validUser = { email: 'user@test.com', age: 25 }
  const validationResult = await validateUser(validUser)

  match(
    (user) => logger.info('✅ User validation passed:', { user }),
    (error) => logger.error('❌ User validation failed:', undefined, { error: error.message }),
    validationResult
  )

  // Invalid email
  const invalidUser = { email: 'invalid-email', age: 25 }
  const invalidResult = await validateUser(invalidUser)

  match(
    (user) => logger.info('✅ User validation passed:', { user }),
    (error) => {
      const response = handleUserError(error, logger)
      logger.info('Error handled:', { response })
    },
    invalidResult
  )

  // Authentication flow - using proper async composition
  const authResult = await authenticateUser('user@test.com', 'user123')
  const authFlow = await flatMapAsync((user) => authorizeUser(user, 'delete'), authResult)

  match(
    (user) => logger.info('✅ User authorized:', { user }),
    (error) => {
      const response = handleUserError(error, logger)
      logger.info('Authorization failed:', { response })
    },
    authFlow
  )
}

async function demonstratePaymentFlow(logger: DemoLogger) {
  logger.info('=== Payment Flow Examples ===')

  // Valid payment
  const validPayment = { amount: 100, currency: 'USD' }
  const paymentResult = await processPayment(validPayment)

  match(
    (payment) => logger.info('✅ Payment processed:', { payment }),
    (error) => {
      const response = handlePaymentError(error, logger)
      logger.info('Payment failed:', { response })
    },
    paymentResult
  )

  // Network failure simulation
  const failedPayment = { amount: 100, currency: 'USD', simulateFailure: true }
  const failedResult = await processPayment(failedPayment)

  match(
    (payment) => logger.info('✅ Payment processed:', { payment }),
    (error) => {
      const response = handlePaymentError(error, logger)
      logger.info('Payment failed with retry info:', { response })
    },
    failedResult
  )
}

async function demonstrateOrderFlow(logger: DemoLogger) {
  logger.info('=== Order Flow Examples ===')

  // Valid order
  const validOrder = {
    customerId: 'cust_123',
    items: [
      { id: 'item_1', price: 100, quantity: 2 },
      { id: 'item_2', price: 50, quantity: 1 },
    ],
  }
  const orderResult = await createOrder(validOrder)

  match(
    (order) => logger.info('✅ Order created:', { order }),
    (error) => {
      const response = handleOrderError(error, logger)
      logger.info('Order failed:', { response })
    },
    orderResult
  )

  // Order too large
  const largeOrder = {
    customerId: 'cust_123',
    items: [{ id: 'item_1', price: 30000, quantity: 2 }],
  }
  const largeResult = await createOrder(largeOrder)

  match(
    (order) => logger.info('✅ Order created:', { order }),
    (error) => {
      const response = handleOrderError(error, logger)
      logger.info('Large order rejected:', { response })
    },
    largeResult
  )
}

async function demonstrateErrorComposition(logger: DemoLogger) {
  logger.info('=== Error Composition Examples ===')

  // Compose multiple operations with different error types
  async function fullOrderFlow(
    userData: UserData,
    paymentData: PaymentData,
    orderData: OrderData
  ): Promise<
    Result<
      { user: AuthenticatedUser; payment: PaymentResult; order: OrderResult },
      UserError | PaymentError | OrderError
    >
  > {
    // ✅ ACCEPTABLE: Manual early returns for mixed error domain composition
    // Note: This is a valid pattern when composing operations with different error types
    // The union type (UserError | PaymentError | OrderError) makes functional composition
    // complex due to TypeScript's type system limitations with union error types.
    const userResult = await validateUser(userData)
    if (userResult.tag === 'failure') return userResult

    const authResult = await authenticateUser(userData.email || '', userData.password || '')
    if (authResult.tag === 'failure') return authResult

    const authzResult = await authorizeUser(authResult.value, 'write')
    if (authzResult.tag === 'failure') return authzResult

    const paymentResult = await processPayment(paymentData)
    if (paymentResult.tag === 'failure') return paymentResult

    const orderResult = await createOrder(orderData)
    if (orderResult.tag === 'failure') return orderResult

    return success({
      user: authzResult.value,
      payment: paymentResult.value,
      order: orderResult.value,
    })
  }

  // Test full flow
  const userData = { email: 'admin@test.com', password: 'admin123', age: 30 }
  const paymentData = { amount: 250, currency: 'USD' }
  const orderData = {
    customerId: 'cust_123',
    items: [{ id: 'item_1', price: 100, quantity: 2 }],
  }

  const flowResult = await fullOrderFlow(userData, paymentData, orderData)

  match(
    (result) => logger.info('✅ Full order flow completed:', { result }),
    (error) => {
      // Error could be from any domain - handle appropriately
      if (
        error.category === 'VALIDATION' ||
        error.category === 'AUTHENTICATION' ||
        error.category === 'AUTHORIZATION'
      ) {
        const response = handleUserError(error as UserError, logger)
        logger.info('User error in flow:', { response })
      } else if (
        error.category === 'NETWORK' ||
        (error.category === 'BUSINESS' && 'paymentId' in error.context)
      ) {
        const response = handlePaymentError(error as PaymentError, logger)
        logger.info('Payment error in flow:', { response })
      } else if (
        error.category === 'SYSTEM' ||
        (error.category === 'BUSINESS' && 'orderId' in error.context)
      ) {
        const response = handleOrderError(error as OrderError, logger)
        logger.info('Order error in flow:', { response })
      } else {
        logger.error('Unknown error in flow:', undefined, { error: error.message })
      }
    },
    flowResult
  )
}

async function main() {
  const loggerResult = createLogger({ level: 'info', pretty: true })
  if (loggerResult.tag === 'failure') throw new Error('Logger failed')
  const logger = loggerResult.value

  console.log('🚀 QiCore Foundation - Error Extension Examples')
  console.log('==============================================\n')

  await demonstrateUserFlow(logger)
  console.log()

  await demonstratePaymentFlow(logger)
  console.log()

  await demonstrateOrderFlow(logger)
  console.log()

  await demonstrateErrorComposition(logger)
  console.log()

  logger.info('✨ Error extension examples completed!')
  logger.info('💡 Key takeaways:')
  logger.info('   - Domain-specific error types provide better context')
  logger.info('   - Error categories determine retry strategies')
  logger.info('   - Structured error handling improves debugging')
  logger.info('   - Error composition works across different domains')
}

main().catch(console.error)
