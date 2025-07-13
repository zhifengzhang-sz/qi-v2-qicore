/**
 * QiCore Base Component - TypeScript Basic Usage Examples
 * 
 * This file demonstrates practical usage of QiCore Result<T> patterns
 * in TypeScript, showing modern async/await integration and error handling.
 */

import { Result, success, failure, QiError, ErrorCategory } from '@qi/qicore-foundation';

// ==============================================================================
// Basic Result Operations
// ==============================================================================

function divide(x: number, y: number): Result<number, QiError> {
  if (y === 0) {
    return failure(QiError.create(
      'DIVISION_BY_ZERO',
      'Cannot divide by zero',
      'VALIDATION',
      { dividend: x, divisor: y }
    ));
  }
  return success(x / y);
}

function sqrt(x: number): Result<number, QiError> {
  if (x < 0) {
    return failure(QiError.create(
      'NEGATIVE_SQRT',
      'Cannot take square root of negative number',
      'VALIDATION',
      { input: x }
    ));
  }
  return success(Math.sqrt(x));
}

// ==============================================================================
// Chaining Operations with andThen
// ==============================================================================

function safeMathPipeline(x: number, y: number): Result<number, QiError> {
  return success(x)
    .andThen(val => divide(val, y))
    .andThen(val => sqrt(val))
    .inspect(result => console.log(`Computation result: ${result}`))
    .inspectErr(error => console.error(`Computation failed: ${error.message}`));
}

// Usage
const result1 = safeMathPipeline(16, 4); // success(2)
const result2 = safeMathPipeline(16, 0); // failure (division by zero)
const result3 = safeMathPipeline(-16, 4); // failure (negative sqrt)

// ==============================================================================
// Async Operations with TypeScript 5.8
// ==============================================================================

async function fetchUserData(userId: string): Promise<Result<User, QiError>> {
  try {
    const response = await fetch(`/api/users/${userId}`);
    if (!response.ok) {
      return failure(QiError.create(
        'HTTP_ERROR',
        `HTTP ${response.status}: ${response.statusText}`,
        'NETWORK',
        { userId, status: response.status }
      ));
    }
    
    const userData = await response.json();
    return success(userData as User);
  } catch (error) {
    return failure(QiError.fromError(error, 'NETWORK'));
  }
}

async function validateAndProcessUser(userId: string): Promise<Result<ProcessedUser, QiError>> {
  return (await fetchUserData(userId))
    .andThen(async user => {
      if (!user.email) {
        return failure(QiError.create(
          'INVALID_USER',
          'User must have an email',
          'VALIDATION',
          { userId, user }
        ));
      }
      return success(user);
    })
    .andThen(async user => {
      // Simulate async processing
      const processed = await processUser(user);
      return success(processed);
    });
}

// ==============================================================================
// Collection Operations
// ==============================================================================

async function processMultipleUsers(userIds: string[]): Promise<Result<ProcessedUser[], QiError>> {
  // Fetch all users concurrently
  const userPromises = userIds.map(id => fetchUserData(id));
  const userResults = await Promise.all(userPromises);
  
  // Use sequence to fail-fast on first error
  const usersResult = ResultArrayUtils.sequence(userResults);
  
  if (usersResult.kind === 'failure') {
    return usersResult;
  }
  
  // Process all users
  const processedUsers = await Promise.all(
    usersResult.value.map(user => processUser(user))
  );
  
  return success(processedUsers);
}

// Alternative: Collect successes and log failures
async function processUsersWithPartialFailure(userIds: string[]): Promise<ProcessedUser[]> {
  const userPromises = userIds.map(id => fetchUserData(id));
  const userResults = await Promise.all(userPromises);
  
  const [successfulUsers, failedUsers] = ResultArrayUtils.partition(userResults);
  
  // Log failures but continue with successes
  if (failedUsers.length > 0) {
    console.warn(`Failed to fetch ${failedUsers.length} users:`, 
      failedUsers.map(error => error.formatChain()));
  }
  
  return Promise.all(successfulUsers.map(user => processUser(user)));
}

// ==============================================================================
// Error Chain Management
// ==============================================================================

function createNestedError(): Result<string, QiError> {
  const dbError = QiError.create(
    'CONNECTION_FAILED',
    'Database connection lost',
    'SYSTEM',
    { host: 'db.example.com', port: 5432 }
  );
  
  const operationError = QiError.create(
    'QUERY_FAILED',
    'Failed to execute user query',
    'SYSTEM',
    { query: 'SELECT * FROM users', table: 'users' }
  ).withCause(dbError);
  
  const businessError = QiError.create(
    'USER_LOOKUP_FAILED',
    'Could not retrieve user information',
    'BUSINESS',
    { operation: 'getUserProfile', userId: '12345' }
  ).chain(operationError);
  
  return failure(businessError);
}

// Error analysis
const errorResult = createNestedError();
if (errorResult.kind === 'failure') {
  const error = errorResult.error;
  
  console.log('Error chain:', error.formatChain());
  console.log('Root cause:', error.getRootError().message);
  console.log('Is network related:', error.hasCategory('NETWORK'));
  console.log('Full context:', JSON.stringify(error.toJSON(), null, 2));
}

// ==============================================================================
// Pattern Matching and Error Recovery
// ==============================================================================

function handleUserOperation(userId: string): Promise<string> {
  return fetchUserData(userId).then(result => 
    result.match(
      // Success case
      user => `Welcome, ${user.name}!`,
      
      // Error case with recovery strategies
      error => {
        switch (error.category) {
          case 'NETWORK':
            return 'Service temporarily unavailable. Please try again.';
          case 'VALIDATION':
            return 'Invalid user ID provided.';
          case 'SECURITY':
            return 'Access denied.';
          default:
            console.error('Unexpected error:', error.formatChain());
            return 'An unexpected error occurred.';
        }
      }
    )
  );
}

// ==============================================================================
// Type Definitions for Examples
// ==============================================================================

interface User {
  id: string;
  name: string;
  email: string;
  active: boolean;
}

interface ProcessedUser {
  id: string;
  name: string;
  email: string;
  processedAt: Date;
  status: 'active' | 'inactive';
}

async function processUser(user: User): Promise<ProcessedUser> {
  // Simulate async processing
  await new Promise(resolve => setTimeout(resolve, 100));
  
  return {
    id: user.id,
    name: user.name,
    email: user.email,
    processedAt: new Date(),
    status: user.active ? 'active' : 'inactive'
  };
}

// ==============================================================================
// Advanced: Custom Result Utilities
// ==============================================================================

// Retry with exponential backoff
async function retryWithBackoff<T>(
  operation: () => Promise<Result<T, QiError>>,
  maxAttempts: number = 3,
  baseDelayMs: number = 1000
): Promise<Result<T, QiError>> {
  let lastError: QiError;
  
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const result = await operation();
    
    if (result.kind === 'success') {
      return result;
    }
    
    lastError = result.error;
    
    // Don't retry certain error categories
    if (!['NETWORK', 'TIMEOUT', 'SYSTEM'].includes(result.error.category)) {
      break;
    }
    
    if (attempt < maxAttempts) {
      const delay = baseDelayMs * Math.pow(2, attempt - 1);
      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }
  
  return failure(lastError!.withContext({ 
    retryAttempts: maxAttempts,
    finalAttempt: true 
  }));
}

// Usage with retry
async function robustFetchUser(userId: string): Promise<Result<User, QiError>> {
  return retryWithBackoff(
    () => fetchUserData(userId),
    3,  // max attempts
    1000 // 1 second base delay
  );
}

export {
  divide,
  sqrt,
  safeMathPipeline,
  fetchUserData,
  validateAndProcessUser,
  processMultipleUsers,
  processUsersWithPartialFailure,
  createNestedError,
  handleUserOperation,
  retryWithBackoff,
  robustFetchUser
};