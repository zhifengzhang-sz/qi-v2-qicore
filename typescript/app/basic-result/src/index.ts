#!/usr/bin/env node

import {
  Err,
  Ok,
  type QiError,
  type Result,
  flatMap,
  map,
  match,
  unwrapOr,
  validationError,
} from '@qi/base'

// === Basic Result Creation ===

function divide(a: number, b: number): Result<number, QiError> {
  if (b === 0) {
    return Err(validationError('Division by zero'))
  }
  return Ok(a / b)
}

function sqrt(x: number): Result<number, QiError> {
  if (x < 0) {
    return Err(validationError('Cannot take square root of negative number'))
  }
  return Ok(Math.sqrt(x))
}

// === Transformation Examples ===

function demonstrateMap() {
  console.log('\n=== Map Operations ===')

  const number = Ok(42)
  const doubled = map((x: number) => x * 2, number)
  const formatted = map((x: number) => `Result: ${x}`, doubled)

  console.log('Original:', number)
  console.log('Doubled:', doubled)
  console.log('Formatted:', formatted)

  // Map on error does nothing
  const error = Err(validationError('Something went wrong'))
  const mappedError = map((x: number) => x * 2, error)
  console.log('Mapped error:', mappedError)
}

function demonstrateFlatMap() {
  console.log('\n=== FlatMap Operations ===')

  // Chain operations that might fail
  const result1 = flatMap(sqrt, divide(16, 4))
  console.log('sqrt(16/4):', result1)

  // Error in first operation
  const result2 = flatMap(sqrt, divide(16, 0))
  console.log('sqrt(16/0):', result2)

  // Error in second operation
  const result3 = flatMap(sqrt, divide(-16, 4))
  console.log('sqrt(-16/4):', result3)
}

// === Pattern Matching ===

function demonstratePatternMatching() {
  console.log('\n=== Pattern Matching ===')

  const results = [
    Ok(42),
    Err(validationError('Something went wrong')),
    Ok('hello'),
    Err(validationError('Another error')),
  ]

  for (const [index, result] of results.entries()) {
    const message = match(
      (value: unknown) => `Success: ${value}`,
      (error: QiError) => `Error: ${error.message}`,
      result
    )

    console.log(`Result ${index + 1}: ${message}`)
  }
}

// === Safe Value Extraction ===

function demonstrateExtraction() {
  console.log('\n=== Safe Value Extraction ===')

  const success = Ok(42)
  const failure = Err(validationError('Failed'))

  // Safe extraction with defaults
  console.log('Success with default:', unwrapOr(0, success))
  console.log('Failure with default:', unwrapOr(0, failure))

  // Type guards
  if (success.tag === 'success') {
    console.log('Success value:', success.value)
  }

  if (failure.tag === 'failure') {
    console.log('Failure error:', failure.error.message)
  }
}

// === Real-World Example ===

interface User {
  id: number
  name: string
  email: string
}

interface Profile {
  userId: number
  bio: string
  avatar: string
}

function fetchUser(id: number): Result<User, QiError> {
  if (id <= 0) {
    return Err(validationError('Invalid user ID'))
  }

  return Ok({
    id,
    name: `User ${id}`,
    email: `user${id}@example.com`,
  })
}

function fetchProfile(userId: number): Result<Profile, QiError> {
  if (userId === 999) {
    return Err(validationError('Profile not found'))
  }

  return Ok({
    userId,
    bio: `Bio for user ${userId}`,
    avatar: `avatar-${userId}.jpg`,
  })
}

function getUserWithProfile(id: number): Result<{ user: User; profile: Profile }, QiError> {
  return flatMap(
    (user: User) => flatMap((profile: Profile) => Ok({ user, profile }), fetchProfile(user.id)),
    fetchUser(id)
  )
}

function demonstrateRealWorld() {
  console.log('\n=== Real-World Example ===')

  const scenarios = [
    { id: 1, description: 'Valid user with profile' },
    { id: 0, description: 'Invalid user ID' },
    { id: 999, description: 'Valid user but no profile' },
  ]

  for (const { id, description } of scenarios) {
    console.log(`\nScenario: ${description}`)

    const result = getUserWithProfile(id)

    const message = match(
      ({ user, profile }: { user: User; profile: Profile }) =>
        `User: ${user.name} (${user.email}), Bio: ${profile.bio}`,
      (error: QiError) => `Failed: ${error.message}`,
      result
    )

    console.log(message)
  }
}

// === Main Application ===

function main() {
  console.log('🚀 QiCore Foundation - Basic Result Examples')
  console.log('='.repeat(50))

  const scenario = process.argv[3] || 'all'

  switch (scenario) {
    case 'map':
      demonstrateMap()
      break
    case 'flatmap':
      demonstrateFlatMap()
      break
    case 'pattern':
      demonstratePatternMatching()
      break
    case 'extract':
      demonstrateExtraction()
      break
    case 'real':
      demonstrateRealWorld()
      break
    default:
      demonstrateMap()
      demonstrateFlatMap()
      demonstratePatternMatching()
      demonstrateExtraction()
      demonstrateRealWorld()
  }

  console.log('\n✨ Done! Try running with different scenarios:')
  console.log('  bun run dev -- map')
  console.log('  bun run dev -- flatmap')
  console.log('  bun run dev -- pattern')
  console.log('  bun run dev -- extract')
  console.log('  bun run dev -- real')
}

main()
