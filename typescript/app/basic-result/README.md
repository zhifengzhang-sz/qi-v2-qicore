# Basic Result Usage Example

This example demonstrates fundamental Result<T> operations and patterns using QiCore Foundation.

## Overview

Learn how to:
- Create and handle Results
- Transform values with map and flatMap
- Handle errors gracefully
- Use pattern matching for control flow
- Extract values safely

## Running the Example

```bash
# Install dependencies
bun install

# Run the example
bun run dev

# Run with different scenarios
bun run dev -- --scenario=success
bun run dev -- --scenario=error
bun run dev -- --scenario=chain
```

## Key Concepts Demonstrated

### 1. Creating Results
```typescript
// Success cases
const number = Ok(42)
const user = Ok({ id: 1, name: 'Alice' })

// Error cases
const error = Err(new Error('Something went wrong'))
const validationError = Err(validationError('INVALID_INPUT', 'Invalid input'))
```

### 2. Transforming Values
```typescript
// Map transforms success values
const doubled = map(number, x => x * 2)

// FlatMap chains operations that return Results
const result = flatMap(getUser(123), user => 
  flatMap(getProfile(user.id), profile => 
    Ok({ user, profile })
  )
)
```

### 3. Error Handling
```typescript
// Pattern matching
const message = match(result, {
  success: value => `Success: ${value}`,
  failure: error => `Error: ${error.message}`
})

// Safe extraction
const value = unwrapOr(result, 'default')
```

## Files Structure

```
src/
├── basic-operations.ts    # Fundamental Result operations
├── transformations.ts     # Map and flatMap examples
├── error-handling.ts      # Error handling patterns
├── pattern-matching.ts    # Pattern matching examples
├── real-world.ts         # Practical use cases
└── index.ts              # Main application
```

## Example Scenarios

### Success Scenario
Shows normal operation flow with all operations succeeding.

### Error Scenario
Demonstrates error propagation and handling.

### Chain Scenario
Shows complex operation chaining with mixed success/failure cases.

## Learning Objectives

After running this example, you'll understand:
- How to create and use Results effectively
- When to use map vs flatMap
- How to handle errors without exceptions
- Pattern matching for control flow
- Safe value extraction techniques