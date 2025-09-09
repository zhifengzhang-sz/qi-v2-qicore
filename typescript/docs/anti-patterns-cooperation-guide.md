# Anti-Patterns Cooperation Guide

## Problem: Introducing Anti-Patterns Under Pressure

### Root Cause Analysis

When encountering TypeScript errors or technical challenges, there's a tendency to:

1. **Panic and abandon established patterns** instead of thinking systematically
2. **Revert to imperative/familiar approaches** rather than staying within architectural boundaries
3. **Introduce inconsistencies** that violate codebase principles

### Example: The Cache Cleanup Anti-Pattern

**Problem Encountered:**
```typescript
// TypeScript Error:
Type 'Result<void, QiError>' is not assignable to type 'Result<void, CacheError>'
```

**Wrong Response (Anti-Pattern):**
```typescript
// Abandoned functional approach for imperative try/catch
async close(): Promise<Result<void, CacheError>> {
  try {
    await this.redis.quit()
    this.events.removeAllListeners()
    return success(undefined)
  } catch (error) {
    return failure(cacheError('Failed to close Redis cache', { ... }))
  }
}
```

**Correct Response (Functional):**
```typescript
// Created cache-specific functional wrapper
const fromCacheAsyncTryCatch = async <T>(
  operation: () => Promise<T>,
  errorMapper: (error: unknown) => CacheError
): Promise<Result<T, CacheError>> => { ... }

async close(): Promise<Result<void, CacheError>> {
  return fromCacheAsyncTryCatch(
    async () => {
      await this.redis.quit()
      this.events.removeAllListeners()
    },
    (error) => cacheError('Failed to close Redis cache', { ... })
  )
}
```

## Cooperation Guidelines

### For Assistant

#### Before Making Changes
1. **Stop and think** when hitting errors - they're usually solvable within existing patterns
2. **Ask "How would this codebase solve this?"** before introducing new approaches
3. **Analyze the type mismatch** systematically rather than abandoning the approach

#### During Implementation
1. **Stay consistent** with established architectural decisions
2. **Create appropriate abstractions** within the existing paradigm
3. **Don't mix paradigms** (functional vs imperative) in the same codebase

#### Communication
1. **Explain reasoning** for architectural choices
2. **Acknowledge when unsure** about the best approach
3. **Ask for guidance** when patterns seem complex

### For Human

#### Quality Control
1. **Call out anti-patterns immediately** - catches problems early
2. **Reference specific principles** being violated (functional programming, Result types, etc.)
3. **Ask for explanations** when changes seem inconsistent

#### Guidance
1. **Reinforce architectural boundaries** when patterns are violated
2. **Provide examples** of correct patterns when possible
3. **Encourage systematic thinking** over quick fixes

## Key Principles

### Functional Programming Mindset
- **Type errors in functional codebases** are usually solved by creating appropriate functional abstractions
- **Don't abandon functional approach** for imperative solutions
- **Compose functions** rather than embedding logic in try/catch blocks

### Consistency Over Convenience
- **Follow established patterns** even when they require more work
- **Create new abstractions** within the existing paradigm
- **Maintain architectural coherence** across the codebase

### Systematic Problem Solving
1. **Understand the root cause** of type errors
2. **Identify the pattern mismatch** (e.g., QiError vs CacheError)
3. **Create appropriate bridges** (e.g., domain-specific wrappers)
4. **Test the solution** within the existing architecture

## Success Metrics

### What Good Looks Like
- Type errors resolved within established patterns
- Consistent functional composition throughout
- New abstractions that enhance rather than break existing patterns
- Clear reasoning for architectural decisions

### Red Flags
- Mixing imperative and functional styles
- Abandoning Result<T> patterns for raw error handling
- Introducing inconsistencies to "solve" type issues
- Quick fixes that bypass architectural principles

## Examples of Correct Pattern Extensions

### Domain-Specific Error Handling
```typescript
// Instead of abandoning Result<T>, create domain wrappers
const fromCacheAsyncTryCatch = <T>(
  operation: () => Promise<T>,
  errorMapper: (error: unknown) => CacheError
): Promise<Result<T, CacheError>>
```

### Type Bridge Functions
```typescript
// Instead of type casting, create proper type bridges
const mapQiErrorToCacheError = (qiError: QiError): CacheError => {
  // Proper transformation logic
}
```

### Composition Helpers
```typescript
// Instead of manual unwrapping, create composition helpers
const composeWithCacheError = <T, U>(
  operation: (input: T) => Promise<Result<U, QiError>>
): (input: T) => Promise<Result<U, CacheError>>
```

---

This guide serves as a reference for maintaining architectural consistency and avoiding anti-patterns during collaborative development.