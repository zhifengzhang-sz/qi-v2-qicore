# Async Result Composition Example

This example demonstrates the powerful async composition patterns available with QiCore Foundation's async helpers, showing how to eliminate manual Promise/Result unwrapping anti-patterns.

## What This Example Shows

1. **Before/After Comparisons**: Manual unwrapping vs. clean async composition
2. **Real-World Scenarios**: User data loading, API calls, file processing  
3. **Error Handling**: Graceful failure management in async chains
4. **Collection Operations**: Processing multiple async operations
5. **Performance Benefits**: Cleaner, more maintainable code

## Key Patterns Demonstrated

### Async Transformation Operations
- `flatMapAsync` - Chain async operations that return `Result<T>`
- `mapAsync` - Transform values with async functions (auto error-wrapping)
- `matchAsync` - Handle results with async success/error handlers

### Promise Composition Operations  
- `flatMapPromise` - Compose operations on `Promise<Result<T>>`
- `mapPromise` - Map over `Promise<Result<T>>` with sync/async functions
- `matchPromise` - Pattern match on `Promise<Result<T>>`

### Collection Operations
- `sequenceAsync` - Process array of promises, stop at first failure
- `collectAsync` - Process all promises, partition successes/failures

## Running the Example

```bash
# From the async-composition directory
bun install      # Install dependencies
bun run dev      # Run the example
bun run typecheck # Verify types
```

## Files

- `src/index.ts` - Main example with before/after comparisons
- `src/types.ts` - Domain types for the example
- `src/services.ts` - Simulated async services
- `configs/app.yaml` - Example configuration

## Learning Outcomes

After running this example, you'll understand:

1. How async helpers eliminate manual `result.tag === 'success'` checks
2. When to use each async composition function
3. Error handling strategies in async chains
4. Performance and readability benefits of functional composition
5. Real-world patterns for API integration and data processing

## Next Steps

- Try modifying the error scenarios to see how failures propagate
- Add your own async operations using the patterns shown  
- Explore combining async helpers with configuration and logging
- Check out the other examples for more QiCore Foundation patterns