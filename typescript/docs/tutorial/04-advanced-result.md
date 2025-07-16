# Advanced Result Operations

This guide covers advanced Result<T> operations including collection handling, applicative operations, parallel processing, and complex composition patterns.

## Collection Operations

### Sequence - Converting Arrays of Results

The `sequence` function converts an array of Results into a Result of an array:

```typescript
import { sequence, Ok, Err, type Result } from '@qi/qicore-foundation/base'

// Array of Results
const results: Result<number>[] = [Ok(1), Ok(2), Ok(3)]

// Convert to Result of array
const sequenced: Result<number[]> = sequence(results)
// Result<number[]> = Ok([1, 2, 3])

// If any Result is an error, the entire sequence fails
const mixedResults: Result<number>[] = [Ok(1), Err(new Error('failed')), Ok(3)]
const sequencedMixed: Result<number[]> = sequence(mixedResults)
// Result<number[]> = Err(Error('failed'))
```

### Traverse - Map and Sequence Combined

The `traverse` function maps a function over an array and sequences the results:

```typescript
import { traverse, Ok, Err } from '@qi/qicore-foundation/base'

// Function that returns a Result
const parseNumber = (str: string): Result<number> => {
  const num = parseInt(str, 10)
  return isNaN(num) ? Err(new Error(`Invalid number: ${str}`)) : Ok(num)
}

const strings = ['1', '2', '3']
const numbers: Result<number[]> = traverse(strings, parseNumber)
// Result<number[]> = Ok([1, 2, 3])

const badStrings = ['1', 'abc', '3']
const failedNumbers: Result<number[]> = traverse(badStrings, parseNumber)
// Result<number[]> = Err(Error('Invalid number: abc'))
```

### Partition - Separating Success and Failure

The `partition` function separates an array of Results into successes and failures:

```typescript
import { partition, Ok, Err } from '@qi/qicore-foundation/base'

const mixedResults: Result<number>[] = [
  Ok(1),
  Err(new Error('error1')),
  Ok(3),
  Err(new Error('error2')),
  Ok(5)
]

const { successes, failures } = partition(mixedResults)
// successes: number[] = [1, 3, 5]
// failures: Error[] = [Error('error1'), Error('error2')]
```

### Combine2 - Combining Two Results

The `combine2` function combines two Results into a single Result with a tuple:

```typescript
import { combine2, Ok, Err } from '@qi/qicore-foundation/base'

const result1: Result<number> = Ok(42)
const result2: Result<string> = Ok('hello')

const combined: Result<[number, string]> = combine2(result1, result2)
// Result<[number, string]> = Ok([42, 'hello'])

const errorResult: Result<number> = Err(new Error('failed'))
const combinedError: Result<[number, string]> = combine2(errorResult, result2)
// Result<[number, string]> = Err(Error('failed'))
```

## Applicative Operations

### Apply - Function Application

The `apply` function applies a function wrapped in a Result to a value wrapped in a Result:

```typescript
import { apply, pure, Ok, Err } from '@qi/qicore-foundation/base'

// Function in a Result
const addFunction: Result<(x: number) => number> = Ok((x: number) => x + 10)
const value: Result<number> = Ok(5)

const result: Result<number> = apply(addFunction, value)
// Result<number> = Ok(15)

// Curried functions work naturally
const addCurried: Result<(x: number) => (y: number) => number> = 
  Ok((x: number) => (y: number) => x + y)

const partiallyApplied: Result<(y: number) => number> = apply(addCurried, Ok(10))
const finalResult: Result<number> = apply(partiallyApplied, Ok(5))
// Result<number> = Ok(15)
```

### Pure - Lifting Values

The `pure` function lifts a value into a Result context:

```typescript
import { pure } from '@qi/qicore-foundation/base'

const value: Result<number> = pure(42)
// Result<number> = Ok(42)

// Equivalent to Ok(42)
```

### Applicative Validation

Use applicative operations for validation that accumulates errors:

```typescript
import { apply, pure, Err, createAggregateError } from '@qi/qicore-foundation/base'

interface User {
  name: string
  email: string
  age: number
}

// Validation functions
const validateName = (name: string): Result<string> => 
  name.length > 0 ? Ok(name) : Err(validationError('EMPTY_NAME', 'Name cannot be empty'))

const validateEmail = (email: string): Result<string> => 
  email.includes('@') ? Ok(email) : Err(validationError('INVALID_EMAIL', 'Invalid email format'))

const validateAge = (age: number): Result<number> => 
  age >= 0 ? Ok(age) : Err(validationError('NEGATIVE_AGE', 'Age cannot be negative'))

// Applicative validation (accumulates all errors)
const validateUser = (name: string, email: string, age: number): Result<User> => {
  const nameResult = validateName(name)
  const emailResult = validateEmail(email)
  const ageResult = validateAge(age)

  // This would accumulate all validation errors
  // Note: This is conceptual - actual implementation might differ
  return apply(
    apply(
      apply(
        pure((name: string) => (email: string) => (age: number) => ({ name, email, age })),
        nameResult
      ),
      emailResult
    ),
    ageResult
  )
}
```

## Async Operations

### Async Sequence

Process multiple async operations and collect results:

```typescript
import { asyncSequence, fromPromise } from '@qi/qicore-foundation/base'

// Array of async operations
const asyncOperations = [
  fromPromise(fetch('/api/user/1').then(r => r.json())),
  fromPromise(fetch('/api/user/2').then(r => r.json())),
  fromPromise(fetch('/api/user/3').then(r => r.json()))
]

// Wait for all and collect results
const allUsers: Result<User[]> = await asyncSequence(asyncOperations)
```

### Parallel Processing

Process multiple operations in parallel:

```typescript
import { asyncMap, traverse } from '@qi/qicore-foundation/base'

// Process users in parallel
const userIds = [1, 2, 3, 4, 5]

const processUser = async (id: number): Promise<Result<ProcessedUser>> => {
  const userResult = await fetchUser(id)
  return asyncMap(userResult, async user => {
    const profile = await enrichUserProfile(user)
    return { ...user, profile }
  })
}

// Process all users in parallel
const processedUsers: Result<ProcessedUser[]> = await traverse(userIds, processUser)
```

## Complex Composition Patterns

### Railway-Oriented Programming

Build complex pipelines using Result composition:

```typescript
import { flatMap, map, asyncAndThen } from '@qi/qicore-foundation/base'

// Define a pipeline type
type Pipeline<T, U> = (input: T) => Promise<Result<U>>

// Create pipeline stages
const validateInput: Pipeline<any, ValidatedInput> = async (input) => {
  // Validation logic
  return input.isValid ? Ok(input) : Err(validationError('INVALID_INPUT', 'Invalid input'))
}

const enrichData: Pipeline<ValidatedInput, EnrichedData> = async (input) => {
  // Enrichment logic
  return asyncAndThen(await fetchExternalData(input.id), externalData => 
    Ok({ ...input, externalData })
  )
}

const processData: Pipeline<EnrichedData, ProcessedData> = async (data) => {
  // Processing logic
  return Ok(await performComplexProcessing(data))
}

const saveData: Pipeline<ProcessedData, SavedData> = async (data) => {
  // Save logic
  return await saveToDatabase(data)
}

// Compose the pipeline
const fullPipeline: Pipeline<any, SavedData> = async (input) => {
  const step1 = await validateInput(input)
  if (step1.tag === 'failure') return step1

  const step2 = await enrichData(step1.value)
  if (step2.tag === 'failure') return step2

  const step3 = await processData(step2.value)
  if (step3.tag === 'failure') return step3

  return await saveData(step3.value)
}

// Or using async composition
const composedPipeline: Pipeline<any, SavedData> = async (input) => {
  const validated = await validateInput(input)
  return asyncAndThen(validated, async validatedInput => {
    const enriched = await enrichData(validatedInput)
    return asyncAndThen(enriched, async enrichedData => {
      const processed = await processData(enrichedData)
      return asyncAndThen(processed, saveData)
    })
  })
}
```

### Either-Style Composition

Create reusable composition utilities:

```typescript
import { type Result, flatMap, map } from '@qi/qicore-foundation/base'

// Compose multiple transformations
const pipe = <T>(...fns: Array<(x: any) => any>) => (input: T) => 
  fns.reduce((acc, fn) => fn(acc), input)

// Result-aware pipe
const resultPipe = <T>(input: Result<T>) => ({
  map: <U>(fn: (x: T) => U) => resultPipe(map(input, fn)),
  flatMap: <U>(fn: (x: T) => Result<U>) => resultPipe(flatMap(input, fn)),
  filter: (predicate: (x: T) => boolean, error: () => Error) => 
    resultPipe(filter(input, predicate, error)),
  value: () => input
})

// Usage
const result = resultPipe(Ok(42))
  .map(x => x * 2)
  .flatMap(x => x > 50 ? Ok(x) : Err(new Error('Too small')))
  .map(x => `Result: ${x}`)
  .value()
```

### Retry Patterns

Implement retry logic with Result:

```typescript
import { type Result, Err, asyncTryCatch } from '@qi/qicore-foundation/base'

async function withRetry<T>(
  operation: () => Promise<Result<T>>,
  maxRetries: number = 3,
  backoffMs: number = 1000
): Promise<Result<T>> {
  let lastError: Error | undefined
  
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    const result = await operation()
    
    if (result.tag === 'success') {
      return result
    }
    
    lastError = result.error
    
    // Don't retry on validation errors
    if (hasCategory(result.error, 'VALIDATION')) {
      return result
    }
    
    // Wait before retry
    if (attempt < maxRetries) {
      await new Promise(resolve => setTimeout(resolve, backoffMs * (attempt + 1)))
    }
  }
  
  return Err(lastError!)
}

// Usage
const result = await withRetry(
  () => fetchUserData(123),
  3,
  1000
)
```

### Circuit Breaker Pattern

Implement circuit breaker with Result:

```typescript
import { type Result, Ok, Err } from '@qi/qicore-foundation/base'

class CircuitBreaker<T> {
  private failures = 0
  private lastFailureTime?: Date
  private state: 'CLOSED' | 'OPEN' | 'HALF_OPEN' = 'CLOSED'

  constructor(
    private threshold: number = 5,
    private timeout: number = 60000
  ) {}

  async call(operation: () => Promise<Result<T>>): Promise<Result<T>> {
    if (this.state === 'OPEN') {
      if (Date.now() - this.lastFailureTime!.getTime() > this.timeout) {
        this.state = 'HALF_OPEN'
      } else {
        return Err(systemError('CIRCUIT_BREAKER_OPEN', 'Circuit breaker is open'))
      }
    }

    try {
      const result = await operation()
      
      if (result.tag === 'success') {
        this.onSuccess()
        return result
      } else {
        this.onFailure()
        return result
      }
    } catch (error) {
      this.onFailure()
      return Err(systemError('CIRCUIT_BREAKER_ERROR', 'Circuit breaker caught error'))
    }
  }

  private onSuccess() {
    this.failures = 0
    this.state = 'CLOSED'
  }

  private onFailure() {
    this.failures++
    this.lastFailureTime = new Date()
    
    if (this.failures >= this.threshold) {
      this.state = 'OPEN'
    }
  }
}

// Usage
const breaker = new CircuitBreaker<User>(5, 60000)

const fetchUserWithBreaker = (id: number): Promise<Result<User>> => 
  breaker.call(() => fetchUser(id))
```

## Performance Optimization

### Lazy Evaluation

Create lazy Result computations:

```typescript
type LazyResult<T> = () => Result<T>

const lazy = <T>(computation: () => Result<T>): LazyResult<T> => computation

const lazyMap = <T, U>(
  lazyResult: LazyResult<T>,
  fn: (x: T) => U
): LazyResult<U> => 
  () => map(lazyResult(), fn)

const lazyFlatMap = <T, U>(
  lazyResult: LazyResult<T>,
  fn: (x: T) => Result<U>
): LazyResult<U> => 
  () => flatMap(lazyResult(), fn)

// Usage
const lazyComputation = lazy(() => {
  console.log('Computing...')
  return Ok(42)
})

const lazyResult = lazyMap(lazyComputation, x => x * 2)
// Nothing computed yet

const actualResult = lazyResult() // Now it computes
```

### Memoization

Memoize expensive Result computations:

```typescript
const memoize = <T extends any[], U>(
  fn: (...args: T) => Result<U>
): (...args: T) => Result<U> => {
  const cache = new Map<string, Result<U>>()
  
  return (...args: T): Result<U> => {
    const key = JSON.stringify(args)
    
    if (cache.has(key)) {
      return cache.get(key)!
    }
    
    const result = fn(...args)
    cache.set(key, result)
    return result
  }
}

// Usage
const expensiveComputation = memoize((x: number): Result<number> => {
  console.log(`Computing for ${x}...`)
  return Ok(x * x)
})

const result1 = expensiveComputation(5) // Computes
const result2 = expensiveComputation(5) // Uses cache
```

## Real-World Example: Data Processing Pipeline

Here's a comprehensive example showing advanced Result operations:

```typescript
import { 
  sequence, traverse, partition, asyncSequence, asyncMap,
  type Result, Ok, Err, flatMap, map
} from '@qi/qicore-foundation/base'

interface RawData {
  id: string
  data: any
}

interface ProcessedData {
  id: string
  processed: any
  metadata: any
}

interface ValidationResult {
  valid: ProcessedData[]
  invalid: { data: RawData; error: Error }[]
}

class DataProcessor {
  // Validate individual data items
  private validateData = (data: RawData): Result<RawData> => {
    if (!data.id || !data.data) {
      return Err(validationError('INVALID_DATA', 'Data missing required fields'))
    }
    return Ok(data)
  }

  // Enrich data with external information
  private enrichData = async (data: RawData): Promise<Result<ProcessedData>> => {
    try {
      const metadata = await this.fetchMetadata(data.id)
      return Ok({
        id: data.id,
        processed: this.processRawData(data.data),
        metadata
      })
    } catch (error) {
      return Err(networkError('ENRICHMENT_FAILED', 'Failed to enrich data'))
    }
  }

  // Process batch of data
  async processBatch(rawData: RawData[]): Promise<Result<ValidationResult>> {
    // Step 1: Validate all data items
    const validationResults = rawData.map(this.validateData)
    const { successes: validData, failures: validationErrors } = partition(validationResults)

    // Step 2: Enrich valid data in parallel
    const enrichmentPromises = validData.map(data => this.enrichData(data))
    const enrichmentResults = await asyncSequence(enrichmentPromises)

    return map(enrichmentResults, enrichedData => {
      const { successes: processed, failures: enrichmentErrors } = partition(enrichedData)
      
      return {
        valid: processed,
        invalid: [
          ...validationErrors.map((error, index) => ({
            data: rawData[index],
            error
          })),
          ...enrichmentErrors.map((error, index) => ({
            data: validData[index],
            error
          }))
        ]
      }
    })
  }

  // Process multiple batches
  async processMultipleBatches(batches: RawData[][]): Promise<Result<ValidationResult[]>> {
    const batchPromises = batches.map(batch => this.processBatch(batch))
    return await asyncSequence(batchPromises)
  }

  // Utility methods
  private async fetchMetadata(id: string): Promise<any> {
    // Simulate API call
    return { timestamp: Date.now(), source: 'external' }
  }

  private processRawData(data: any): any {
    // Simulate processing
    return { ...data, processed: true }
  }
}

// Usage
async function main() {
  const processor = new DataProcessor()
  
  const rawData = [
    { id: '1', data: { value: 'a' } },
    { id: '2', data: { value: 'b' } },
    { id: '', data: { value: 'c' } }, // Invalid
    { id: '4', data: { value: 'd' } }
  ]

  const result = await processor.processBatch(rawData)
  
  if (result.tag === 'success') {
    console.log('Valid items:', result.value.valid.length)
    console.log('Invalid items:', result.value.invalid.length)
  } else {
    console.error('Processing failed:', result.error)
  }
}
```

## Best Practices

1. **Use traverse for collections**: Prefer traverse over manual mapping + sequence
2. **Leverage applicative validation**: Use applicative operations for accumulating errors
3. **Implement retry patterns**: Build retry logic with Result-aware patterns
4. **Optimize with memoization**: Cache expensive computations
5. **Use lazy evaluation**: Delay computations until needed
6. **Compose operations**: Build complex pipelines from simple operations
7. **Handle parallelism**: Use async operations for concurrent processing

## Next Steps

Now that you understand advanced Result operations, learn about:
- [Configuration Management](./05-configuration.md)
- [Integration Patterns](./08-integration-patterns.md)

## Common Patterns Reference

```typescript
// Pattern 1: Process array of items
const results = await traverse(items, processItem)

// Pattern 2: Combine multiple Results
const combined = combine2(result1, result2)

// Pattern 3: Separate successes and failures
const { successes, failures } = partition(results)

// Pattern 4: Retry with backoff
const result = await withRetry(operation, 3, 1000)

// Pattern 5: Circuit breaker
const result = await circuitBreaker.call(operation)
```