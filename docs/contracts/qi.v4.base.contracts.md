# QiCore v4.0 Base Component Contracts

This document defines pure behavioral contracts for QiCore v4.0 **Base Component** types (Result<T> and QiError). These contracts are completely language-agnostic and specify only the mathematical and behavioral properties that any implementation must satisfy.

## 1. Result<T> Behavioral Contract

### Mathematical Foundation
- **Category Theory**: Result<T> forms a Monad, Functor, and Applicative
- **Type Definition**: `Result<T> = Success<T> | Failure<QiError>`
- **Variance**: Covariant in T
- **Laws**: Must satisfy Functor, Monad, and Applicative laws

### Factory Operations
```yaml
success:
  signature: "T → Result<T>"
  laws:
    - "success(x).isSuccess() == true"
    - "success(x).getData() == x"
    - "success(x).getError() == null"

failure:
  signature: "QiError → Result<T>"
  laws:
    - "failure(e).isFailure() == true"
    - "failure(e).getError() == e"
    - "failure(e).getData() == null"

fromTryCatch:
  signature: "(() → T) → Result<T>"
  laws:
    - "if operation succeeds: result.isSuccess() == true"
    - "if operation throws: result.isFailure() == true"
    - "exception safety: never propagates exceptions"

fromAsyncTryCatch:
  signature: "(() → Promise<T>) → Promise<Result<T>>"
  laws:
    - "async variant of fromTryCatch"
    - "promise rejection becomes failure state"

fromMaybe:
  signature: "(T?, QiError) → Result<T>"
  laws:
    - "if value != null: result.isSuccess() == true"
    - "if value == null: result.isFailure() == true"
    - "parameters: (maybeValue, errorIfNull) for consistency"

fromEither:
  signature: "Either<QiError, T> → Result<T>"
  laws:
    - "preserves left/right semantics"
    - "maintains functor/monad laws"
```

### Query Properties
```yaml
isSuccess:
  signature: "Result<T> → Boolean"
  laws:
    - "isSuccess(success(x)) == true"
    - "isSuccess(failure(e)) == false"
    - "type guard property"

isFailure:
  signature: "Result<T> → Boolean"
  laws:
    - "isFailure(failure(e)) == true"
    - "isFailure(success(x)) == false"
    - "isSuccess(r) XOR isFailure(r) == true"

getValue:
  signature: "Result<T> → T?"
  laws:
    - "getValue(success(x)) == x"
    - "getValue(failure(e)) == null"
    - "partial function: use with type checking"

getError:
  signature: "Result<T> → QiError?"
  laws:
    - "getError(failure(e)) == e"
    - "getError(success(x)) == null"
```

### Transformation Operations
```yaml
map:
  signature: "(T → U) → Result<T> → Result<U>"
  laws:
    # Functor Laws
    - "identity: map(id) == id"
    - "composition: map(f ∘ g) == map(f) ∘ map(g)"
    # Behavior Laws
    - "map(f)(success(x)) == success(f(x))"
    - "map(f)(failure(e)) == failure(e)"

mapError:
  signature: "(QiError → QiError) → Result<T> → Result<T>"
  laws:
    - "mapError(f)(success(x)) == success(x)"
    - "mapError(f)(failure(e)) == failure(f(e))"

flatMap:
  signature: "(T → Result<U>) → Result<T> → Result<U>"
  laws:
    # Monad Laws
    - "left identity: flatMap(f)(success(x)) == f(x)"
    - "right identity: result.flatMap(success) == result"
    - "associativity: result.flatMap(f).flatMap(g) == result.flatMap(x => f(x).flatMap(g))"
    # Behavior Laws
    - "flatMap(f)(failure(e)) == failure(e)"

andThen:
  signature: "(T → Result<U>) → Result<T> → Result<U>"
  laws:
    # Alias for flatMap with clearer semantics (Rust-style naming)
    - "andThen(f) == flatMap(f)"
    - "andThen(f)(success(x)) == f(x)"
    - "andThen(f)(failure(e)) == failure(e)"
    - "more intuitive than flatMap for sequential operations"

inspect:
  signature: "(T → void) → Result<T> → Result<T>"
  laws:
    - "inspect(f)(success(x)) == success(x) after calling f(x)"
    - "inspect(f)(failure(e)) == failure(e) without calling f"
    - "used for logging, debugging, side effects"
    - "does not change Result value or type"

inspectErr:
  signature: "(QiError → void) → Result<T> → Result<T>"
  laws:
    - "inspectErr(f)(failure(e)) == failure(e) after calling f(e)"
    - "inspectErr(f)(success(x)) == success(x) without calling f"
    - "used for error logging, debugging"
    - "does not change Result value or type"

collect:
  signature: "Result<Result<T>> → Result<T>"
  laws:
    # Flattens nested Results (join/flatten operation)
    - "collect(success(success(x))) == success(x)"
    - "collect(success(failure(e))) == failure(e)"
    - "collect(failure(e)) == failure(e)"
    - "equivalent to flatMap(identity)"

filter:
  signature: "(T → Boolean) → Result<T> → Result<T>"
  laws:
    - "filter(pred)(success(x)) == success(x) if pred(x) is true"
    - "filter(pred)(success(x)) == failure(FILTERED_ERROR) if pred(x) is false"
    - "filter(pred)(failure(e)) == failure(e)"
    - "essential for validation pipelines"

orElse:
  signature: "(QiError → Result<T>) → Result<T> → Result<T>"
  laws:
    - "orElse(alt)(success(x)) == success(x)"
    - "orElse(alt)(failure(e)) == alt(e)"
    - "provides error recovery mechanism"
    - "enables fallback strategies"
```

### Extraction Operations
```yaml
unwrap:
  signature: "Result<T> → T"
  laws:
    - "unwrap(success(x)) == x"
    - "unwrap(failure(e)) throws exception"
    - "partial function: only safe after isSuccess check"

unwrapOr:
  signature: "T → Result<T> → T"
  laws:
    - "unwrapOr(default)(success(x)) == x"
    - "unwrapOr(default)(failure(e)) == default"
    - "total function: always returns value"

match:
  signature: "(T → R) → (QiError → R) → Result<T> → R"
  laws:
    - "match(onSuccess, onError)(success(x)) == onSuccess(x)"
    - "match(onSuccess, onError)(failure(e)) == onError(e)"
    - "total function: always returns value of type R"

orElse:
  signature: "(QiError → Result<T>) → Result<T> → Result<T>"
  laws:
    - "orElse(alt)(success(x)) == success(x)"
    - "orElse(alt)(failure(e)) == alt(e)"
    - "error recovery: allows chaining fallback operations"
```

### Collection Operations
```yaml
sequence:
  signature: "List<Result<T>> → Result<List<T>>"
  laws:
    - "if all success: returns success with all values"
    - "if any failure: returns first failure (fail-fast)"
    - "preserves order: success values maintain input order"

traverse:
  signature: "(A → Result<B>) → List<A> → Result<List<B>>"
  laws:
    - "traverse(f) == sequence ∘ map(f)"
    - "maintains order: preserves input list order"

partition:
  signature: "List<Result<T>> → (List<T>, List<QiError>)"
  laws:
    - "splits Results into separate success and error lists"
    - "preserves order within each list"
    - "total function: handles all Results"
    - "partition(results) == (successes, failures)"

lefts:
  signature: "List<Result<T>> → List<QiError>"
  laws:
    - "extracts all errors from Result list"
    - "lefts(results) == errors where result.isFailure()"
    - "preserves order of errors"

rights:
  signature: "List<Result<T>> → List<T>"
  laws:
    - "extracts all successes from Result list"
    - "rights(results) == values where result.isSuccess()"
    - "preserves order of values"

combine2:
  signature: "Result<T> → Result<U> → ((T, U) → V) → Result<V>"
  laws:
    - "combine2(success(x), success(y), f) == success(f(x, y))"
    - "if either Result is failure, return first failure"
    - "enables applicative combination of two Results"
    - "fails fast on first error"
```

### Applicative Operations
```yaml
apply:
  signature: "Result<(T → U)> → Result<T> → Result<U>"
  laws:
    # Applicative Laws
    - "identity: apply(success(id))(result) == result"
    - "composition: apply(apply(apply(success(compose))(f))(g))(x) == apply(f)(apply(g)(x))"
    - "homomorphism: apply(success(f))(success(x)) == success(f(x))"
    - "interchange: apply(f)(success(x)) == apply(success(f => f(x)))(f)"
    # Behavior Laws
    - "apply(success(f))(success(x)) == success(f(x))"
    - "apply(failure(e))(result) == failure(e)"
    - "apply(success(f))(failure(e)) == failure(e)"

pure:
  signature: "T → Result<T>"
  laws:
    - "pure(x) == success(x)"
    - "applicative unit operation"
    - "alias for success in applicative context"
```

### Async Operations
```yaml
asyncMap:
  signature: "(T → Promise<U>) → Result<T> → Promise<Result<U>>"
  laws:
    - "asyncMap(f)(success(x)) == Promise.resolve(success(await f(x)))"
    - "asyncMap(f)(failure(e)) == Promise.resolve(failure(e))"
    - "preserves Result semantics in async context"
    - "handles async function exceptions as failures"

asyncAndThen:
  signature: "(T → Promise<Result<U>>) → Result<T> → Promise<Result<U>>"
  laws:
    - "async version of andThen/flatMap"
    - "asyncAndThen(f)(success(x)) == f(x)"
    - "asyncAndThen(f)(failure(e)) == Promise.resolve(failure(e))"
    - "handles Promise rejection as failure"

asyncSequence:
  signature: "List<Promise<Result<T>>> → Promise<Result<List<T>>>"
  laws:
    - "resolves all promises, then applies sequence logic"
    - "if all Results are success: returns success with all values"
    - "if any Result is failure: returns first failure"
    - "maintains async semantics with Result logic"

fromPromise:
  signature: "Promise<T> → Promise<Result<T>>"
  laws:
    - "converts Promise<T> to Promise<Result<T>>"
    - "Promise resolution becomes success"
    - "Promise rejection becomes failure with QiError"
    - "preserves async timing"

toPromise:
  signature: "Result<T> → Promise<T>"
  laws:
    - "success(x) becomes Promise.resolve(x)"
    - "failure(e) becomes Promise.reject(e)"
    - "converts Result back to Promise semantics"
```

## 2. QiError Behavioral Contract

### Mathematical Foundation
- **Type Definition**: Product type `Code × Message × Category × Context × Cause × Timestamp`
- **Immutability**: All operations return new instances
- **Composability**: Supports error chaining and context accumulation

### Core Structure
```yaml
required_fields:
  code:
    type: "String"
    laws:
      - "non-empty: code.length > 0"
      - "identifier format: suitable for programmatic handling"
  
  message:
    type: "String"  
    laws:
      - "non-empty: message.length > 0"
      - "human readable: provides actionable information"
  
  category:
    type: "ErrorCategory"
    laws:
      - "enum value: one of predefined categories"
      - "determines retry strategy"
  
  context:
    type: "Map<String, Any>"
    laws:
      - "immutable: operations return new instances"
      - "accumulative: withContext merges with existing"
  
  cause:
    type: "QiError?"
    laws:
      - "optional: can be null"
      - "supports chaining: forms linked list of errors"
  
  timestamp:
    type: "Number"
    laws:
      - "positive: timestamp > 0"
      - "milliseconds since epoch"
```

### Factory Operations
```yaml
create:
  signature: "(String, String, ErrorCategory, Context?, QiError?, Timestamp?) → QiError"
  laws:
    - "all required fields populated"
    - "timestamp defaults to current time"
    - "context defaults to empty map"

fromException:
  signature: "(Exception, ErrorCategory?) → QiError"
  laws:
    - "preserves exception message"
    - "includes stack trace in context"
    - "category defaults to UNKNOWN"

fromString:
  signature: "(String, ErrorCategory?) → QiError"
  laws:
    - "creates QiError from simple message"
    - "category defaults to UNKNOWN"
```

### Query Operations
```yaml
toString:
  signature: "QiError → String"
  laws:
    - "includes code and message"
    - "human readable format"
    - "consistent format across implementations"

toStructuredData:
  signature: "QiError → SerializableObject"
  laws:
    - "result is JSON serializable"
    - "round-trip preserves all information"
    - "nested causes properly serialized"

getCategory:
  signature: "QiError → ErrorCategory"
  laws:
    - "getCategory(error) == error.category"

getRootCause:
  signature: "QiError → QiError"
  laws:
    - "if cause == null: getRootCause(error) == error"
    - "if cause != null: getRootCause(error) == getRootCause(error.cause)"
    - "terminates: cause chains are finite"

getErrorChain:
  signature: "QiError → List<QiError>"
  laws:
    - "returns complete chain from root to current"
    - "preserves order: [root, ..., current]"
    - "non-empty: always contains at least current error"
```

### Transformation Operations
```yaml
withContext:
  signature: "Map<String, Any> → QiError → QiError"
  laws:
    - "preserves all fields except context"
    - "merges new context with existing"
    - "immutable: original error unchanged"
    - "associative: withContext(c1)(withContext(c2)(e)) == withContext(c1 ∪ c2)(e)"

withCause:
  signature: "QiError → QiError → QiError"
  laws:
    - "preserves all fields except cause"
    - "replaces existing cause"
    - "immutable: original error unchanged"

withSeverity:
  signature: "ErrorSeverity → QiError → QiError"
  laws:
    - "preserves all fields except severity"
    - "immutable: original error unchanged"

chain:
  signature: "QiError → QiError → QiError"
  laws:
    - "creates error chain preserving both errors"
    - "chain(cause, effect) links cause as root of effect"
    - "different from withCause: preserves both error contexts"
    - "maintains causal relationship"

getRootError:
  signature: "QiError → QiError"
  laws:
    - "follows cause chain to find original error"
    - "getRootError(error) == error if no cause"
    - "terminates: cause chains are finite"
    - "more explicit than getRootCause"

hasCategory:
  signature: "ErrorCategory → QiError → Boolean"
  laws:
    - "hasCategory(cat, error) == (error.category == cat)"
    - "O(1) operation for category checking"
    - "used for retry strategy decisions"

formatChain:
  signature: "QiError → String"
  laws:
    - "returns human-readable error chain"
    - "includes all errors in causal order"
    - "suitable for logging and debugging"
    - "consistent format across implementations"
```

## 3. ErrorCategory Enumeration Contract

```yaml
categories:
  VALIDATION:
    retry_strategy: "never"
    description: "Input constraint violations"
  
  NETWORK:
    retry_strategy: "exponential_backoff"
    description: "Communication failures"
  
  SYSTEM:
    retry_strategy: "linear_backoff"  
    description: "Resource/infrastructure problems"
  
  BUSINESS:
    retry_strategy: "never"
    description: "Domain rule violations"
  
  SECURITY:
    retry_strategy: "never"
    description: "Authorization/authentication failures"
  
  PARSING:
    retry_strategy: "never"
    description: "Data format/syntax errors"
  
  TIMEOUT:
    retry_strategy: "timeout_backoff"
    description: "Operation time limit exceeded"
  
  ASYNC:
    retry_strategy: "exponential_backoff"
    description: "Async operation failures (cancellation, await issues)"
  
  CONCURRENCY:
    retry_strategy: "linear_backoff"
    description: "Thread safety, locking, race conditions"
  
  RESOURCE:
    retry_strategy: "linear_backoff"
    description: "Memory, file handles, connection limits"
  
  CONFIGURATION:
    retry_strategy: "never"
    description: "Config loading, validation, missing keys"
  
  SERIALIZATION:
    retry_strategy: "never"
    description: "JSON/YAML parsing, data conversion"
  
  FILESYSTEM:
    retry_strategy: "linear_backoff"
    description: "File I/O operations, permissions"
  
  UNKNOWN:
    retry_strategy: "cautious"
    description: "Unclassified errors"

laws:
  - "complete enumeration: covers all expected error types"
  - "mutually exclusive: each error has exactly one category"
  - "retry strategy: category determines retry behavior"
  - "categories reflect modern async/concurrent programming needs"
```

## 4. Advanced Behavioral Contracts

### Error Accumulation
```yaml
parallel_validation:
  signature: "List<Validation<T>> → Result<List<T>>"
  laws:
    - "collects all errors instead of failing fast"
    - "if all valid: returns success with all values"
    - "if any invalid: returns failure with all errors"

applicative_sequence:
  signature: "List<Result<T>> → Result<List<T>>"
  laws:
    - "follows applicative functor laws"
    - "accumulates errors in applicative context"
```

### Resource Management
```yaml
bracket:
  signature: "Result<Resource> → (Resource → Result<A>) → (Resource → Result<()>) → Result<A>"
  laws:
    - "acquire-use-release pattern"
    - "cleanup guaranteed even on failure"
    - "exception safe: handles errors in all phases"

timeout:
  signature: "Duration → Result<T> → Result<T>"
  laws:
    - "operation completes within duration or fails"
    - "timeout error includes timing information"
    - "cancellation: stops operation on timeout"
```

### Async Operations
```yaml
parallel:
  signature: "List<AsyncResult<T>> → AsyncResult<List<T>>"
  laws:
    - "executes operations concurrently"
    - "fails fast on first error"
    - "preserves result order"

race:
  signature: "List<AsyncResult<T>> → AsyncResult<T>"
  laws:
    - "returns first successful completion"
    - "cancels remaining on success"
    - "aggregates errors if all fail"

retry:
  signature: "RetryConfig → AsyncResult<T> → AsyncResult<T>"
  laws:
    - "respects maximum retry limit"
    - "implements configured backoff strategy"
    - "eventually fails after max attempts"
```

## 5. Universal Laws and Properties

### Composition Laws
```yaml
composition:
  associativity:
    - "flatMap(f) ∘ flatMap(g) ∘ result == flatMap(x => flatMap(f)(g(x))) ∘ result"
  
  identity:
    - "flatMap(success) ∘ result == result"
    - "map(id) ∘ result == result"
  
  distributivity:
    - "map(f) ∘ map(g) == map(f ∘ g)"
```

### Error Propagation Laws
```yaml
error_propagation:
  preservation:
    - "operations on failure results preserve the error"
    - "error information never lost during transformations"
  
  accumulation:
    - "context accumulation is associative"
    - "error chaining preserves causal relationships"
```

### Performance Contracts
```yaml
performance:
  operation_complexity:
    - "factory operations: O(1)"
    - "query operations: O(1)"
    - "transformation operations: O(1) for structure, O(f) for function"
    - "error chain traversal: O(chain_length)"
  
  memory_usage:
    - "constant overhead per Result instance"
    - "linear memory usage for error chains"
    - "efficient context storage"
```

## 6. Contract Verification Requirements

### Property-Based Testing
- All mathematical laws must be verified with property-based tests
- Edge cases (null, empty, large data) must be covered
- Performance requirements must be benchmarked

### Type Safety Verification
- Impossible states must be prevented at compile time where possible
- Runtime type guards must be consistent with compile-time types
- No undefined behavior allowed

### Cross-Language Consistency
- Identical behavior across all language implementations
- Serialization format compatibility
- Performance characteristics within acceptable ranges

---

**Contract Compliance**: Any implementation claiming QiCore v4.0 compatibility must satisfy ALL laws and properties defined in this specification.