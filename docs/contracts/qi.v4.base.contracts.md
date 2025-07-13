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
  signature: "(QiError, T?) → Result<T>"
  laws:
    - "if value != null: result.isSuccess() == true"
    - "if value == null: result.isFailure() == true"

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

getData:
  signature: "Result<T> → T?"
  laws:
    - "getData(success(x)) == x"
    - "getData(failure(e)) == null"

getError:
  signature: "Result<T> → QiError?"
  laws:
    - "getError(failure(e)) == e"
    - "getError(success(x)) == null"
```

### Transformation Operations
```yaml
mapResult:
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
    - "right identity: flatMap(success) == id"
    - "associativity: flatMap(g) ∘ flatMap(f) == flatMap(x => flatMap(g)(f(x)))"
    # Behavior Laws
    - "flatMap(f)(failure(e)) == failure(e)"
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
  
  UNKNOWN:
    retry_strategy: "cautious"
    description: "Unclassified errors"

laws:
  - "complete enumeration: covers all expected error types"
  - "mutually exclusive: each error has exactly one category"
  - "retry strategy: category determines retry behavior"
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