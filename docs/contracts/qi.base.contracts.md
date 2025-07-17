# QiCore Base Component Contracts

This document defines pure behavioral contracts for QiCore **Base Component** types (Result<T> and QiError). These contracts are completely language-agnostic and specify only the mathematical and behavioral properties that any implementation must satisfy.

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

#### Core Transformation Contract
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
```


## 2. QiError Behavioral Contract

### Mathematical Foundation
- **Type Definition**: Product type `Code × Message × Category × Context?`
- **Immutability**: All operations return new instances
- **Simplicity**: Essential error information only

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

optional_fields:
  context:
    type: "Map<String, Any>"
    laws:
      - "optional: can be empty"
      - "immutable: operations return new instances"
```

### Factory Operations
```yaml
create:
  signature: "(String, String, ErrorCategory, Context?) → QiError"
  laws:
    - "all required fields populated"
    - "context defaults to empty map"

createError:
  signature: "ErrorOptions → QiError"
  behavior: "Create QiError from options object"
  laws:
    - "ErrorOptions: {code: String, message: String, category: ErrorCategory, context?: Context}"
    - "all required fields populated from options"
    - "context defaults to empty map if not provided"

fromException:
  signature: "(Exception, ErrorCategory?) → QiError"
  laws:
    - "preserves exception message"
    - "category defaults to UNKNOWN"

fromString:
  signature: "(String, ErrorCategory?) → QiError"
  laws:
    - "creates QiError from simple message"
    - "category defaults to UNKNOWN"

loggerError:
  signature: "(String, Context?) → LoggerError"
  behavior: "Create logger-specific error"
  laws:
    - "category automatically set to LOGGER"
    - "code automatically set to LOGGER_ERROR"
    - "context defaults to empty map"
```

### Query Operations
```yaml
toString:
  signature: "QiError → String"
  laws:
    - "includes code and message"
    - "human readable format"
    - "consistent format across implementations"

getCategory:
  signature: "QiError → ErrorCategory"
  laws:
    - "getCategory(error) == error.category"

toStructuredData:
  signature: "QiError → SerializableObject"
  laws:
    - "result is JSON serializable"
    - "preserves all error information"
```

### Transformation Operations
```yaml
withContext:
  signature: "Map<String, Any> → QiError → QiError"
  laws:
    - "preserves all fields except context"
    - "merges new context with existing"
    - "immutable: original error unchanged"
```

## 3. ErrorCategory Enumeration Contract

```yaml
categories:
  VALIDATION:
    retry_strategy: "never"
    description: "Input validation and constraint violations"
  
  NETWORK:
    retry_strategy: "exponential_backoff"
    description: "Network communication failures"
  
  SYSTEM:
    retry_strategy: "linear_backoff"  
    description: "System resource and infrastructure problems"
  
  BUSINESS:
    retry_strategy: "never"
    description: "Business logic and domain rule violations"
  
  AUTHENTICATION:
    retry_strategy: "never"
    description: "Authentication failures"
  
  AUTHORIZATION:
    retry_strategy: "never"
    description: "Authorization/permission failures"
  
  CONFIGURATION:
    retry_strategy: "never"
    description: "Configuration/setup errors"
  
  TIMEOUT:
    retry_strategy: "exponential_backoff"
    description: "Timeout errors"
  
  RESOURCE:
    retry_strategy: "linear_backoff"
    description: "Resource exhaustion/unavailable"
  
  CONCURRENCY:
    retry_strategy: "linear_backoff"
    description: "Concurrency conflicts"
  
  LOGGER:
    retry_strategy: "never"
    description: "Logger-related errors"

laws:
  - "complete enumeration: covers all expected error types"
  - "mutually exclusive: each error has exactly one category"  
  - "retry strategy: category determines retry behavior"
  - "cross-language consistency: same categories across all implementations"
```

---

**Contract Compliance**: Any implementation claiming QiCore Base Component compatibility must satisfy ALL laws and properties defined in this specification for Result<T> and QiError types.