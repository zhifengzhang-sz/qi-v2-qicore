# QiCore Core Component Contracts

This document defines pure behavioral contracts for QiCore **Core Component** types (Configuration, Logger, Cache). These contracts are completely language-agnostic and specify only the mathematical and behavioral properties that any implementation must satisfy.

**Dependencies**: This specification depends on [Base Component Contracts](qi.base.contracts.md) for Result<T> and QiError types.

## 1. Configuration Behavioral Contract

### Mathematical Foundation
- **Monoid Structure**: Configuration forms a monoid with associative merge operation and empty identity
- **Immutability**: All configuration objects are immutable after creation
- **Type Safety**: Strong typing for configuration values with validation

### Configuration Data Contract

```yaml
ConfigData:
  structure: "Nested key-value mapping"
  immutability: "Read-only after creation"
  serialization: "JSON/YAML/TOML compatible"
  
laws:
  - "all configuration data is immutable"
  - "nested structures preserved during operations"
  - "type information maintained where possible"
```

### Factory Operations

```yaml
fromFile:
  signature: "FilePath → Result<ConfigData>"
  behavior: "Load configuration from file (async)"
  laws:
    - "supports JSON, YAML, TOML formats"
    - "auto-detects format from file extension"
    - "returns PARSING error for invalid format"
    - "returns FILESYSTEM error for file access issues"

fromObject:
  signature: "Object → Result<ConfigData>"
  behavior: "Create configuration from object/map"
  laws:
    - "preserves nested structure"
    - "validates object structure"
    - "returns VALIDATION error for invalid structure"

fromString:
  signature: "String → Format → Result<ConfigData>"
  behavior: "Parse configuration from string content"
  laws:
    - "format parameter specifies JSON/YAML/TOML"
    - "returns PARSING error for syntax errors"
    - "preserves type information where possible"

fromEnvironment:
  signature: "Prefix? → Result<ConfigData>"
  behavior: "Load configuration from environment variables"
  laws:
    - "prefix filters environment variables"
    - "converts ENV_VAR_NAME to nested.var.name structure"
    - "attempts type coercion for common types"
    - "missing prefix loads all environment variables"
```

### Query Operations

```yaml
get:
  signature: "Key → ConfigData → Result<Value>"
  behavior: "Retrieve value by key path"
  laws:
    - "supports nested key paths (e.g., 'database.host')"
    - "returns NOT_FOUND error for missing keys"
    - "preserves original value type"

getWithDefault:
  signature: "Key → DefaultValue → ConfigData → Value"
  behavior: "Retrieve value with fallback default"
  laws:
    - "returns default if key not found"
    - "returns actual value if key exists"
    - "never fails (total function)"

has:
  signature: "Key → ConfigData → Boolean"
  behavior: "Check if key exists in configuration"
  laws:
    - "returns true if key path exists"
    - "supports nested key paths"
    - "never fails (total function)"

keys:
  signature: "ConfigData → List<Key>"
  behavior: "Get all available keys"
  laws:
    - "returns all key paths as flattened list"
    - "nested keys represented as dot-separated paths"
    - "keys sorted lexicographically"
```

### Monoid Operations

```yaml
merge:
  signature: "List<ConfigData> → Result<ConfigData>"
  behavior: "Merge multiple configurations (monoid operation)"
  laws:
    # Monoid Laws
    - "associativity: merge([a, merge([b, c])]) == merge([merge([a, b]), c])"
    - "left identity: merge([empty, config]) == config"
    - "right identity: merge([config, empty]) == config"
    # Merge Behavior
    - "right-biased: later configs override earlier ones"
    - "deep merge: nested objects merged recursively"
    - "array concatenation: arrays from all configs combined"

empty:
  signature: "() → ConfigData"
  behavior: "Create empty configuration (monoid identity)"
  laws:
    - "merge([empty(), config]) == config"
    - "merge([config, empty()]) == config"
    - "has(anyKey, empty()) == false"
```

### Schema Validation Operations (MANDATORY - 2025 Requirement)

```yaml
validate:
  signature: "ZodSchema → ConfigData → Result<ConfigData>"
  behavior: "Validate configuration against Zod schema with runtime type safety"
  laws:
    - "returns original config if valid"
    - "returns VALIDATION error with detailed Zod error information"
    - "schema provides TypeScript type inference"
    - "supports type coercion (string to number/boolean)"
    - "enables custom refinements with .refine() method"

validateWithTransform:
  signature: "ZodSchema → ConfigData → Result<TransformedConfigData>"
  behavior: "Validate and transform configuration data"
  laws:
    - "applies type transformations (string to number, etc.)"
    - "returns transformed config if validation passes"
    - "preserves all transformation errors in Result"

safeParse:
  signature: "ZodSchema → ConfigData → SafeParseResult<ConfigData>"
  behavior: "Safe validation without throwing exceptions"
  laws:
    - "returns success object with data if valid"
    - "returns error object with detailed issues if invalid"
    - "never throws exceptions (total function)"
    - "provides discriminated union result type"

validateRequired:
  signature: "List<Key> → ConfigData → Result<ConfigData>"
  behavior: "Validate that required keys are present"
  laws:
    - "returns original config if all keys present"
    - "returns VALIDATION error listing missing keys"
    - "checks nested key paths correctly"

validateTypes:
  signature: "TypeSchema → ConfigData → Result<ConfigData>"
  behavior: "Validate value types against schema"
  laws:
    - "returns original config if types match"
    - "returns VALIDATION error with type mismatches"
    - "supports basic types: String, Number, Boolean, Array, Object"

# 2025 Pattern: Injected Configuration with Dependency Injection
validateDependencies:
  signature: "DependencySchema → ConfigData → Result<ConfigData>"
  behavior: "Validate configuration dependencies are satisfied"
  laws:
    - "checks that all service dependencies have valid configurations"
    - "validates circular dependency detection"
    - "returns CONFIGURATION error for missing dependencies"

# 2025 Pattern: Strategy Pattern for Configuration Loading
loadStrategy:
  signature: "LoadStrategy → ConfigData → Result<ConfigData>"
  behavior: "Apply strategy-based configuration loading"
  laws:
    - "supports multiple loading strategies (file, env, remote, etc.)"
    - "strategies are composable and cacheable"
    - "enables hot-reloading with change detection"
```

## 2. Logger Behavioral Contract

### Mathematical Foundation
- **Effect System**: Logging is a side effect with configurable levels
- **Performance**: Level checking optimized for high-frequency operations
- **Structured Data**: Support for both simple messages and rich context

### Logger Configuration Contract

```yaml
LoggerConfig:
  level: LogLevel          # Minimum level to log
  format: LogFormat        # Output format (JSON, TEXT, CUSTOM)
  destination: Destination # Where to output logs
  
laws:
  - "level determines which messages are processed"
  - "format affects output structure only"
  - "destination determines I/O behavior"
```

### Log Level Contract

```yaml
LogLevel:
  hierarchy: "DEBUG < INFO < WARN < ERROR < FATAL"
  numeric_values: 
    DEBUG: 10
    INFO: 20
    WARN: 30
    ERROR: 40
    FATAL: 50
  
laws:
  - "levels form total ordering"
  - "higher numeric value = higher severity"
  - "level comparison is O(1) operation"
```

### Factory Operations

```yaml
create:
  signature: "LoggerConfig → Result<Logger>"
  behavior: "Create logger instance with configuration"
  laws:
    - "validates configuration before creation"
    - "returns VALIDATION error for invalid config"
    - "logger operates according to config"

createDefault:
  signature: "() → Result<Logger>"
  behavior: "Create logger with sensible defaults"
  laws:
    - "uses INFO level by default"
    - "outputs to console by default"
    - "uses TEXT format by default"
```

### Logging Operations

```yaml
debug:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log debug-level message"
  laws:
    - "only logs if logger level <= DEBUG"
    - "includes timestamp and level in output"
    - "context added to structured output if provided"

info:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log info-level message"
  laws:
    - "only logs if logger level <= INFO"
    - "suitable for general application flow"
    - "context enhances message with additional data"

warn:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log warning-level message"
  laws:
    - "only logs if logger level <= WARN"
    - "indicates potential issues"
    - "does not affect program execution"

error:
  signature: "Message → Error? → Context? → Logger → Effect<Void>"
  behavior: "Log error-level message"
  laws:
    - "only logs if logger level <= ERROR"
    - "includes error details if provided"
    - "suitable for recoverable errors"

fatal:
  signature: "Message → Error? → Context? → Logger → Effect<Void>"
  behavior: "Log fatal-level message"
  laws:
    - "only logs if logger level <= FATAL"
    - "indicates critical system failures"
    - "suitable for non-recoverable errors"
```

### Performance Operations

```yaml
isLevelEnabled:
  signature: "LogLevel → Logger → Boolean"
  behavior: "Check if level would be logged (performance optimization)"
  laws:
    - "returns true if level >= logger.level"
    - "O(1) operation for performance"
    - "use to avoid expensive message construction"

withContext:
  signature: "Context → Logger → Logger"
  behavior: "Create logger with additional context"
  laws:
    - "all subsequent log calls include context"
    - "context merged with per-call context"
    - "original logger unchanged (immutable)"

# 2025 Pattern: OpenTelemetry Integration
withTraceContext:
  signature: "TraceId → SpanId → Logger → Logger"
  behavior: "Create logger with OpenTelemetry trace correlation"
  laws:
    - "all logs include trace and span IDs for correlation"
    - "enables distributed tracing across services"
    - "compatible with OpenTelemetry semantic conventions"

# 2025 Pattern: Structured Logging with Correlation
withCorrelationId:
  signature: "CorrelationId → Logger → Logger"
  behavior: "Create logger with correlation ID for request tracking"
  laws:
    - "all logs include correlation ID"
    - "enables request flow tracking across components"
    - "supports nested correlation contexts"

# 2025 Pattern: AI-Enhanced Observability
logWithMetrics:
  signature: "Message → Metrics → Context? → Logger → Effect<Void>"
  behavior: "Log with associated performance metrics"
  laws:
    - "includes execution time, memory usage, etc."
    - "enables AI-driven anomaly detection"
    - "supports automated performance analysis"
```

## 3. Cache Behavioral Contract

### Mathematical Foundation
- **Key-Value Mapping**: Cache implements Map semantics with expiration
- **LRU Semantics**: Least Recently Used eviction when size limits exceeded
- **TTL Semantics**: Time-To-Live expiration for automatic cleanup

### Cache Configuration Contract

```yaml
CacheConfig:
  maxSize: Integer?        # Maximum number of entries (optional)
  defaultTTL: Duration?    # Default time-to-live (optional)
  evictionPolicy: Policy   # LRU, FIFO, or RANDOM
  persistent: Boolean      # Whether to persist to disk
  
laws:
  - "maxSize controls memory usage"
  - "defaultTTL applied when no explicit TTL provided"
  - "evictionPolicy determines removal strategy"
  - "persistent enables durability across restarts"
```

### Factory Operations

```yaml
createMemory:
  signature: "CacheConfig → Result<Cache>"
  behavior: "Create in-memory cache"
  laws:
    - "fast access with memory storage only"
    - "data lost on process restart"
    - "optimal for temporary caching"

createPersistent:
  signature: "FilePath → CacheConfig → Result<Cache>"
  behavior: "Create cache with disk persistence"
  laws:
    - "data survives process restarts"
    - "slower access due to disk I/O"
    - "suitable for durable caching"
    - "returns FILESYSTEM error for access issues"
```

### Core Operations

```yaml
get:
  signature: "Key → Cache → Result<Value>"
  behavior: "Retrieve value from cache"
  laws:
    - "returns NOT_FOUND if key doesn't exist"
    - "returns NOT_FOUND if key expired"
    - "updates access time for LRU tracking"
    - "O(1) average case performance"

set:
  signature: "Key → Value → TTL? → Cache → Result<Void>"
  behavior: "Store value in cache"
  laws:
    - "overwrites existing value for same key"
    - "uses default TTL if not specified"
    - "may trigger eviction if cache full"
    - "updates access time for LRU tracking"

has:
  signature: "Key → Cache → Boolean"
  behavior: "Check if key exists (without retrieving value)"
  laws:
    - "returns false for expired keys"
    - "does not update access time"
    - "O(1) average case performance"

remove:
  signature: "Key → Cache → Boolean"
  behavior: "Remove key from cache"
  laws:
    - "returns true if key existed"
    - "returns false if key didn't exist"
    - "idempotent: safe to call multiple times"

clear:
  signature: "Cache → Void"
  behavior: "Remove all entries from cache"
  laws:
    - "cache becomes empty after operation"
    - "resets all internal counters"
    - "O(n) operation where n is cache size"

size:
  signature: "Cache → Integer"
  behavior: "Get number of entries in cache"
  laws:
    - "returns current count of non-expired entries"
    - "O(1) operation"
    - "size <= maxSize when maxSize specified"
```

### Advanced Operations

```yaml
getOrSet:
  signature: "Key → (() → Result<Value>) → TTL? → Cache → Result<Value>"
  behavior: "Get value or compute and cache if missing"
  laws:
    - "returns existing value if key present and not expired"
    - "calls factory function if key missing or expired"
    - "caches factory result before returning"
    - "atomic: prevents duplicate computation"

setMany:
  signature: "Map<Key, Value> → TTL? → Cache → Result<Void>"
  behavior: "Set multiple key-value pairs"
  laws:
    - "applies same TTL to all entries"
    - "atomic: either all succeed or all fail"
    - "may trigger multiple evictions"

getMany:
  signature: "List<Key> → Cache → Result<Map<Key, Value>>"
  behavior: "Get multiple values by keys"
  laws:
    - "returns only keys that exist and haven't expired"
    - "missing keys not included in result"
    - "maintains key order where possible"

expire:
  signature: "Key → Cache → Boolean"
  behavior: "Manually expire a key"
  laws:
    - "removes key from cache immediately"
    - "returns true if key was present"
    - "equivalent to TTL reaching zero"

# 2025 Pattern: Distributed Cache Operations
createDistributed:
  signature: "ClusterConfig → CacheConfig → Result<Cache>"
  behavior: "Create distributed cache (Redis/Valkey cluster)"
  laws:
    - "supports up to 1000 nodes (Valkey improvement)"
    - "automatic failover and scaling"
    - "consistent hashing for key distribution"
    - "20% memory optimization over Redis (Valkey)"

# 2025 Pattern: Multi-threaded Cache Operations
createMultiThreaded:
  signature: "ThreadConfig → CacheConfig → Result<Cache>"
  behavior: "Create cache with enhanced multithreading (Valkey)"
  laws:
    - "improved I/O multithreading for modern CPUs"
    - "parallel operations while maintaining data safety"
    - "significant performance improvements on multi-core"

# 2025 Pattern: Cache-aside with Write-behind
writeBehind:
  signature: "Key → Value → BackingStore → Cache → Result<Void>"
  behavior: "Asynchronous write to backing store"
  laws:
    - "cache updated immediately"
    - "backing store updated asynchronously"
    - "reduces write latency for applications"

# 2025 Pattern: Cache Warming and Preloading
warmCache:
  signature: "List<Key> → DataSource → Cache → Result<Integer>"
  behavior: "Preload cache with frequently accessed data"
  laws:
    - "returns count of successfully loaded entries"
    - "non-blocking operation with progress tracking"
    - "respects TTL and eviction policies"
```

### Cache Semantics

```yaml
eviction_laws:
  lru_policy:
    - "least recently used entries evicted first"
    - "access updates recency (get, set operations)"
    - "eviction triggered when size > maxSize"
  
  ttl_laws:
    - "entries automatically removed when TTL expires"
    - "expired entries not returned by get operations"
    - "TTL checked on every access"
  
  persistence_laws:
    - "persistent caches survive process restarts"
    - "memory caches provide fastest access"
    - "persistence adds durability overhead"
```

## Universal Core Component Laws

### Dependency Laws

```yaml
component_dependencies:
  configuration_base:
    - "Configuration uses Result<T> for all fallible operations"
    - "Configuration uses QiError for detailed error information"
  
  logger_base:
    - "Logger uses Result<T> for creation and setup operations"
    - "Logger uses QiError for configuration validation"
  
  cache_base:
    - "Cache uses Result<T> for all operations that can fail"
    - "Cache uses QiError for detailed error reporting"
```

### Performance Contracts

```yaml
performance_requirements:
  configuration:
    - "get operations: O(1) for direct keys, O(depth) for nested"
    - "merge operations: O(n) where n is total config size"
    - "file loading: I/O bound, async operations preferred"
  
  logger:
    - "level checking: O(1) with early exit for disabled levels"
    - "message formatting: lazy evaluation when possible"
    - "I/O operations: non-blocking when feasible"
  
  cache:
    - "get/set operations: O(1) average case"
    - "eviction: O(1) for LRU, O(log n) for other policies"
    - "memory usage: bounded by maxSize when specified"
```

### Error Handling Laws

```yaml
error_consistency:
  validation_errors:
    - "VALIDATION category for invalid inputs/configuration"
    - "detailed context about what validation failed"
  
  filesystem_errors:
    - "FILESYSTEM category for file I/O operations"
    - "include file path and operation in context"
  
  performance_errors:
    - "operations maintain performance guarantees even under error conditions"
    - "error paths do not significantly impact normal operation performance"
```

## Contract Verification

### Core Component Tests

```yaml
configuration_tests:
  - "monoid laws: associativity, identity"
  - "merge behavior: right-bias, deep merge"
  - "validation: schema compliance, required keys"
  - "file formats: JSON, YAML, TOML parsing"

logger_tests:
  - "level hierarchy: ordering and filtering"
  - "performance: level checking overhead"
  - "context: structured data inclusion"
  - "output formats: JSON, text formatting"

cache_tests:
  - "TTL expiration: automatic cleanup"
  - "eviction policies: LRU, FIFO behavior"
  - "persistence: durability across restarts"
  - "performance: operation complexity guarantees"
```

---

**Contract Compliance**: Any implementation claiming QiCore Core Component compatibility must satisfy ALL contracts defined in this specification for Configuration, Logger, and Cache components.