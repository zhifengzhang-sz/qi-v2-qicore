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
  
laws:
  - "all configuration data is immutable"
  - "nested structures preserved during operations"
  - "type information maintained where possible"
```

### Factory Operations

```yaml
fromFile:
  signature: "FilePath → Result<ConfigData>"
  behavior: "Load configuration from file"
  laws:
    - "supports common configuration formats"
    - "returns PARSING error for invalid format"
    - "returns FILESYSTEM error for file access issues"

fromObject:
  signature: "Object → Result<ConfigData>"
  behavior: "Create configuration from object/map"
  laws:
    - "validates object structure"
    - "returns VALIDATION error for invalid structure"
    - "preserves nested structure"

fromEnvironment:
  signature: "Prefix? → Result<ConfigData>"
  behavior: "Load configuration from environment variables"
  laws:
    - "prefix filters environment variables"
    - "converts ENV_VAR_NAME to nested.var.name structure"
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
```

### Transformation Operations

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

empty:
  signature: "() → ConfigData"
  behavior: "Create empty configuration (monoid identity)"
  laws:
    - "merge([empty(), config]) == config"
    - "merge([config, empty()]) == config"
    - "has(anyKey, empty()) == false"

validate:
  signature: "Schema → ConfigData → Result<ConfigData>"
  behavior: "Validate configuration against schema"
  laws:
    - "returns original config if valid"
    - "returns VALIDATION error if invalid"
    - "validates nested structures"
    - "supports type checking and constraints"
```

## 2. Logger Behavioral Contract

### Mathematical Foundation
- **Effect System**: Logging is a side effect with configurable levels
- **Performance**: Level checking optimized for high-frequency operations
- **Structured Data**: Support for both simple messages and rich context

### Logger Configuration Contract

```yaml
LoggerConfig:
  level: LogLevel  # Minimum level to log
  
laws:
  - "level determines which messages are processed"
```

### Log Level Contract

```yaml
LogLevel:
  hierarchy: "DEBUG < INFO < WARN < ERROR < FATAL"
  
laws:
  - "levels form total ordering"
  - "higher severity has higher precedence"
  - "level comparison is constant time"
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
```

### Logging Operations

```yaml
debug:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log debug-level message"
  laws:
    - "only logs if logger level <= DEBUG"
    - "context added to output if provided"

info:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log info-level message"
  laws:
    - "only logs if logger level <= INFO"
    - "suitable for general application flow"

warn:
  signature: "Message → Context? → Logger → Effect<Void>"
  behavior: "Log warning-level message"
  laws:
    - "only logs if logger level <= WARN"
    - "indicates potential issues"

error:
  signature: "Message → Error? → Context? → Logger → Effect<Void>"
  behavior: "Log error-level message"
  laws:
    - "only logs if logger level <= ERROR"
    - "includes error details if provided"

fatal:
  signature: "Message → Error? → Context? → Logger → Effect<Void>"
  behavior: "Log fatal-level message"
  laws:
    - "only logs if logger level <= FATAL"
    - "indicates critical system failures"
```

### Utility Operations

```yaml
isLevelEnabled:
  signature: "LogLevel → Logger → Boolean"
  behavior: "Check if level would be logged"
  laws:
    - "returns true if level >= logger.level"
    - "constant time operation"
    - "use to avoid expensive message construction"

withContext:
  signature: "Context → Logger → Logger"
  behavior: "Create logger with additional context"
  laws:
    - "all subsequent log calls include context"
    - "context merged with per-call context"
    - "original logger unchanged (immutable)"
```

## 3. Cache Behavioral Contract

### Mathematical Foundation
- **Key-Value Mapping**: Cache implements Map semantics with expiration
- **LRU Semantics**: Least Recently Used eviction when size limits exceeded
- **TTL Semantics**: Time-To-Live expiration for automatic cleanup

### Cache Configuration Contract

```yaml
CacheConfig:
  maxSize: Integer?     # Maximum number of entries
  defaultTTL: Duration? # Default time-to-live
  
laws:
  - "maxSize controls memory usage"
  - "defaultTTL applied when no explicit TTL provided"
```

### Factory Operations

```yaml
createMemory:
  signature: "CacheConfig → Result<Cache>"
  behavior: "Create in-memory cache"
  laws:
    - "data lost on process restart"
    - "optimal for temporary caching"

createPersistent:
  signature: "FilePath → CacheConfig → Result<Cache>"
  behavior: "Create cache with persistence"
  laws:
    - "data survives process restarts"
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

set:
  signature: "Key → Value → TTL? → Cache → Result<Void>"
  behavior: "Store value in cache"
  laws:
    - "overwrites existing value for same key"
    - "uses default TTL if not specified"
    - "may trigger eviction if cache full"

has:
  signature: "Key → Cache → Boolean"
  behavior: "Check if key exists"
  laws:
    - "returns false for expired keys"
    - "does not update access time"

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

size:
  signature: "Cache → Integer"
  behavior: "Get number of entries in cache"
  laws:
    - "returns current count of non-expired entries"
    - "constant time operation"
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
```

### Cache Semantics

```yaml
eviction_laws:
  - "least recently used entries evicted first when maxSize exceeded"
  - "expired entries not returned by get operations"
  - "TTL checked on access"
  
persistence_laws:
  - "persistent caches survive process restarts"
  - "memory caches provide fastest access"
```

## Universal Core Component Laws

### Dependency Laws

```yaml
component_dependencies:
  - "All components use Result<T> for fallible operations"
  - "All components use QiError for error information"
  - "Components are independent (no inter-dependencies)"
```

### Performance Guarantees

```yaml
performance_guarantees:
  configuration:
    - "get operations: constant time for direct keys"
    - "merge operations: linear in total size"
  
  logger:
    - "level checking: constant time"
    - "logging operations: effect only, no return value"
  
  cache:
    - "get/set operations: constant average time"
    - "memory usage: bounded by maxSize"
```

---

**Contract Compliance**: Any implementation claiming QiCore Core Component compatibility must satisfy ALL contracts defined in this specification for Configuration, Logger, and Cache components.