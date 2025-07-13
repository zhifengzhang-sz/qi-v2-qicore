# QiCore v4.0 Component Architecture Specification

This document defines the component-level architecture for QiCore v4.0. Components are organizational units that group related behavioral contracts with clear dependency relationships and interface boundaries.

## Component Architecture Overview

QiCore v4.0 is organized into five distinct components that form a dependency hierarchy:

```mermaid
graph TD
    subgraph "QiCore v4.0 Component Architecture"
        subgraph "Application Components"
            HTTP[HTTP Component]
            Document[Document Component]
            CLP[CLP Component]
        end
        
        subgraph "Core Component"
            CoreComp[Core Component<br/>Config, Logger, Cache]
        end
        
        subgraph "Base Component"
            BaseComp[Base Component<br/>Result, QiError]
        end
        
        HTTP --> CoreComp
        Document --> CoreComp
        CLP --> CoreComp
        
        CoreComp --> BaseComp
        
        classDef component fill:#e8f5e9,stroke:#1b5e20,stroke-width:3px
        classDef core fill:#f3e5f5,stroke:#4a148c,stroke-width:3px
        classDef base fill:#e1f5fe,stroke:#01579b,stroke-width:3px
        
        class HTTP,Document,CLP component
        class CoreComp core
        class BaseComp base
    end
```

---

## Base Component

**Purpose**: Foundational error handling and result types used by all other components

### Component Interface

```
BaseComponent provides:
  Result operations:
    - success(data) → Result containing data
    - failure(error) → Result containing error
    - fromTryCatch(operation) → Result from fallible operation
  
  Error operations:
    - create(code, message, category) → QiError
    - withContext(error, contextData) → QiError with context
    - withCause(error, causeError) → QiError with cause chain
```

### Included Contracts
- **Result<T>**: Type-safe error handling with functional composition
- **QiError**: Structured error representation with context and chaining

**📋 Complete Base Contracts**: See [qi.v4.base.contracts.md](qi.v4.base.contracts.md) for detailed behavioral specifications.

### Dependencies
- **None**: Base component has no dependencies

### Exported Types
- `Result<T>`: Success/failure container type
- `QiError`: Structured error type
- `ErrorCategory`: Error classification enum

### Component Guarantees
- **Zero Dependencies**: Can be used standalone
- **Immutable**: All types are immutable after creation
- **Thread-Safe**: Safe for concurrent use
- **Minimal Overhead**: < 1KB runtime overhead

### Usage Pattern
```
// Language-agnostic usage pattern
result = success(42)
doubled = map(multiply_by_2, result)  // Result containing 84

error = create("NETWORK_ERROR", "Connection failed", NETWORK)
failure = failure(error)
```

---

## Core Component

**Purpose**: Essential infrastructure services for configuration, logging, and caching

### Component Interface

```
CoreComponent provides:
  Configuration operations:
    - fromFile(path) → async Result<ConfigData>
    - fromObject(data) → Result<ConfigData>
    - fromEnvironment(prefix) → Result<ConfigData>
    - get(config, key) → Result<value>
    - merge(configs) → Result<ConfigData> // Monoid operation
  
  Logging operations:
    - create(config) → Result<Logger>
    - log(level, message, context?) → void
    - isLevelEnabled(level) → boolean
  
  Cache operations:
    - createMemory(config) → Result<Cache>
    - createPersistent(path, config) → async Result<Cache>
    - get(cache, key) → Result<value>
    - set(cache, key, value, ttl) → Result<void>
```

### Included Contracts
- **Configuration**: Multi-source config loading with monoid merge semantics
- **Logging**: Simple effect interface with level-based filtering
- **Cache**: High-performance caching with eviction policies

**📋 Complete Core Contracts**: See [qi.v4.core.contracts.md](qi.v4.core.contracts.md) for detailed behavioral specifications.

### Dependencies
- **Base Component**: Uses Result<T> and QiError throughout

### Exported Types
- `ConfigData`: Immutable configuration container
- `Logger`: Logger instance type
- `Cache`: Cache instance type
- `LogLevel`: Logging level enum
- `CacheConfig`: Cache configuration options

### Component Guarantees
- **Independent Services**: Config, Logger, and Cache can be used independently
- **Consistent Error Handling**: All operations return Result<T>
- **Async-Aware**: File I/O operations are properly async
- **Performance**: Operations meet language-tier performance targets
- **Resource Management**: Proper cleanup for persistent resources

### Internal Dependencies
- Configuration has no dependencies on Logger or Cache
- Logger may use Configuration for initialization
- Cache may use Configuration for settings and Logger for operations

### Usage Pattern
```
// Language-agnostic usage pattern
configResult = fromFile("app.json")  // async operation
config = unwrapOr(defaultConfig, configResult)

// Monoid merge operation
merged = merge([defaultConfig, fileConfig, envConfig])

// Simple logging interface
logger = create(LogConfig{level: INFO})
log(INFO, "Application started", {version: "1.0.0"}, logger)
```

---

## HTTP Component

**Purpose**: Asynchronous HTTP client functionality with robust error handling and resilience patterns

### Component Interface

```
HttpComponent provides:
  HTTP operations (all async):
    - get(url, options) → async Result<HttpResponse>
    - post(url, body, options) → async Result<HttpResponse>
    - put(url, body, options) → async Result<HttpResponse>
    - delete(url, options) → async Result<HttpResponse>
    - request(config) → async Result<HttpResponse>
    - stream(url, options) → async Result<Stream<Chunk>>
  
  Circuit Breaker operations:
    - withCircuitBreaker(config) → HttpClient
    - getCircuitState() → CircuitState
```

### Included Contracts
- **HTTP Client**: Full-featured HTTP client with retries and timeouts
- **Circuit Breaker**: Failure detection and recovery patterns
- **Streaming**: Support for chunked responses and large payloads

### Dependencies
- **Base Component**: Uses Result<T> and QiError
- **Core Component**: 
  - Uses Configuration for default settings (timeouts, retries)
  - Uses Logger for request/response logging
  - May use Cache for response caching

### Exported Types
- `HttpResponse`: Response data structure
- `HttpConfig`: Request configuration
- `HttpOptions`: Simplified request options
- `CircuitBreakerConfig`: Circuit breaker settings
- `CircuitState`: Current circuit state (CLOSED, OPEN, HALF_OPEN)
- `Stream<T>`: Async stream for chunked data

### Component Guarantees
- **Async Operations**: All operations are asynchronous
- **Automatic Retries**: Configurable retry policies with exponential backoff
- **Timeout Management**: Request-level timeout control
- **Connection Pooling**: Efficient connection reuse
- **Circuit Breaking**: Automatic failure detection and recovery
- **Streaming Support**: Memory-efficient large response handling

### Circuit Breaker Configuration
```yaml
CircuitBreakerConfig:
  failureThreshold: Number     # Failures before opening (default: 5)
  resetTimeout: Number         # Ms before attempting reset (default: 60000)
  monitoringPeriod: Number     # Ms window for failure counting (default: 10000)
  halfOpenRequests: Number     # Requests to test in half-open (default: 3)
```

### Usage Pattern
```
// Language-agnostic pattern
httpClient = withCircuitBreaker(
  CircuitBreakerConfig{
    failureThreshold: 5,
    resetTimeout: 60000
  }
)

// Streaming large response
streamResult = stream("https://api.example.com/large-file", httpClient)
if isSuccess(streamResult) then
  stream = unwrap(streamResult)
  forEach chunk in stream do
    processChunk(chunk)  // Process without loading entire response
  end
end
```

---

## Document Component

**Purpose**: Template-based document generation with multiple format support and streaming capabilities

### Component Interface

```
DocumentComponent provides:
  Document operations:
    - generate(template, data) → async Result<Document>
    - generateFromFile(path, data) → async Result<Document>
    - generateStream(template, data) → async Result<Stream<DocumentChunk>>
    - generateBatch(templates, data) → async Result<Array<Document>>
    - validateTemplate(template, schema) → Result<ValidationResult>
```

### Included Contracts
- **Document Generation**: Multi-engine template processing
- **Streaming Generation**: Memory-efficient large document handling

### Dependencies
- **Base Component**: Uses Result<T> and QiError
- **Core Component**:
  - Uses Configuration for template paths and settings
  - Uses Logger for generation logging
  - May use Cache for compiled templates

### Exported Types
- `Document`: Generated document structure
- `Template`: Template definition
- `ValidationResult`: Template validation outcome
- `DocumentChunk`: Streamable document fragment
- `Stream<DocumentChunk>`: Async stream for large documents

### Component Guarantees
- **Format Agnostic**: Supports multiple output formats
- **Engine Agnostic**: Pluggable template engines
- **Batch Processing**: Efficient multi-document generation
- **Validation**: Pre-generation template validation
- **Streaming**: Memory-efficient large document generation

### Usage Pattern
```
// Language-agnostic pattern
streamResult = generateStream(template, largeDataset)  // async
if isSuccess(streamResult) then
  stream = unwrap(streamResult)
  writer = createFileWriter("output.pdf")
  
  forEach chunk in stream do
    write(chunk, writer)  // async
  end
  close(writer)  // async
end
```

---

## CLP Component

**Purpose**: Command-line argument parsing with validation and help generation

### Component Interface

```
CLPComponent provides:
  CLI operations:
    - parse(args, config) → Result<ParsedArguments>
    - parseString(input, config) → Result<ParsedArguments>
    - validate(args, config) → Result<ValidationResult>
    - generateHelp(config) → string
    - generateUsage(config) → string
```

### Included Contracts
- **Command-Line Processing**: Hierarchical command parsing

### Dependencies
- **Base Component**: Uses Result<T> and QiError
- **Core Component**:
  - Uses Configuration for default values
  - Uses Logger for debug output

### Exported Types
- `ParsedArguments`: Parsed command-line structure
- `ParserConfig`: Parser configuration
- `ValidationResult`: Argument validation outcome

### Component Guarantees
- **Type Safety**: Runtime type validation
- **Hierarchical Commands**: Nested command support
- **Auto Documentation**: Help and usage generation
- **POSIX Compliance**: Standard option parsing

---

## Component Initialization Patterns

### Dependency-Aware Initialization

```
// Language-agnostic initialization pattern
function initializeQiCore() -> Result<QiCoreContainer>:
  // Base is always available (no initialization needed)
  base = BaseComponent
  
  // Initialize Core components with async operations
  configResult = fromFile("app.json")  // async
  if isFailure(configResult) then
    return failure(getError(configResult))
  end
  config = unwrap(configResult)
  
  // Logger initialization (sync)
  loggerResult = create(get("logging", config))
  if isFailure(loggerResult) then
    return failure(getError(loggerResult))
  end
  logger = unwrap(loggerResult)
  
  // Cache initialization (potentially async for persistent cache)
  cacheConfig = get("cache", config)
  cacheResult = if persistent(cacheConfig) then
    createPersistent(path(cacheConfig), cacheConfig)  // async
  else
    createMemory(cacheConfig)  // sync
  end
    
  if isFailure(cacheResult) then
    log(ERROR, "Cache initialization failed", getError(cacheResult), logger)
    return failure(getError(cacheResult))
  end
  
  // Initialize application components
  core = CoreContainer{config, logger, cache: unwrap(cacheResult)}
  http = createHttp(core)
  document = createDocument(core)
  clp = createCLP(core)
  
  return success(QiCoreContainer{base, core, http, document, clp})
end
```

---

## Component Version Management

### Version Strategy

Each component maintains its own semantic version while ensuring compatibility:

```yaml
component_versions:
  base: "1.0.0"      # Most stable, rarely changes
  core: "1.2.0"      # Moderate change frequency
  http: "1.5.2"      # More frequent updates
  document: "1.3.1"  # Template engine updates
  clp: "1.1.0"       # Stable CLI interface

compatibility_matrix:
  - base: "1.0.0"
    compatible_with:
      core: "1.0.0 - 1.x"
      http: "1.0.0 - 1.x"
      document: "1.0.0 - 1.x"
      clp: "1.0.0 - 1.x"
  
  - core: "1.2.0"
    requires:
      base: "^1.0.0"
    compatible_with:
      http: "1.3.0 - 1.x"
      document: "1.2.0 - 1.x"
      clp: "1.0.0 - 1.x"
```

### Breaking Change Policy

1. **Base Component**: Breaking changes require major version bump of entire framework
2. **Core Component**: Breaking changes require coordinated update of dependent components
3. **Application Components**: Can have breaking changes independently with minor framework version bump

---

## Component Testing Strategy

### Unit Testing
- Each component has isolated unit tests
- Mock implementations for dependencies
- Property-based testing for mathematical laws

### Integration Testing
```
// Language-agnostic integration test pattern
test "HTTP with Cache Integration":
  // Setup
  cache = createMemory(CacheConfig{maxSize: 100})
  http = createHttp(HttpConfig{cache: cache})
  
  // First request hits network
  result1 = get("https://api.example.com/data", http)  // async
  assert(isSuccess(result1))
  
  // Second request hits cache
  result2 = get("https://api.example.com/data", http)  // async
  assert(isSuccess(result2))
  assert(has("https://api.example.com/data", cache))
end
```

---

## Dependencies and References

- **Depends on**: 
  - [Base Component Contracts](qi.v4.base.contracts.md) - Defines Result<T> and QiError behavioral contracts
  - [Core Component Contracts](qi.v4.core.contracts.md) - Defines Configuration, Logger, and Cache behavioral contracts
- **Architecture Summary**: Five components (Base, Core, HTTP, Document, CLP) with clear dependency hierarchy
- **Contract Compliance**: All components must satisfy the behavioral contracts defined in their respective specifications
- **Language Agnostic**: Component architecture applies to all QiCore v4.0 implementations regardless of programming language

---

**Component Architecture**: QiCore v4.0 organizes behavioral contracts into five components with clear dependencies, initialization patterns, and integration requirements that apply across all programming language implementations.