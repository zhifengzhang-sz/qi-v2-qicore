# QiCore v4.0 Foundation

**Mathematical foundation types and essential infrastructure services for QiCore v4.0 platform**

## Overview

QiCore Foundation provides the mathematical and infrastructure building blocks that all QiCore applications depend on:

- **qi/base**: Foundational types (Result<T>, QiError) with category theory foundations
- **qi/core**: Infrastructure services (Config, Logger, Cache) with monoid semantics

## Architecture

```
qi-v2-qicore/
├── docs/contracts/                    # Language-agnostic behavioral contracts
│   ├── qi.v4.base.contracts.md       # Result<T>, QiError contracts
│   ├── qi.v4.core.contracts.md       # Config, Logger, Cache contracts  
│   └── qi.v4.component.contracts.md  # Component architecture
├── haskell/                          # Haskell implementation
├── typescript/                       # TypeScript implementation (future)
├── python/                          # Python implementation (future)
└── rust/                            # Rust implementation (future)
```

## Core Components

### Base Component (`qi/base`)

**Mathematical Foundation**: Category theory-based error handling with monadic composition

```haskell
-- Result<T> monad for pure functional error handling
data Result a = Success a | Failure QiError

-- Composable error type with context chaining
data QiError = QiError
  { code :: Text
  , message :: Text  
  , category :: ErrorCategory
  , context :: Map Text Value
  , cause :: Maybe QiError
  , timestamp :: UTCTime
  }
```

**Key Properties**:
- ✅ Monad, Functor, Applicative laws
- ✅ Type-safe error propagation  
- ✅ Immutable, thread-safe operations
- ✅ Zero runtime overhead composition

### Core Component (`qi/core`)

**Infrastructure Services**: Configuration, logging, and caching with performance contracts

```haskell
-- Monoid-based configuration with right-bias merge
data ConfigData = ConfigData 
  { configData :: ConfigObject
  , configTimestamp :: UTCTime
  , configSource :: Text
  }

-- Level-based logging with structured context
data Logger = Logger
  { loggerConfig :: LoggerConfig  
  , loggerContext :: TVar LogContext
  }

-- High-performance caching with TTL and eviction
data Cache = Cache
  { cacheConfig :: CacheConfig
  , cacheStorage :: TVar CacheStorage
  , cachePath :: Maybe FilePath
  }
```

**Key Properties**:
- ✅ O(1) operations for core functionality
- ✅ STM-based concurrency safety
- ✅ Resource cleanup and management
- ✅ Configuration-driven initialization

## Language-Agnostic Contracts

All implementations must satisfy the behavioral contracts defined in `docs/contracts/`:

### Base Contracts
- **Result<T>**: Monadic error handling with functor/applicative laws
- **QiError**: Structured error representation with context chaining

### Core Contracts  
- **Configuration**: Monoid merge semantics with validation
- **Logger**: Level-based filtering with performance guarantees
- **Cache**: Map semantics with TTL and eviction policies

## Usage

### Haskell Implementation

```bash
cd haskell
nix develop
cabal build qi-base qi-core
cabal test
```

### Cross-Language Compatibility

The foundation contracts ensure identical behavior across all language implementations:

```yaml
# Mathematical laws verified across all languages
result_monad_laws:
  - "flatMap(f) ∘ flatMap(g) == flatMap(x => flatMap(f)(g(x)))"
  - "flatMap(success) == identity"
  - "map(f) ∘ map(g) == map(f ∘ g)"

configuration_monoid_laws:
  - "merge([a, merge([b, c])]) == merge([merge([a, b]), c])" 
  - "merge([empty, config]) == config"
  - "merge([config, empty]) == config"
```

## Dependencies

### Haskell
- **GHC 9.10+**: Modern Haskell with GHC2024 language edition
- **Core Libraries**: text, containers, stm, aeson, time
- **Development**: hspec, QuickCheck, tasty (for testing)

### Build Requirements
- **Nix**: Reproducible development environment
- **Cabal 3.6+**: Modern Haskell build system

## Testing

```bash
# Unit tests for mathematical laws
cabal test qi-base:test

# Integration tests for infrastructure services  
cabal test qi-core:test

# Property-based testing with QuickCheck
cabal test --test-options="--quickcheck-tests=1000"
```

## Performance Guarantees

### Base Component
- **Result operations**: O(1) for all transformations
- **Error construction**: O(1) with context accumulation
- **Memory overhead**: < 64 bytes per Result instance

### Core Component  
- **Config get operations**: O(1) for direct keys, O(depth) for nested
- **Logger level checking**: O(1) with early exit optimization
- **Cache operations**: O(1) average case with STM concurrency

## Related Projects

- **[qi-v2-dp-actor](../qi-v2-dp-actor)**: Data processing actors built on this foundation
- **QiCore Applications**: Any project requiring foundational types and infrastructure

## Contract Compliance

This implementation satisfies **ALL** contracts defined in the language-agnostic specifications. Any implementation claiming QiCore v4.0 compatibility must pass the same contract verification tests.

---

**QiCore Foundation**: The mathematical and infrastructure bedrock for composable, high-performance data processing systems.