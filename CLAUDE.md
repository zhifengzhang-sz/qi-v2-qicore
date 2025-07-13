# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

QiCore v4.0 Foundation provides mathematical foundation types and infrastructure services for the QiCore platform. The repository implements category theory-based Result<T> monadic error handling and essential services (Config, Logger, Cache) across multiple languages with language-agnostic behavioral contracts.

## Key Architecture

### Multi-Language Implementation
- **haskell/**: Reference implementation with mathematical rigor
- **typescript/**: Production-ready TypeScript implementation (planned)
- **docs/contracts/**: Language-agnostic behavioral contracts that all implementations must satisfy

### Core Components
- **qi/base**: Foundational types (Result<T>, QiError) with monadic composition
- **qi/core**: Infrastructure services (Config, Logger, Cache) with performance contracts

## Common Development Commands

### Haskell Implementation
```bash
# Enter Nix development environment
cd haskell && nix develop

# Build all components
cabal build qi-base qi-core

# Run tests with property-based verification
cabal test

# Run property tests with increased iterations
cabal test --test-options="--quickcheck-tests=1000"

# Build specific library
cabal build qi-base     # Base mathematical foundations
cabal build qi-core     # Infrastructure services

# Run specific test suite
cabal test qi-qicore-test
```

### TypeScript Implementation (when available)
```bash
cd typescript

# Install dependencies
bun install

# Build library with dual CJS/ESM output
bun run build

# Run all tests with coverage
bun run test:coverage

# Property-based testing
bun run test:properties

# Format and lint code
bun run format && bun run lint

# Type checking
bun run typecheck

# Complete validation (format + lint + test + typecheck)
bun run check

# Performance benchmarks
bun run bench
```

## Development Workflow

### For Base Component Changes (Result<T>, QiError)
1. **Always** verify mathematical laws hold with property-based tests
2. Update contracts in `docs/contracts/qi.base.contracts.md` if behavior changes
3. Ensure cross-language consistency - changes must be implementable in all target languages
4. Run full test suite: `cabal test --test-options="--quickcheck-tests=1000"`

### For Core Component Changes (Config, Logger, Cache)
1. Test performance contracts are maintained (O(1) operations)
2. Verify STM concurrency safety in Haskell implementation
3. Update contracts in `docs/contracts/qi.core.contracts.md` if needed
4. Test resource cleanup and error handling

### Contract Compliance
- All implementations must satisfy behavioral contracts in `docs/contracts/`
- Mathematical laws (Functor, Monad, Applicative) are non-negotiable
- Performance requirements are specified and must be maintained
- Use property-based testing to verify contracts across all implementations

## Key Patterns and Conventions

### Result<T> Usage Patterns
```haskell
-- Monadic composition for error propagation
processData :: Input -> Result Output
processData input = do
  validated <- validateInput input
  processed <- computeResult validated  
  saveResult processed
```

### Error Handling Best Practices
- Use specific ErrorCategory values for retry strategy determination
- Chain errors with `withCause` to preserve causal relationships  
- Accumulate context with `withContext` for debugging
- Never throw exceptions - always return Result<T>

### Configuration Service Patterns
- Use monoid merge semantics with right-bias (later configs override earlier)
- Validate configuration against schemas where possible
- Support multiple sources: files (JSON/YAML), environment variables, objects

### Infrastructure Service Initialization
- Services depend on validated configuration
- Use STM for thread-safe state management
- Implement proper resource cleanup

## Testing Requirements

### Mathematical Law Verification
- **Required**: Property-based tests for all Functor/Monad/Applicative laws
- **Required**: QuickCheck tests with minimum 1000 iterations
- **Required**: Edge case coverage (null, empty, large data)

### Performance Verification  
- **Required**: Benchmark tests for O(1) operation guarantees
- **Required**: Memory usage regression tests
- **Required**: Concurrency safety tests for STM operations

### Integration Testing
- **Required**: Cross-component interaction tests
- **Required**: Configuration loading from multiple sources
- **Required**: Error propagation through service layers

## Build Dependencies

### Haskell
- **Nix**: Required for reproducible development environment
- **GHC 9.10+**: Modern Haskell with GHC2024 language edition
- **Cabal 3.6+**: For building and testing

### TypeScript (when implemented)
- **Bun**: Preferred runtime and package manager
- **Biome**: For formatting and linting
- **Vitest**: For testing with property-based support (fast-check)

## Critical Constraints

### Never Do
- Break mathematical laws (Functor, Monad, Applicative)
- Introduce runtime exceptions in Result<T> operations
- Compromise O(1) complexity guarantees for core operations
- Change contracts without updating all implementations

### Always Do
- Run property-based tests before committing
- Update behavioral contracts when changing semantics
- Maintain cross-language compatibility
- Verify performance requirements are met

### Repository Structure Invariants
- `docs/contracts/` contains the authoritative behavioral specifications
- Haskell implementation serves as the mathematical reference
- All language implementations must pass identical contract verification tests
- Build systems must enforce the same code quality standards across languages

## Contract Verification

The repository includes comprehensive behavioral contracts that specify exact mathematical properties and performance requirements. Any changes to core components must maintain contract compliance across all implementations.

### **Critical Contract Verification Requirements:**

#### Mathematical Law Verification (MANDATORY)
```bash
# Property-based testing for all mathematical laws
cabal test qi-base:test --test-options="--quickcheck-tests=1000"

# Verify Functor laws: map(id) == id, map(f ∘ g) == map(f) ∘ map(g)
# Verify Monad laws: left identity, right identity, associativity
# Verify Applicative laws: identity, composition, homomorphism, interchange
```

#### Cross-Language Consistency Tests
- **ALL** language implementations must pass identical behavioral tests
- Result operations must have identical semantics across TypeScript, Haskell, Python, C++
- Error handling patterns must be consistent
- Performance characteristics must be within specified bounds

#### Contract Compliance Checklist (Before Implementation)
- [ ] All factory operations implemented with correct signatures
- [ ] All transformation operations (map, flatMap, andThen) follow mathematical laws
- [ ] All collection operations (partition, sequence, combine2) handle edge cases
- [ ] All async operations properly handle language-specific async patterns
- [ ] All error operations (chain, formatChain, hasCategory) work correctly
- [ ] Performance requirements met (O(1) operations, etc.)

#### Repository Scope Clarification
**IMPORTANT**: This repository implements QiCore **Foundation Only** (Base + Core components):
- ✅ Base: Result<T>, QiError with all mathematical operations
- ✅ Core: Configuration, Logger, Cache with 2025 patterns
- ❌ Application Components: HTTP, Document, CLP are in separate repositories

Key verification points:
- Mathematical laws verified with property-based testing (minimum 1000 test cases)
- Performance characteristics benchmarked and regression-tested  
- Cross-language behavioral consistency ensured via shared test suites
- Error handling patterns consistent across all implementations
- 2025 patterns (OpenTelemetry, Valkey, dependency injection) properly integrated