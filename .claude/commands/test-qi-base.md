# Test QiBase Implementation

Run comprehensive test suite for qi/base component with property-based testing.

This command executes the complete test suite for QiCore Foundation's mathematical Result<T> and QiError implementations, verifying category theory laws and behavioral contracts.

## What This Tests

1. **Mathematical Laws**: Functor, Applicative, and Monad laws (automatically satisfied via Either)
2. **Behavioral Contracts**: All 60+ operations from qi.base.contracts.md
3. **Error Handling**: QiError construction, chaining, and manipulation
4. **Edge Cases**: Boundary conditions and error scenarios
5. **JSON Serialization**: Modern aeson 2.2+ compatibility

## Test Framework

- **Framework**: Tasty + QuickCheck (2025 standards)
- **Property-Based**: 1000+ test cases per mathematical law
- **Coverage**: Complete contract operation coverage
- **No Mocking**: All tests use real data structures
- **GHC 9.12.1**: Verified with cutting-edge compiler

## Test Categories

### Core Mathematical Properties
- **Functor Laws**: `fmap id = id`, `fmap (f . g) = fmap f . fmap g`
- **Applicative Laws**: Identity, composition, homomorphism, interchange
- **Monad Laws**: Left identity, right identity, associativity

### Contract Operations
- **Factory**: success, failure, fromTryCatch, fromMaybe, fromEither
- **Query**: isSuccess, isFailure, getValue, getData, getError
- **Transform**: map, flatMap, andThen, inspect, collect, filter
- **Collection**: sequence, traverse, partition, lefts, rights

### Error Management
- **Construction**: QiError with 14 ErrorCategory values
- **Context**: withContext, withCause for error enrichment
- **Chaining**: chain, formatChain, getRootError
- **Serialization**: JSON round-trip testing

## Quality Standards

- **Zero Fake Code**: No undefined, TODO, or stub implementations
- **Contract Compliance**: 100% behavioral contract satisfaction
- **Performance**: O(1) complexity for core operations
- **Memory Safety**: STM-based concurrency patterns

## Usage

Run this command to:
- Verify implementation correctness before releases
- Test after making changes to Result or Error types
- Validate mathematical law compliance
- Check regression after dependency updates

!cd haskell && nix develop --command cabal test qi-base-test --test-show-details=direct