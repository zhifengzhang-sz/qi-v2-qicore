# QiCore Haskell Testing Guide - 2025 Edition

This document outlines the modern testing practices and infrastructure for the QiCore Haskell implementation, updated for 2025 with current best practices.

## Overview

The QiCore Haskell testing approach follows 2025 best practices using:
- **Tasty** as the unified testing framework
- **QuickCheck** for property-based testing
- **GHC 9.12.1** with modern Haskell features
- **Nix flakes** for reproducible development environments

## Testing Framework Architecture

### Tasty Framework (Modern Foundation)

We use Tasty as the foundational testing framework because it:
- Combines multiple test types (unit, property, golden, etc.) into a single test suite
- Provides excellent resource management and configurability
- Offers extensible API for custom test providers
- Integrates seamlessly with CI/CD pipelines

### Property-Based Testing with QuickCheck

QuickCheck remains the gold standard for property-based testing in Haskell:
- **Mathematical Law Verification**: All Functor, Applicative, and Monad laws
- **Contract Compliance**: Direct mapping to behavioral contracts
- **High Coverage**: 100 test cases per property by default
- **Deterministic Shrinking**: Automatic test case minimization

## Test Organization

### Test Suite Structure

```
test/
├── Main.hs                    # Main test runner
└── [future test modules]      # Additional test files as needed
```

### Test Categories

1. **Mathematical Law Verification**
   - Functor laws (identity, composition)
   - Applicative laws (identity, homomorphism, interchange)
   - Monad laws (left identity, right identity, associativity)

2. **Contract Compliance Tests**
   - Factory operations (success, failure, fromMaybe, etc.)
   - Query properties (isSuccess, isFailure, getValue, etc.)
   - Transformation operations (map, flatMap, andThen, etc.)
   - Extraction operations (unwrapOr, match, etc.)

3. **Error Handling Tests**
   - QiError construction and manipulation
   - Error chaining and context preservation
   - Severity and category management

## Test Configuration

### QuickCheck Configuration

```haskell
-- Default configuration in test/Main.hs
main :: IO ()
main = defaultMain $ testGroup "QiCore Base Component Tests"
  [ -- Test groups here
  ]
```

### Property Test Sizes

- **Default**: 100 test cases per property
- **For complex properties**: Can be increased using `QC.withMaxSuccess`
- **For simple properties**: Standard 100 cases provide sufficient coverage

## Test Data Generation (2025 Best Practices)

### Arbitrary Instances with Modern Patterns

We provide comprehensive Arbitrary instances following 2025 best practices:

```haskell
-- Use smart constructors for domain-valid data
instance Arbitrary Text where
  arbitrary = T.pack <$> listOf1 (choose ('a', 'z'))  -- Non-empty, valid text

instance Arbitrary ErrorCategory where
  arbitrary = arbitraryBoundedEnum  -- Automatic enumeration support

-- Advanced QiError generation with controlled complexity
instance Arbitrary QiError where
  arbitrary = sized $ \n -> do
    code <- arbitrary
    message <- arbitrary  
    category <- arbitrary
    severity <- arbitrary
    let timestamp = read "1970-01-01 00:00:00 UTC"  -- Deterministic timestamp
    context <- if n > 0 
               then fmap Map.fromList $ listOf $ (,) <$> arbitrary <*> pure (toJSON ())
               else pure Map.empty
    cause <- if n > 1  -- Controlled recursive depth
             then frequency [(1, pure Nothing), (1, Just <$> resize (n `div` 2) arbitrary)]
             else pure Nothing
    pure $ QiError code message category context cause timestamp severity

-- Type-safe Result generation
instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = frequency
    [ (7, Success <$> arbitrary)  -- Bias toward success cases
    , (3, Failure <$> arbitrary)  -- Include failure scenarios
    ]
```

### Advanced Generator Patterns

**Conditional Generation:**
```haskell
-- Generate valid vs invalid inputs separately
validInputGen :: Gen Text
validInputGen = T.pack <$> listOf1 (elements ['a'..'z'])

invalidInputGen :: Gen Text  
invalidInputGen = T.pack <$> oneof
  [ pure ""  -- Empty string
  , listOf (elements ['!', '@', '#'])  -- Invalid characters
  ]
```

**Monadic Testing for IO Operations:**
```haskell
-- Test IO-based error operations
prop_fromExceptionIO :: Property
prop_fromExceptionIO = monadicIO $ do
  result <- run $ QE.fromExceptionIO (throwIO (userError "test"))
  assert (R.isFailure result)
```

### Key Testing Principles (2025 Standards)

1. **Zero Mocking Policy**: All tests use real data structures and pure functions
2. **Complete Implementation Requirement**: Absolutely no fake, stub, or undefined code
3. **Deterministic Test Design**: Fixed seeds, timestamps, and controlled randomness
4. **Mathematical Law Compliance**: Rigorous property-based verification of category theory laws
5. **Contract-Driven Testing**: Direct mapping between tests and behavioral contracts
6. **Performance Awareness**: O(1) complexity guarantees verified through testing
7. **Type Safety**: Leverage GHC 9.12's type system for compile-time verification

## Running Tests

### Development Environment

```bash
# Enter Nix development shell with GHC 9.12
nix develop

# Run all tests
cabal test

# Run with detailed output
cabal test --test-show-details=direct

# Run specific test suite
cabal test qi-base-test
```

### Continuous Integration

Tests run automatically on:
- All commits to main branch
- Pull requests
- Release candidates

### Test Coverage Expectations

- **Mathematical Laws**: 100% coverage of all required laws
- **Contract Operations**: All operations from behavioral contracts tested
- **Edge Cases**: Null, empty, and boundary conditions covered
- **Error Paths**: All error handling scenarios verified

## Best Practices

### Writing New Tests

1. **Use Property-Based Testing**: Prefer properties over specific examples
2. **Test Contracts Directly**: Map test names to contract requirements
3. **Use Descriptive Names**: Test names should explain the property being tested
4. **Group Related Tests**: Use testGroup to organize related properties

### Test Organization Principles

```haskell
-- Good: Clear test organization
testGroup "Mathematical Law Verification"
  [ functorLawTests
  , applicativeLawTests  
  , monadLawTests
  ]

-- Good: Descriptive property names
QC.testProperty "Identity Law: fmap id == id" $ \(r :: Result Int) ->
  R.map id r === r
```

### Performance Testing

While not the focus of this test suite, we include basic performance verification:
- O(1) operation guarantees for core functions
- Memory usage regression prevention
- No performance-critical paths in test code itself

## Integration with Development Workflow

### Pre-commit Hooks

Tests should be run before commits using:
```bash
cabal test qi-base-test --test-show-details=direct
```

### IDE Integration

The test suite integrates with:
- Haskell Language Server (HLS)
- ghcid for fast feedback
- VS Code Haskell extension

## Future Enhancements

### Potential Additions

1. **Hedgehog Integration**: For more sophisticated generators if needed
2. **Golden Tests**: For serialization format stability
3. **Benchmark Tests**: Using criterion for performance regression testing
4. **Mutation Testing**: Using tools like MuCheck for test quality assessment

### Migration Path

This testing approach is designed to:
- Scale with additional QiCore components
- Support multiple target languages (TypeScript, Python, etc.)
- Maintain consistency across implementations
- Provide clear upgrade paths for new testing tools

## Conclusion

The QiCore testing strategy emphasizes mathematical rigor, contract compliance, and modern tooling. By using Tasty + QuickCheck with GHC 9.12, we ensure comprehensive coverage while maintaining development velocity and code quality.

All tests must pass before release, and the test suite serves as both verification and documentation of the QiCore mathematical foundation.