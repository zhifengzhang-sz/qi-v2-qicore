# Cross-Language Test Strategy - QiCore Foundation

## Overview

This document defines the comprehensive strategy for ensuring behavioral consistency and compatibility across all QiCore Foundation language implementations (Haskell, TypeScript, Python, etc.).

## Core Testing Principles

### 1. Behavioral Consistency
- **Identical mathematical laws** across all implementations
- **Same error semantics** and propagation behavior
- **Equivalent performance characteristics** within acceptable bounds
- **Compatible serialization formats** for cross-language data exchange

### 2. Contract Compliance
- All implementations must satisfy the same behavioral contracts
- Property-based testing must verify identical mathematical properties
- Performance requirements must be met across all languages

## Cross-Language Test Architecture

### Test Data Generation

**Shared Test Cases (JSON Format):**
```json
{
  "functor_identity_tests": [
    {
      "input": {"tag": "success", "value": 42},
      "expected": {"tag": "success", "value": 42},
      "description": "Functor identity law with integer"
    },
    {
      "input": {"tag": "failure", "error": {"code": "TEST_ERROR", "message": "test"}},
      "expected": {"tag": "failure", "error": {"code": "TEST_ERROR", "message": "test"}},
      "description": "Functor identity law with error"
    }
  ],
  "monad_left_identity_tests": [
    {
      "input": 42,
      "function": "toString",
      "expected": {"tag": "success", "value": "42"},
      "description": "Monad left identity with number to string"
    }
  ]
}
```

**Test Case Categories:**
1. **Mathematical Law Tests** - Functor, Monad, Applicative laws
2. **Error Handling Tests** - Error propagation and categorization
3. **Async Operation Tests** - Promise/Future integration patterns
4. **Serialization Tests** - JSON compatibility across languages
5. **Performance Benchmark Tests** - Relative performance validation

### Implementation Strategy

#### Phase 1: Haskell Reference Implementation
```haskell
-- Generate canonical test results from Haskell reference
generateTestResults :: TestCase -> IO TestResult
generateTestResults testCase = do
  result <- runTest testCase
  pure $ TestResult
    { testId = testCase.id
    , input = testCase.input
    , output = result
    , timestamp = getCurrentTime
    , implementation = "haskell-reference"
    }
```

#### Phase 2: TypeScript Validation
```typescript
// Validate TypeScript implementation against Haskell results
interface CrossLanguageTest {
  testId: string
  description: string
  haskellResult: unknown
  typescriptResult: unknown
  isCompatible: boolean
}

const validateBehavioralConsistency = async (
  testCases: TestCase[]
): Promise<CrossLanguageTest[]> => {
  return Promise.all(
    testCases.map(async testCase => {
      const tsResult = await runTypeScriptTest(testCase)
      const hsResult = await loadHaskellReference(testCase.id)
      
      return {
        testId: testCase.id,
        description: testCase.description,
        haskellResult: hsResult,
        typescriptResult: tsResult,
        isCompatible: deepEqual(hsResult, tsResult)
      }
    })
  )
}
```

## Mathematical Law Verification

### Functor Laws Cross-Language Verification

**Test Generation:**
```yaml
functor_identity_law:
  description: "map(id) === id across all languages"
  test_cases:
    - input_type: "number"
      values: [0, 1, -1, 42, 3.14, -3.14, Infinity, -Infinity]
    - input_type: "string" 
      values: ["", "hello", "unicode: 🦄", "multiline\nstring"]
    - input_type: "object"
      values: [{}, {"key": "value"}, {"nested": {"key": "value"}}]
    - input_type: "array"
      values: [[], [1,2,3], ["a","b","c"]]

functor_composition_law:
  description: "map(f ∘ g) === map(f) ∘ map(g) across all languages"
  function_pairs:
    - f: "x => x * 2"
      g: "x => x + 1"
      input_type: "number"
    - f: "s => s.toUpperCase()"
      g: "s => s.trim()"
      input_type: "string"
```

**Verification Process:**
1. **Generate test inputs** in language-agnostic JSON format
2. **Execute identical tests** in each language implementation
3. **Compare outputs** for exact behavioral consistency
4. **Report discrepancies** with detailed analysis

### Monad Laws Cross-Language Verification

**Left Identity Law:**
```yaml
monad_left_identity:
  test_pattern: "flatMap(f)(Ok(x)) === f(x)"
  verification:
    - language: "haskell"
      syntax: "return x >>= f == f x"
    - language: "typescript" 
      syntax: "from(Ok(x)).flatMap(f).build() === f(x)"
    - language: "python"
      syntax: "Ok(x).flat_map(f) == f(x)"
```

**Cross-Language Test Suite:**
```typescript
describe('Cross-Language Monad Law Verification', () => {
  const testCases = loadSharedTestCases('monad_laws.json')
  
  testCases.forEach(testCase => {
    test(`Monad ${testCase.law} - ${testCase.description}`, async () => {
      const result = await runCrossLanguageTest(testCase)
      expect(result.allLanguagesConsistent).toBe(true)
      
      if (!result.allLanguagesConsistent) {
        console.error('Language discrepancies:', result.discrepancies)
      }
    })
  })
})
```

## Error Handling Consistency

### QiError Serialization Compatibility

**Canonical JSON Format:**
```typescript
interface QiErrorSerialized {
  code: string                          // Must match across languages
  message: string                       // Must match across languages  
  category: string                      // Must match ErrorCategory enum
  context: Record<string, unknown>      // Must serialize consistently
  cause?: QiErrorSerialized           // Recursive error chaining
  timestamp: number                     // Unix timestamp in milliseconds
  severity?: 'low' | 'medium' | 'high' // Optional severity level
}
```

**Cross-Language Validation:**
```yaml
error_serialization_tests:
  simple_error:
    haskell_json: |
      {
        "code": "VALIDATION_FAILED",
        "message": "Input validation failed", 
        "category": "VALIDATION",
        "context": {"field": "email"},
        "timestamp": 1704067200000
      }
    typescript_json: |
      {
        "code": "VALIDATION_FAILED",
        "message": "Input validation failed",
        "category": "VALIDATION", 
        "context": {"field": "email"},
        "timestamp": 1704067200000
      }
    expected_equality: true

  chained_error:
    description: "Error with cause chain must serialize identically"
    validation: "cause field recursively matches structure"
```

### Error Category Mapping Verification

**Category Consistency Test:**
```typescript
describe('ErrorCategory Cross-Language Consistency', () => {
  test('All categories map to same string values', () => {
    const haskellCategories = loadHaskellErrorCategories()
    const typescriptCategories = Object.values(ErrorCategory)
    
    expect(typescriptCategories.sort()).toEqual(haskellCategories.sort())
  })
  
  test('Retry strategies match across languages', () => {
    Object.values(ErrorCategory).forEach(category => {
      const hsStrategy = getHaskellRetryStrategy(category)
      const tsStrategy = getRetryStrategy(createError({ category }))
      expect(tsStrategy).toBe(hsStrategy)
    })
  })
})
```

## Performance Equivalence Testing

### Benchmark Specification

**Performance Contracts:**
```yaml
performance_requirements:
  result_map_operation:
    complexity: "O(1)"
    typescript_max_overhead: "50% vs Haskell"
    baseline_ops_per_second: 1000000
    
  result_flatmap_operation:
    complexity: "O(1)" 
    typescript_max_overhead: "100% vs Haskell"
    baseline_ops_per_second: 800000
    
  fluent_chaining_10_ops:
    complexity: "O(1)"
    typescript_max_overhead: "200% vs Haskell"
    baseline_ops_per_second: 100000
    
  error_chain_traversal:
    complexity: "O(chain_length)"
    typescript_max_overhead: "20% vs Haskell"
    max_chain_length: 1000
```

**Benchmark Implementation:**
```typescript
interface PerformanceBenchmark {
  name: string
  iterations: number
  haskellBaseline?: number
  typescriptResult: number
  overheadPercentage: number
  withinContract: boolean
}

const runPerformanceSuite = async (): Promise<PerformanceBenchmark[]> => {
  const benchmarks: PerformanceBenchmark[] = []
  
  // Result.map performance
  const mapBenchmark = await bench('Result.map O(1)', () => {
    from(Ok(42)).map(x => x * 2).build()
  }, { iterations: 1000000 })
  
  benchmarks.push({
    name: 'Result.map',
    iterations: 1000000,
    typescriptResult: mapBenchmark.opsPerSecond,
    overheadPercentage: calculateOverhead(mapBenchmark.opsPerSecond),
    withinContract: mapBenchmark.opsPerSecond >= 1000000
  })
  
  return benchmarks
}
```

## Async Operation Consistency

### Promise/Result Integration Testing

**Async Pattern Verification:**
```typescript
describe('Cross-Language Async Patterns', () => {
  test('Promise rejection becomes Result failure consistently', async () => {
    const testPromise = Promise.reject(new Error('Async failure'))
    
    // TypeScript
    const tsResult = await asyncTryCatch(() => testPromise)
    expect(tsResult.tag).toBe('failure')
    
    // Compare with Haskell IO behavior (conceptually)
    const expectedBehavior = await loadHaskellAsyncReference('promise_rejection')
    expect(tsResult).toMatchBehavior(expectedBehavior)
  })
  
  test('Fluent async chaining preserves types across languages', async () => {
    const pipeline = from(Ok('123'))
      .mapAsync(async str => parseInt(str))
      .then(builder => builder.map(num => num * 2))
      .then(builder => builder.build())
    
    const result = await pipeline
    expect(result).toEqual(Ok(246))
    
    // Verify Haskell equivalent produces same result
    const haskellEquivalent = await runHaskellAsyncPipeline('string_to_doubled_int')
    expect(result).toMatchHaskellResult(haskellEquivalent)
  })
})
```

## Test Execution Framework

### Continuous Integration Setup

**Multi-Language CI Pipeline:**
```yaml
# .github/workflows/cross-language-tests.yml
name: Cross-Language Consistency Tests

on: [push, pull_request]

jobs:
  generate-haskell-reference:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Setup Haskell
        uses: haskell/actions/setup@v2
      - name: Generate reference test results
        run: |
          cd haskell
          cabal run test-generator -- --output ../test-results/haskell-reference.json
      - name: Upload reference results
        uses: actions/upload-artifact@v3
        with:
          name: haskell-reference
          path: test-results/haskell-reference.json

  validate-typescript:
    needs: generate-haskell-reference
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: oven-sh/setup-bun@v1
      - name: Download Haskell reference
        uses: actions/download-artifact@v3
        with:
          name: haskell-reference
          path: test-results/
      - name: Run cross-language validation
        run: |
          cd typescript
          bun install
          bun run test:cross-language --reference ../test-results/haskell-reference.json
```

### Local Development Testing

**Development Workflow:**
```bash
# Generate test cases from Haskell reference
cd haskell && cabal run generate-test-cases

# Validate TypeScript implementation
cd typescript && bun run test:cross-language

# Check performance benchmarks  
bun run bench:cross-language

# Generate compatibility report
bun run report:cross-language
```

**Test Report Format:**
```typescript
interface CrossLanguageReport {
  summary: {
    totalTests: number
    passingTests: number
    failingTests: number
    consistencyPercentage: number
  }
  mathLawsConsistency: {
    functorLaws: boolean
    monadLaws: boolean  
    applicativeLaws: boolean
  }
  errorHandlingConsistency: {
    serializationCompatible: boolean
    categoryMappingConsistent: boolean
    retryStrategyConsistent: boolean
  }
  performanceCompliance: {
    withinContractBounds: boolean
    averageOverhead: number
    benchmarkResults: PerformanceBenchmark[]
  }
  discrepancies: CrossLanguageDiscrepancy[]
}
```

## Implementation Timeline

### Phase 1: Foundation (v-0.3.2)
- **Haskell reference test generator**
- **TypeScript cross-language test runner**
- **Basic mathematical law verification**
- **Error serialization compatibility tests**

### Phase 2: Comprehensive Testing (v-0.4.0)
- **Complete performance benchmark suite**
- **Async operation consistency tests**
- **Automated CI/CD integration**
- **Cross-language compatibility reporting**

### Phase 3: Additional Languages (v-0.5.0+)
- **Python implementation validation**
- **C++ implementation validation**  
- **Multi-language compatibility matrix**

---

**Document Status**: Complete ✅  
**Strategy Coverage**: Mathematical Laws, Error Handling, Performance, Async Patterns  
**Implementation Phase**: Ready for v-0.3.2 integration  
**Last Updated**: 2025-01-14