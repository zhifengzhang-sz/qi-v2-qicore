# QiCore Anti-Pattern Refactor Guide

## Overview
This guide helps eliminate Result<T> anti-patterns across QiCore modules using the `@qi/no-result-anti-patterns` ESLint rule. This is a comprehensive step-by-step guide that assumes no prior knowledge of the codebase.

## Technology Stack (2025)
- **Bun 1.2.22**: Fast JavaScript runtime and package manager
- **Biome 1.9.4**: Rust-based linter/formatter (10x faster than ESLint/Prettier)
- **Vitest 3.0**: Next-generation testing framework with enhanced TypeScript support
- **Custom ESLint**: `@qi/no-result-anti-patterns` rule for detecting improper Result<T> usage

## Project Structure
```
lib/
├── base/        ✅ FOUNDATION - Result<T> & QiError implementation (NEVER MODIFY)
├── core/        ✅ COMPLETED - Config, Logger, Cache (0 violations, reference implementation)
├── amsg/        🔄 NEEDS FIXING - Message queue system
└── cli/         🔄 NEEDS FIXING - CLI framework

docs/
├── tutorial/    📚 Usage patterns and examples
├── api/         📋 API specifications
└── refactor.md  📖 This guide
```

## Phase 1: Learn the Codebase (CRITICAL - Do This First!)

### Step 1: Verify Environment
```bash
# Check location
pwd
# Should output: /home/zzhang/dev/qi/github/qi-v2-qicore/typescript

# Check tools
bun --version        # Should be 1.2.22+
bunx biome --version # Should be 1.9.4+

# See available commands
cat package.json | grep -A 20 '"scripts"'
```

### Step 2: Understand Result<T> Foundation
```bash
# Read the Result<T> implementation (but never modify it)
cat lib/base/src/result.ts | head -100
cat lib/base/src/index.ts

# Learn what functions are available
grep -E "export.*=" lib/base/src/result.ts
```

**Key functions you'll use:**
- `match(onSuccess, onError, result)` - Pattern matching (most important)
- `map(fn, result)` - Transform success values
- `flatMap(fn, result)` - Chain operations that return Result<T>
- `isSuccess(result)` / `isFailure(result)` - Type-safe checking
- `success(value)` / `failure(error)` - Create Results

### Step 3: Study Working Examples
```bash
# See CORRECT usage patterns in core module
grep -r -A 3 -B 1 "match(" lib/core/src/
grep -r -A 3 -B 1 "flatMap(" lib/core/src/
grep -r -A 3 -B 1 "map(" lib/core/src/

# Study the best implementation (config module)
cat lib/core/src/config.ts | grep -A 5 -B 5 "Result<"
```

### Step 4: See What Anti-Patterns Look Like
```bash
# Run linter to see exact violations
bun run lint:amsg
bun run lint:cli

# This output shows you exactly what patterns are forbidden
```

## Anti-Pattern Examples & Fixes

### ❌ VIOLATION: Direct Property Access
**What ESLint detects:**
```typescript
// WRONG - Direct .value access
if (result.tag === "success") {
  return result.value;  // 🚨 Anti-pattern violation
}

// WRONG - Direct .error access
if (result.tag === "failure") {
  console.log(result.error);  // 🚨 Anti-pattern violation
}

// WRONG - Direct .tag checking
if (result.tag === "success") {  // 🚨 Anti-pattern violation
  // do something
}
```

### ✅ CORRECT: Functional Patterns
**How to fix them:**
```typescript
// CORRECT - Using match() for pattern matching
match(
  (value) => {
    // Handle success case
    return processValue(value);
  },
  (error) => {
    // Handle error case
    console.error("Operation failed:", error.message);
    return handleError(error);
  },
  result
);

// CORRECT - Using isSuccess/isFailure for checking
if (isSuccess(result)) {
  // TypeScript knows result.value is available here
  const value = result.value; // This is OK inside type guard
}

// CORRECT - Using combinators for transformation
const processed = flatMap(transform, result);
const mapped = map(processData, result);
const withDefault = unwrapOr(defaultValue, result);
```

### Real Examples from lib/core

**Config module example (from lib/core/src/config.ts:441):**
```typescript
// CORRECT - Using match() for Result<T> handling
get<T = unknown>(path: string): Result<T, ConfigError> {
  // ... implementation
  return match(
    (value) => value,
    () => defaultValue,
    result,
  );
}
```

**Cache module example (from lib/core/src/cache.ts):**
```typescript
// CORRECT - Using flatMap for operation chaining
return flatMap(
  async (value) => {
    const setResult = await this.set(key, value, ttl);
    return isFailure(setResult) ? setResult : factoryResult;
  },
  factoryResult,
);
```

## Phase 2: Create Documentation

### Step 1: tutorial documentation

- Enhance existing tutorials with real examples
- Document common mistakes and corrections

### Step 2: examples

- Add examples in app/src

## Phase 3: Fix Anti-Patterns

### Module Status Check
```bash
# Check current violation counts
echo "=== AMSG Module ==="
bun run lint:amsg 2>&1 | grep -c "error" || echo "0 errors"

echo "=== CLI Module ==="
bun run lint:cli 2>&1 | grep -c "error" || echo "0 errors"

echo "=== Core Module (should be 0) ==="
bun run lint:core 2>&1 | grep -c "error" || echo "0 errors"
```

### Fix amsg Module

**Step 1: Understand the module**
```bash
# Explore structure
find lib/amsg/src -name "*.ts"
cat lib/amsg/src/index.ts

# Understand what it does
cat lib/amsg/package.json | grep description
```

**Step 2: See exact violations**
```bash
bun run lint:amsg
# This shows file:line:column and exact violation
```

**Step 3: Fix each violation systematically**
For each violation:
1. Open the file at the specified line
2. Identify the anti-pattern (usually `result.value`, `result.error`, or `result.tag`)
3. Replace with functional pattern from examples above
4. Test the change

**Step 4: Verify fixes**
```bash
# Test the module
cd lib/amsg && bun run test

# Check for remaining violations
bun run lint:amsg

# Type check
cd lib/amsg && bun run typecheck
```

### Fix cli Module

**Step 1: Understand scope**
```bash
# See how large it is
find lib/cli/src -name "*.ts" | wc -l

# Explore structure
find lib/cli/src -type d
```

**Step 2: Apply same process as amsg**
```bash
bun run lint:cli
# Fix violations one by one using patterns from Phase 1
```

**Step 3: Verify fixes**
```bash
cd lib/cli && bun run test
bun run lint:cli
cd lib/cli && bun run typecheck
```

## Common Patterns by Use Case

### Configuration Access
```typescript
// ❌ WRONG
if (configResult.tag === "success") {
  const value = configResult.value.someProperty;
}

// ✅ CORRECT
const value = match(
  (config) => config.someProperty,
  (error) => defaultValue,
  configResult
);
```

### Async Operations
```typescript
// ❌ WRONG
const result = await someAsyncOp();
if (result.tag === "success") {
  return result.value;
}

// ✅ CORRECT
return flatMapAsync(
  async (value) => processValue(value),
  await someAsyncOp()
);
```

### Error Handling
```typescript
// ❌ WRONG
if (result.tag === "failure") {
  console.error(result.error.message);
}

// ✅ CORRECT
match(
  (value) => processSuccess(value),
  (error) => {
    console.error("Operation failed:", error.message);
    return handleError(error);
  },
  result
);
```

## Quality Assurance

### Testing Commands
```bash
# Module-specific tests
cd lib/amsg && bun run test
cd lib/cli && bun run test

# Full test suite
bun test

# Integration tests
bun vitest run lib/tests/integration/
```

### Lint Commands
```bash
# Specific modules
bun run lint:amsg
bun run lint:cli

# Full project
bun run lint
```

### Type Checking
```bash
# Specific modules
cd lib/amsg && bun run typecheck
cd lib/cli && bun run typecheck

# Full project
bun run typecheck
```

### Complete Quality Check
```bash
# This runs format, lint, and typecheck
bun run check
```

## Success Criteria

- [ ] `bun run lint:amsg` shows 0 violations
- [ ] `bun run lint:cli` shows 0 violations
- [ ] All module tests pass: `cd lib/amsg && bun run test`
- [ ] All module tests pass: `cd lib/cli && bun run test`
- [ ] Type checking passes: `bun run typecheck`
- [ ] Full quality check passes: `bun run check`

## Troubleshooting

### If you see TypeScript errors:
1. Make sure you're importing from `@qi/base`: `import { match, map, flatMap } from "@qi/base"`
2. Check that function signatures match expected types
3. Look at working examples in `lib/core/src/` for reference

### If tests fail:
1. Read the test to understand expected behavior
2. Compare your changes with working patterns in `lib/core/`
3. Use `console.log` to debug the Result<T> values

### If you're unsure about a pattern:
1. Search `lib/core/src/` for similar usage: `grep -r "similar_function" lib/core/src/`
2. Look at the tests: `grep -r "similar_function" lib/tests/core/`
3. Check the base module documentation: `cat lib/base/src/result.ts`

## Reference Files
- `lib/base/src/result.ts` - Result<T> API reference
- `lib/core/src/config.ts` - Best practices implementation
- `lib/tests/core/config.test.ts` - Test patterns
- `docs/tutorial/qi-base-usage.md` - Usage examples
- `docs/tutorial/qi-core-usage.md` - Core module examples

## Important Notes
- **NEVER modify `lib/base/`** - it's the foundation that implements Result<T>
- **Use `lib/core/` as reference** - it has correct patterns and 0 violations
- **Fix one violation at a time** - don't try to fix everything at once
- **Test after each change** - make sure you don't break existing functionality
- **When in doubt, copy patterns from `lib/core/`** - don't guess at the right approach