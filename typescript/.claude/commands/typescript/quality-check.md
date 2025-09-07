# TypeScript Quality Check

**INSTRUCTION FOR AI ASSISTANT**: Perform comprehensive quality checks for TypeScript projects.

## TypeScript Quality Check Sequence

### 1. TypeScript Type Checking
```bash
# Navigate to TypeScript directory first
cd typescript

# Run TypeScript compiler check
bun run typecheck
# OR
npx tsc --noEmit
```

**Verify:**
- No TypeScript errors or warnings
- Strict mode compliance
- Proper type annotations
- Modern TypeScript feature usage

### 2. Code Quality with Biome
```bash
# Navigate to TypeScript directory first
cd typescript

# Linting and formatting check
bun run lint
# OR
bunx @biomejs/biome check .
```

**Check for:**
- Code formatting consistency
- Import organization
- Unused variables/imports
- Code complexity issues
- Style guide compliance

### 3. Testing with Vitest
```bash
# Navigate to TypeScript directory first
cd typescript

# Run tests with coverage
bun run test:coverage
# OR
bunx vitest run --coverage
```

**Verify:**
- All unit tests pass
- Property-based tests pass (if using fast-check)
- Test coverage above threshold (typically 90%)
- No flaky or skipped tests

### 4. Build Verification
```bash
# Build check
bun run build
# OR
bunx vite build
```

**Ensure:**
- Build completes without errors
- Bundle size within acceptable limits
- All assets properly generated
- No missing dependencies

### 5. Dependency Security Check
```bash
# Check for vulnerabilities
bun audit
# OR
npm audit
```

**Review:**
- No high-severity vulnerabilities
- Dependencies up to date
- License compatibility
- Bundle size impact

## TypeScript-Specific Quality Gates

### Type Safety Verification
- [ ] Strict TypeScript configuration enabled
- [ ] No `any` types in production code
- [ ] Proper error handling with Result<T> or similar patterns
- [ ] Type guards for runtime validation
- [ ] Branded types for domain-specific values

### Modern TypeScript Usage
- [ ] Uses satisfies operator where appropriate
- [ ] Leverages template literal types
- [ ] Applies const assertions for immutable data
- [ ] Uses modern utility types (Pick, Omit, etc.)
- [ ] Proper generic constraints and variance

### Build Tool Integration
- [ ] Bun configuration optimized
- [ ] Biome rules properly configured
- [ ] Vitest setup with TypeScript support
- [ ] Source maps generated for debugging
- [ ] Tree shaking working correctly

### Performance Considerations
- [ ] Bundle size within limits
- [ ] No circular dependencies
- [ ] Lazy loading implemented where beneficial
- [ ] Type-only imports used appropriately
- [ ] Compilation time reasonable

## Automated Quality Pipeline

### Full Quality Check Script
```bash
#!/bin/bash
echo "🔍 Running TypeScript Quality Checks..."

echo "📝 Type checking..."
bun run typecheck || exit 1

echo "🎨 Linting and formatting..."
bun run lint || exit 1

echo "🧪 Running tests..."
bun run test || exit 1

echo "🏗️ Building..."
bun run build || exit 1

echo "🔒 Security audit..."
bun audit || echo "⚠️ Security issues found - review required"

echo "✅ All TypeScript quality checks passed!"
```

## Failure Response

**If any check fails:**
1. **Type Errors**: Fix TypeScript compilation issues
2. **Lint Errors**: Apply Biome fixes or manually resolve
3. **Test Failures**: Debug and fix failing tests
4. **Build Errors**: Resolve build configuration or dependency issues
5. **Security Issues**: Update vulnerable dependencies

## Success Criteria

**All quality gates must pass:**
- ✅ TypeScript compiles without errors
- ✅ Biome linting passes
- ✅ All tests pass with good coverage
- ✅ Build succeeds and generates correct output
- ✅ No critical security vulnerabilities
- ✅ Modern TypeScript patterns applied correctly

**Ready for production deployment with confidence in TypeScript code quality.**