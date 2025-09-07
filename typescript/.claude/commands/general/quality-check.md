# Quality Check

**INSTRUCTION FOR AI ASSISTANT**: Perform comprehensive quality checks for QiCore Foundation.

## Quality Check Sequence

### 1. Design Compliance Check (**filesystem** + **memory**)
- Read `docs/contracts/qi.base.contracts.md`
- Read `docs/contracts/qi.core.contracts.md`  
- Read `docs/contracts/qi.component.contracts.md`
- Verify implementation follows ALL behavioral contracts
- Check mathematical law preservation
- Validate cross-language consistency requirements

### 2. Mathematical Law Verification
**Run property-based tests:**
```bash
cabal test --test-options="--quickcheck-tests=1000"
```

**Verify specific laws:**
- Functor identity and composition laws
- Monad left identity, right identity, associativity
- Applicative identity, composition, homomorphism, interchange
- Performance contracts (O(1) operations)

### 3. Build and Type Checking
**Haskell:**
```bash
cabal build qi-base qi-core
cabal test
```

**TypeScript (when implemented):**
```bash
bun run typecheck
bun run lint  
bun run test
```

### 4. Code Quality Checks
**Check for:**
- No runtime exceptions (Result<T> monad only)
- Proper error handling patterns
- STM usage for concurrency (Haskell)
- Documentation completeness
- Performance contract adherence

### 5. Cross-Language Consistency (when applicable)
- Compare Haskell and TypeScript implementations
- Verify identical behavioral semantics
- Check error handling consistency
- Validate performance characteristics

## Automated Checks to Run

### Haskell Quality Pipeline
```bash
# Build check
cabal build

# Unit tests with property-based testing
cabal test --test-options="--quickcheck-tests=1000"

# Documentation build
cabal haddock

# Dependency check
cabal outdated
```

### TypeScript Quality Pipeline (when implemented)
```bash
# Type checking
bun run typecheck

# Linting and formatting
bun run lint

# Testing with property-based tests
bun run test

# Build check
bun run build
```

## Quality Gates

**MUST PASS:**
- [ ] All mathematical laws verified (1000+ iterations)
- [ ] All unit tests pass
- [ ] Build succeeds without warnings
- [ ] Design contracts followed exactly
- [ ] No runtime exceptions (Result<T> only)
- [ ] Performance contracts satisfied (O(1) operations)
- [ ] Cross-language consistency maintained

**WARNINGS:**
- [ ] Documentation coverage below 90%
- [ ] Test coverage below 90%
- [ ] New dependencies without approval
- [ ] Breaking API changes

## Failure Response

**If any quality check fails:**
1. Stop and fix issues immediately
2. Do not proceed with commits
3. Re-run quality checks after fixes
4. Document any exceptions or trade-offs

## Success Criteria
All quality gates pass, mathematical rigor maintained, and implementation ready for production use.

**Run this before every commit to ensure QiCore Foundation quality standards.**