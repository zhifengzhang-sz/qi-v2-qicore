# QiCore Foundation Design-First Workflow

QiCore-specific design compliance workflow with mathematical foundation requirements.

## Mandatory QiCore Pre-Implementation Process:
Use **sequential-thinking** to systematically execute:

### Step 1: Read QiCore Foundation Documentation (**filesystem**)
- Read ./CLAUDE.md (project instructions and constraints)
- Read ./docs/contracts/qi.base.contracts.md (Result<T>, QiError contracts)
- Read ./docs/contracts/qi.core.contracts.md (Config, Logger, Cache contracts)
- Read ./docs/contracts/qi.component.contracts.md (component interfaces)
- Read ./docs/impl/haskell.impl.insights.md (implementation insights)
- Review Haskell reference implementation in ./haskell/

### Step 2: Store QiCore Mathematical Requirements (**memory**)
- Store Functor law requirements (identity, composition)
- Store Monad law requirements (left identity, right identity, associativity)
- Store Applicative law requirements (identity, composition, homomorphism, interchange)
- Remember cross-language consistency requirements
- Store performance contract requirements (O(1) operations)
- Remember STM concurrency patterns for Haskell

### Step 3: Validate Against QiCore Architecture
- Ensure implementation preserves mathematical laws
- Verify cross-language behavioral consistency
- Check that performance contracts can be maintained
- Validate component boundary respect (qi/base vs qi/core)
- Ensure Result<T> monad usage (no exceptions)

### Step 4: QiCore Implementation Planning
- Plan property-based test coverage (minimum 1000 iterations)
- Design for Haskell reference implementation compatibility
- Plan error handling with Result<T> monad patterns
- Design STM integration for concurrent operations (Haskell)

### Step 5: QiCore Compliance Verification Strategy
- Verify mathematical laws with property-based tests
- Plan cross-language consistency testing
- Ensure performance contract validation
- Plan behavioral contract adherence verification

## QiCore Success Criteria:
- All QiCore Foundation contracts read and understood
- Mathematical law preservation strategy planned
- Cross-language consistency approach defined
- Performance contracts compliance verified
- Property-based testing strategy established
- No exception-throwing patterns planned (Result<T> only)

## QiCore-Specific Failure Prevention:
- NEVER implement without reading behavioral contracts first
- NEVER break mathematical laws for convenience
- NEVER use exceptions instead of Result<T> monad
- NEVER compromise cross-language consistency
- NEVER violate O(1) performance contracts
- ALWAYS verify with property-based tests (1000+ iterations)