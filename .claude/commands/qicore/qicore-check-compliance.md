# QiCore Foundation Design Compliance Checker

Project-specific compliance verification for QiCore Foundation mathematical requirements.

## QiCore-Specific Compliance Checks:
Use **sequential-thinking** to systematically verify:

### 1. Mathematical Law Compliance (MANDATORY)
- Verify Functor laws: map(id) == id, map(f ∘ g) == map(f) ∘ map(g)
- Verify Monad laws: left identity, right identity, associativity
- Verify Applicative laws: identity, composition, homomorphism, interchange
- Check property-based test coverage (minimum 1000 iterations)
- Validate QuickCheck tests in Haskell implementation
- Validate fast-check tests in TypeScript implementation

### 2. Cross-Language Consistency
- Compare Haskell and TypeScript Result<T> implementations
- Verify identical behavior for all operations
- Check error handling consistency
- Validate performance characteristics match contracts

### 3. QiCore Architecture Compliance
- Verify component boundaries (qi/base vs qi/core)
- Check behavioral contract adherence in docs/contracts/
- Validate O(1) complexity guarantees
- Ensure STM usage in Haskell implementation

### 4. Build System Compliance
- Verify Cabal build configuration
- Check Nix development environment setup
- Validate TypeScript configuration (when implemented)
- Ensure test coverage thresholds met

## QiCore-Specific Quality Gates:
- [ ] All mathematical laws verified with property tests
- [ ] Cross-language behavioral consistency confirmed
- [ ] Performance contracts satisfied
- [ ] Behavioral contracts in docs/contracts/ followed
- [ ] Build systems configured correctly
- [ ] Test coverage above 90%

## Project Context Files to Check:
- docs/contracts/qi.base.contracts.md
- docs/contracts/qi.core.contracts.md
- haskell/qi-base/
- haskell/qi-core/
- CLAUDE.md (project instructions)

## Usage:
"Check QiCore Foundation compliance for Result<T> implementation"
"Verify mathematical laws are satisfied across all implementations"
"Check cross-language consistency between Haskell and TypeScript"