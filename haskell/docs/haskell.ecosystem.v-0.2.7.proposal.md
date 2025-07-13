# haskell ecosystem v-0.2.7 proposal

## scope and objectives

### primary goal
Update QiCore Foundation Haskell implementation to modern 2025 ecosystem standards while maintaining mathematical rigor and backward compatibility.

### version progression
- v-0.2.5: Redis/Valkey distributed caching (completed)
- v-0.2.6: Redis operations + YAML/TOML parsing (in progress - needs actual testing)
- **v-0.2.7: Modern Haskell ecosystem update (this proposal)**

## ecosystem updates proposed

### ghc version upgrade
**current**: GHC 9.12.1
**target**: GHC 9.12.2 (latest stable as of 2025)

**new language features available**:
- multiline string literals
- or-patterns for cleaner pattern matching  
- named defaults for better type defaulting

### language edition upgrade
**current**: GHC2021 (in cabal file)
**target**: GHC2024 (now recommended for new code)

**ghc2024 additions**:
- DataKinds
- DerivingStrategies  
- DisambiguateRecordFields
- ExplicitNamespaces
- GADTs (implies MonoLocalBinds)
- LambdaCase
- RoleAnnotations

### package version updates
**current versions** → **target versions**:
- QuickCheck: 2.15 → 2.16.0.0
- aeson: 2.2 → 2.2.3.0 (performance improvements)
- tasty: 1.5 → 1.5.3
- text: current → latest (with fusion framework awareness)
- containers: current → latest
- yaml: 0.11 → latest stable
- toml-parser: 2.0 → latest stable

### nix flake improvements
**updates needed**:
- ghc version: ghc9121 → ghc9122
- package references: update to latest nixpkgs-unstable
- development shell: improved messaging about new features
- tool availability: check for ecosystem compatibility improvements

## implementation plan

### phase 1: language and compiler updates
1. update cabal file language edition to GHC2024
2. update nix flake to GHC 9.12.2
3. test compilation with new language features
4. address any MonoLocalBinds compatibility issues

### phase 2: package dependency updates  
1. update package version bounds in cabal file
2. test build with new package versions
3. update any deprecated API usage
4. verify no breaking changes affect our code

### phase 3: leverage new features (optional)
1. consider using multiline string literals for JSON test data
2. evaluate or-patterns for cleaner pattern matching in Result operations
3. assess DataKinds usage for type-level configuration validation

### phase 4: documentation and verification
1. update compatibility documentation
2. verify all 43 test cases still pass
3. update development setup instructions
4. document new language features available

## compatibility considerations

### backward compatibility
- maintain all existing APIs unchanged
- preserve mathematical law compliance
- keep same Result<T> semantics
- no breaking changes to public interfaces

### migration risks
**MonoLocalBinds impact**:
- GADTs in GHC2024 implies MonoLocalBinds
- may require type signatures in some local bindings
- need to test existing code for type inference issues

**package updates**:
- aeson API changes (should be minimal)
- text fusion framework changes (performance related)
- yaml/toml-parser API stability

### testing strategy
1. compile with new GHC/packages
2. run existing 29 mathematical foundation tests
3. run new 14 feature tests (once v-0.2.6 is properly working)
4. performance regression testing
5. memory usage verification

## expected benefits

### developer experience
- access to modern Haskell language features
- improved error messages from GHC 9.12.2
- better development tools as ecosystem catches up
- enhanced type-level programming capabilities with DataKinds

### performance improvements
- aeson 2.2.3.0 performance enhancements
- GHC 9.12.2 SIMD support for numeric operations
- optimized STM performance
- text library optimizations

### ecosystem alignment
- using recommended GHC2024 language edition
- staying current with Haskell community best practices
- better long-term maintainability
- easier contribution from other Haskell developers

## risk mitigation

### compilation failures
- test incrementally: GHC first, then packages, then language edition
- maintain GHC 9.12.1 fallback option in nix flake
- document any required code changes

### api breaking changes
- carefully review changelog for each package update
- test critical functionality after each update
- maintain compatibility shims if needed

### performance regressions
- benchmark key operations before/after
- monitor memory usage patterns
- verify STM concurrency performance

## success criteria

### technical criteria
- [ ] all code compiles with GHC 9.12.2 + GHC2024
- [ ] all existing tests pass (29 mathematical + 14 feature tests)
- [ ] no performance regressions in core operations  
- [ ] memory usage within acceptable bounds

### quality criteria
- [ ] documentation updated for new setup
- [ ] development workflow remains smooth
- [ ] no new deprecation warnings
- [ ] ecosystem tools work correctly

## timeline estimate

**total effort**: 1-2 days
- phase 1 (compiler): 2-4 hours
- phase 2 (packages): 2-4 hours  
- phase 3 (features): 1-2 hours (optional)
- phase 4 (docs): 1-2 hours

**dependencies**:
- v-0.2.6 must be properly working first
- access to nix development environment
- ability to run full test suite

## deliverables

1. updated `qi-foundation.cabal` with GHC2024 and new package versions
2. updated `flake.nix` with GHC 9.12.2 and improved messaging
3. compatibility testing report
4. updated development documentation
5. performance comparison report (before/after)

## notes

this proposal focuses on **ecosystem modernization** without changing functionality. all existing mathematical guarantees and API contracts remain intact. the goal is to keep QiCore Foundation current with 2025 Haskell best practices while maintaining stability.

the actual implementation will be done incrementally with proper testing at each step, unlike the v-0.2.6 approach that made unverified claims.