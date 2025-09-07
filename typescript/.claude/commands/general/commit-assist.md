# Commit Assist

**INSTRUCTION FOR AI ASSISTANT**: Help create meaningful git commits following QiCore Foundation conventions.

## Pre-Commit Analysis

### 1. Review Changes (**filesystem**)
```bash
# Run these commands and analyze the output:
git status
git diff --staged
git diff
```

### 2. Analyze Change Impact
**Check what was modified:**
- Core mathematical operations (Result<T>, QiError)
- Infrastructure services (Config, Logger, Cache) 
- Test files and property-based tests
- Documentation and contracts
- Build configuration

### 3. Verify Quality Gates
**Before committing, ensure:**
- [ ] `/quality-check` has been run and passed
- [ ] All tests pass with 1000+ property iterations
- [ ] Mathematical laws preserved
- [ ] No breaking changes to public APIs
- [ ] Documentation updated if needed

## Commit Message Generation

### Message Format
```
type(scope): brief description

Extended description explaining:
- What was changed and why
- Any mathematical law implications
- Performance impact
- Breaking changes (if any)

Closes #issue-number (if applicable)
```

### Commit Types for QiCore
- **feat**: New feature or mathematical operation
- **fix**: Bug fix or mathematical law correction
- **refactor**: Code restructuring without behavior change
- **test**: Adding or updating tests (especially property-based)
- **docs**: Documentation updates
- **perf**: Performance improvements with contract verification
- **build**: Build system or dependency changes

### Scope Examples
- **base**: qi/base components (Result<T>, QiError)
- **core**: qi/core components (Config, Logger, Cache)
- **haskell**: Haskell implementation specific
- **typescript**: TypeScript implementation specific  
- **contracts**: Behavioral contracts
- **tests**: Testing infrastructure

## Example Commit Messages

**Feature Addition:**
```
feat(base): implement Result<T>.sequence operation

Add sequence operation for Result<T> that transforms List<Result<T, E>> 
to Result<List<T>, E>. Implementation preserves all mathematical laws:
- Functor composition and identity
- Applicative homomorphism and interchange
- Maintains O(1) complexity per element

Property-based tests verify behavior with 1000+ iterations.
```

**Bug Fix:**
```
fix(core): correct Config merge semantics for nested objects

Fix monoid merge behavior to properly handle nested configuration 
objects with right-bias precedence. This resolves inconsistent 
behavior when merging complex configuration structures.

Mathematical properties preserved:
- Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
- Right identity maintained
```

**Test Addition:**
```
test(base): add QuickCheck properties for Result<T> Applicative laws

Add comprehensive property-based tests for Applicative laws:
- Identity: pure id <*> v = v
- Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
- Homomorphism: pure f <*> pure x = pure (f x)
- Interchange: u <*> pure y = pure ($ y) <*> u

All tests run with 1000 iterations to ensure mathematical correctness.
```

## Commit Workflow

### 1. Stage Changes
```bash
git add .  # or specific files
```

### 2. Generate Commit Message
Based on the analysis above, create an appropriate commit message.

### 3. Create Commit
```bash
git commit -m "commit message"
```

### 4. Tag if Release
```bash
git tag v0.x.x  # if this is a release
```

## Special Considerations

**Mathematical Changes:**
- Always mention law preservation
- Include property test verification
- Note any performance implications

**Cross-Language Changes:**
- Mention consistency with other implementations
- Document any API changes
- Verify behavioral contracts maintained

**Breaking Changes:**
- Use BREAKING CHANGE: prefix in message body
- Document migration path
- Update version appropriately

**Generate meaningful commits that reflect QiCore Foundation's mathematical rigor and quality standards.**