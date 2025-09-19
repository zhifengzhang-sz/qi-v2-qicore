# ESLint Anti-Pattern Detection Continuation Guide

## 🎯 Mission: Apply ESLint Rules to Remaining QiCore Modules

### 📋 Prerequisites & Knowledge Required

#### **Essential Context**
1. **Project Structure**: QiCore workspace with `lib/base`, `lib/core`, `lib/amsg`, `lib/cli`
2. **ESLint Plugin**: Custom `@qi/eslint-plugin` detecting Result<T> anti-patterns
3. **Current Status**: ESLint rules cleaned up, modernized, and ready for application
4. **Known Issue**: ESLint v9 + TypeScript-ESLint v8 dependency conflicts affect runtime

#### **Anti-Pattern Knowledge**
**What the rules detect**:
- ✅ **Direct tag checking**: `result.tag === 'success'`
- ✅ **Direct property access**: `result.value`, `result.error`
- ✅ **Destructuring**: `const { tag, value } = result`

**What should be used instead**:
- `match(onSuccess, onError, result)` - Exhaustive handling
- `map(fn, result)` - Transform success values
- `flatMap(fn, result)` - Chain operations
- `isSuccess(result)`, `isFailure(result)` - Type guards

#### **Baseline Detection Results**
From corrected analysis, **100+ violations** found across:
- `lib/core/src/cache.ts`: ~12 violations
- `lib/core/src/config.ts`: ~9 violations
- `lib/cli/src/factories/createReadlineCLI.ts`: ~12 violations
- `lib/base/src/*`: 0 violations (correctly excluded - legitimate implementations)

### 🚀 Step-by-Step Process

#### **Step 1: Environment Setup**
```bash
# Verify current directory
pwd  # Should be: /home/zzhang/dev/qi/github/qi-v2-qicore/typescript

# Verify ESLint plugin workspace setup
bun pm ls | grep eslint-plugin  # Should show: @qi/eslint-plugin@workspace:eslint-rules

# Check available commands
grep "lint" package.json  # Should show: "lint:anti-patterns": "eslint ."
```

#### **Step 2: Module Analysis Priority**

**High Priority** (Application Logic):
1. **`lib/core`**: Config, Logger, Cache - core services with business logic
2. **`lib/cli`**: CLI framework - user-facing functionality
3. **`lib/amsg`**: Message queue - service infrastructure

**Excluded** (Correctly):
4. **`lib/base`**: Result<T> implementation internals (automatically excluded by rule)

#### **Step 3: Analysis Commands**

**Manual Analysis** (bypasses dependency issues):
```bash
# Check violations in a specific module
grep -n "\.tag\s*===\s*['\"]success['\"]" lib/core/src/*.ts
grep -n "\.tag\s*===\s*['\"]failure['\"]" lib/core/src/*.ts
grep -n "result\.value\|result\.error" lib/core/src/*.ts

# Count violations per file
find lib/core/src -name "*.ts" -exec grep -l "\.tag\s*===" {} \; | wc -l
```

**ESLint Analysis** (when dependency issues are resolved):
```bash
# Test specific module
bun run lint:anti-patterns lib/core/src/

# Test specific file
bun run lint:anti-patterns lib/core/src/cache.ts

# Get detailed output
bun run lint:anti-patterns lib/core/src/ --format=codeframe
```

#### **Step 4: Per-Module Analysis Template**

For each module, document:

1. **Violation Count & Distribution**
   ```
   Module: lib/[module]
   Total Files: X
   Files with Violations: Y
   Total Violations: Z

   Top Violating Files:
   - file1.ts: N violations
   - file2.ts: M violations
   ```

2. **Violation Categories**
   ```
   - Direct tag checking: N instances
   - Direct property access: M instances
   - Destructuring: K instances
   ```

3. **Business Impact Assessment**
   ```
   - Critical violations (user-facing logic): X
   - Medium violations (internal services): Y
   - Low violations (utilities/helpers): Z
   ```

4. **Refactoring Recommendations**
   ```
   Priority 1: [file.ts:line] - [reason]
   Priority 2: [file.ts:line] - [reason]
   ```

### 📁 Module-Specific Guidelines

#### **lib/core** - Configuration, Logging, Caching
**Expected Patterns**: Service initialization, config validation, cache operations
**Common Violations**:
- Config validation logic with direct tag checking
- Cache hit/miss logic with direct property access
- Service startup error handling

**Key Files to Analyze**:
- `config.ts` (~10 violations expected)
- `cache.ts` (~10 violations expected)
- `logger.ts` (unknown count)

#### **lib/cli** - CLI Framework
**Expected Patterns**: User input handling, command routing, UI rendering
**Common Violations**:
- Command handler result processing
- Factory pattern implementations with manual Result unwrapping
- UI state management

**Key Files to Analyze**:
- `factories/createReadlineCLI.ts` (~12 violations expected)
- `container/CLIContainer.ts` (unknown count)
- Service initialization files

#### **lib/amsg** - Async Message Queue
**Expected Patterns**: Message processing, queue operations, async coordination
**Common Violations**:
- Message validation and routing logic
- Queue operation error handling
- Factory pattern implementations

### 🛠️ Commands Reference

**Analysis Commands**:
```bash
# Count all violations across lib/
find lib/ -name "*.ts" -exec grep -l "\.tag\s*===" {} \; | wc -l

# Get violation breakdown by module
for module in base core cli amsg; do
  echo "=== lib/$module ==="
  find lib/$module/src -name "*.ts" -exec grep -l "\.tag\s*===" {} \; | wc -l
done

# Detailed analysis of a file
grep -n -C2 "\.tag\s*===" lib/core/src/cache.ts
```

**ESLint Commands** (when working):
```bash
# Apply to all modules
bun run lint:anti-patterns lib/

# Apply to specific module
bun run lint:anti-patterns lib/core/src/

# Get only error count
bun run lint:anti-patterns lib/core/src/ --format=compact | grep -c "error"
```

### 📊 Expected Output Format

For each module, provide a report like:

```markdown
## lib/[module] Anti-Pattern Analysis

### Summary
- **Total Files**: X TypeScript files
- **Files with Violations**: Y files
- **Total Violations**: Z instances
- **Violation Types**: A tag checks, B property access, C destructuring

### Top Violating Files
1. `[file.ts]`: N violations
   - Lines: X, Y, Z
   - Context: [description]
2. `[file2.ts]`: M violations
   - Lines: A, B, C
   - Context: [description]

### Business Impact
- **Critical**: [count] violations in user-facing logic
- **Medium**: [count] violations in internal services
- **Low**: [count] violations in utilities

### Refactoring Priority
1. **High**: [file:line] - [business reason]
2. **Medium**: [file:line] - [technical reason]
3. **Low**: [file:line] - [maintenance reason]
```

### 🚨 Important Notes

1. **Don't Fix Yet**: This is analysis only, no code changes
2. **Focus on Production Code**: Test files may have acceptable violations
3. **Context Matters**: Some violations in base library internals may be acceptable
4. **Document Everything**: Detailed reports are crucial for prioritization
5. **Use Manual Analysis**: Dependency issues prevent ESLint execution but don't affect analysis

### ✅ Success Criteria

Each module analysis should provide:
- ✅ Complete violation count and distribution
- ✅ Business impact assessment
- ✅ Prioritized refactoring recommendations
- ✅ Context for each violation category
- ✅ Clear next steps for remediation

This analysis will guide the remediation phase where violations are systematically converted to proper functional patterns.