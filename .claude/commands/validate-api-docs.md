# Validate API Documentation Command

Validate consistency between API documentation and implementation using sequential thinking analysis.

## Usage

- `/validate-api-docs` - Validate all API docs against implementations
- `/validate-api-docs cache` - Validate cache API docs only
- `/validate-api-docs config` - Validate config API docs only
- `/validate-api-docs logger` - Validate logger API docs only
- `/validate-api-docs error` - Validate error API docs only
- `/validate-api-docs result` - Validate result API docs only

## Instructions

You are tasked with validating API documentation consistency with actual TypeScript implementations. Use the sequential thinking MCP server to perform systematic analysis.

### Step 1: Use Sequential Thinking for Analysis

Use the `mcp__sequential-thinking__sequentialthinking` tool to analyze:

1. **Parse target modules**: Determine which modules to validate (all or specific module from user input)
2. **Extract API signatures**: Parse documented interfaces, types, and function signatures from API docs
3. **Extract implementation signatures**: Parse actual interfaces, types, and exports from implementation files
4. **Compare systematically**: Identify mismatches, missing documentation, undocumented features
5. **Generate validation report**: Provide actionable findings with specific line references

### Step 2: Module Analysis Framework

For each target module, perform the following systematic analysis:

#### API Documentation Analysis
- Read API documentation from `typescript/docs/api/core/{module}.md` or `typescript/docs/api/base/{module}.md`
- Extract documented interfaces, types, classes, and functions
- Parse documented method signatures and parameters
- Note documented usage examples and patterns

#### Implementation Analysis
- Read implementation from `typescript/lib/src/core/{module}.ts` or `typescript/lib/src/base/{module}.ts`
- Extract exported interfaces, types, classes, and functions
- Parse actual method signatures and parameters
- Identify internal vs public APIs

#### Systematic Comparison
- **Implementation-First Approach**: Start with actual exported functions/types from implementation
- **Documentation Matching**: Check if each implementation feature is documented correctly
- **Signature Verification**: Compare parameter order, types, return types
- **Documentation-Only Features**: Identify features documented but not implemented (for deletion)
- **Missing Documentation**: Find implementation features without documentation

### Step 3: Validation Categories

#### Critical Issues (❌)
- Documented interfaces that don't exist in implementation
- Incorrect method signatures (parameters, return types)
- Wrong async/sync patterns in examples
- Documented types that don't match implementation

#### Missing Documentation (⚠️)
- Exported functions/classes not documented
- Implementation methods missing from documented interfaces
- Undocumented parameters or return types
- Missing usage examples for complex features

#### Minor Inconsistencies (📝)
- Parameter naming differences
- Optional parameter documentation inconsistencies
- Example code style inconsistencies
- Missing JSDoc in implementation

### Step 4: Sequential Thinking Process

Use the sequential thinking MCP server with this structure:

1. **Initial Analysis**: Parse module scope and identify files to analyze
2. **Implementation Extraction**: Extract all actual APIs, interfaces, and exports (PRIMARY SOURCE)
3. **Documentation Extraction**: Extract documented APIs, interfaces, and examples  
4. **Implementation-Based Comparison**: For each implementation feature, check documentation accuracy
5. **Documentation-Only Detection**: Find features documented but not implemented
6. **Concise Table Report**: Generate table-based report with clear actions needed

### Step 5: Report Generation

Generate a detailed validation report and save it to the appropriate reports directory:

#### Report Output Location
- **File Pattern**: `typescript/docs/api/reports/{module}.validation.md`
- **Base modules**: `typescript/docs/api/reports/result.validation.md`, `typescript/docs/api/reports/error.validation.md`
- **Core modules**: `typescript/docs/api/reports/cache.validation.md`, `typescript/docs/api/reports/config.validation.md`, `typescript/docs/api/reports/logger.validation.md`

#### Report Format
```markdown
# {Module} API Documentation Validation Report

*Generated: {timestamp}*  
*Module: {module}*

## Summary
- **Critical Issues**: X ❌
- **Missing Documentation**: Y ⚠️  
- **Minor Inconsistencies**: Z 📝
- **Overall Score**: A/B (C%)

## API Implementation vs Documentation

| Feature | Implementation | API Documentation | Status |
|---------|---------------|-------------------|---------|
| `functionName()` | ✅ `path/file.ts:123` | ✅ `docs/api.md:45` | ✅ OK |
| `anotherFunction()` | ✅ `path/file.ts:150` | ❌ `docs/api.md:67` wrong signature | ❌ Signature mismatch |
| `thirdFunction()` | ✅ `path/file.ts:180` | ❌ Not documented | ⚠️ Missing docs |
| `TypeName` | ✅ `TypeName<T>` (line 22) | ❌ `TypeName<T, E>` (line 12) | ❌ Type mismatch |

## Documentation-Only Features (Consider Removing)

| Feature | API Documentation | Implementation | Action |
|---------|------------------|----------------|---------|
| `nonExistentFunction()` | ✅ `docs/api.md:100` | ❌ Not implemented | 🗑️ Delete from docs |
| `anotherMissingFn()` | ✅ `docs/api.md:150` | ❌ Not implemented | 🗑️ Delete from docs |

## Key Issues

### Critical Problems
- Function signature mismatches
- Type definition errors

### Missing Documentation
- Undocumented implementation features

### Documentation Cleanup Needed
- X features documented but not implemented

## Recommendation
[Brief summary of main actions needed]
```

#### Report File Creation Process
1. **Create reports directory** if it doesn't exist: `typescript/docs/api/reports/`
2. **Generate validation report** using sequential thinking analysis
3. **Write report file** with standardized naming: `{module}.validation.md`
4. **Confirm file creation** and provide file path to user

### Step 6: Module-Specific Validation

#### Cache Module
- Focus on ICache interface completeness
- Verify dual backend documentation (MemoryCache, RedisCache)
- Check Redis-specific method documentation (ttl, expire)
- Validate batch operation signatures (mget, mset, mdelete)
- Verify event system documentation

#### Config Module
- Validate ConfigBuilder pattern documentation
- Check ValidatedConfig API documentation
- Verify schema validation method signatures
- Validate multi-format support documentation

#### Logger Module
- Check Logger class method signatures
- Verify event system documentation
- Validate log level type definitions
- Check Pino integration documentation

#### Error Module (Base)
- Validate QiError interface documentation
- Check error factory function signatures
- Verify ErrorCategory enum documentation
- Validate error chaining method documentation

#### Result Module (Base)
- Validate Result<T> interface documentation
- Check monadic operation signatures (map, flatMap, etc.)
- Verify factory method documentation (success, failure)
- Validate mathematical law documentation

### Step 7: Implementation Details

#### File Path Patterns
- API Docs: `typescript/docs/api/{base|core}/{module}.md`
- Implementation: `typescript/lib/src/{base|core}/{module}.ts`
- Use module parameter to determine which files to analyze

#### Parsing Strategy
- Extract TypeScript interfaces using regex patterns
- Parse export statements to identify public API
- Compare method signatures character by character
- Validate async/Promise patterns in examples

#### Error Handling
- If files are missing, note in report
- Continue validation with available files
- Mark missing files as critical issues
- Provide specific file paths for all issues

### Step 8: Automation Potential

This validation process could be automated by:
- Parsing TypeScript AST for precise interface extraction
- Using TypeScript compiler API for signature validation
- Implementing as a Git pre-commit hook
- Integrating with CI/CD pipeline for continuous validation

### Expected Output

The command should produce a comprehensive validation report highlighting:
- All inconsistencies between documentation and implementation
- Specific file locations and line numbers for issues
- Actionable fixes with clear priorities
- Overall consistency score for the module

This ensures API documentation remains accurate and helpful for developers using the QiCore Foundation library.

## Usage Examples

```bash
# Validate all API documentation
/validate-api-docs

# Validate specific module
/validate-api-docs cache

# Expected output format
API Documentation Validation Report
Module: cache
Critical Issues: 2 ❌
Missing Documentation: 3 ⚠️
Minor Inconsistencies: 1 📝
Overall Score: 85/100 (85%)
```

Remember to use the sequential thinking MCP server for complex analysis and provide detailed, actionable validation reports.