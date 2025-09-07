# Generate Reports Command

Generate implementation and contract compliance reports for QiCore Foundation modules.

## Usage

- `/generate-reports` - Generate all reports
- `/generate-reports cache` - Generate reports for cache module only
- `/generate-reports config` - Generate reports for config module only
- `/generate-reports logger` - Generate reports for logger module only
- `/generate-reports error` - Generate reports for error module only
- `/generate-reports result` - Generate reports for result module only
- `/generate-reports --clean` - Clean up typescript/docs/impl directory first, then generate all reports

## Instructions

You are tasked with generating comprehensive implementation and contract compliance reports for QiCore Foundation modules. Follow these steps:

### Step 1: Use Sequential Thinking for Analysis

Use the `mcp__sequential-thinking__sequentialthinking` tool to analyze the current state:

1. **Identify target modules**: Determine which modules to analyze (all modules: cache, config, logger, error, result, or specific module from user input)
2. **Analyze project structure**: Examine the current state of contracts, guides, and implementations
3. **Compare documentation**: Find gaps between contracts, guides, and actual implementations
4. **Generate recommendations**: Provide actionable steps for improvement

### Step 2: Module Analysis

For each target module, perform the following analysis:

#### Contract Analysis
- Read the relevant contract file from `docs/contracts/`
- Extract required features, methods, and specifications
- Note any version-specific requirements

#### Guide Analysis  
- Read the guide file from `typescript/docs/impl/guides/{module}.md`
- Extract documented features and implementation details
- Note any TypeScript-specific adaptations

#### Implementation Analysis
- Read the implementation file from `typescript/lib/src/{base|core}/{module}.ts`
- Extract exported functions, classes, and methods
- Identify implemented features and their signatures

### Step 3: Gap Analysis

Compare the three sources and identify:
- **Missing implementations**: Features required by contract but not implemented
- **Missing documentation**: Implemented features not documented in guides
- **Extra implementations**: Features implemented but not in contracts
- **Inconsistencies**: Differences between contract, guide, and implementation

### Step 4: Generate Reports

Generate two types of reports for each module:

#### Implementation Report (`reports/{module}.md`)
```markdown
# {Module} Implementation Report

## Complete Feature Analysis

| Feature | Contract | Guide | Implementation | Status | Notes |
|---------|----------|--------|----------------|---------|-------|
| `feature1` | ✅ Required | ✅ Documented | ✅ Implemented | ✅ | Perfect alignment |
| `feature2` | ❌ Not in contract | ✅ Documented | ✅ Implemented | ⚠️ | TypeScript extension |

## Summary
- **Contract Compliance**: X/Y required features implemented (Z%)
- **Guide Accuracy**: X/Y documented features implemented (Z%)
- **Implementation Coverage**: X/Y features documented (Z%)
- **Missing Documentation**: List of undocumented features

## Recommendations
1. Implement missing contract features: ...
2. Document implemented features: ...
3. Consider standardizing extensions: ...
```

#### Contract Compliance Report (`reports/{module}.contract.md`)
```markdown
# {Module} Contract Compliance Report

## Contract vs guides/{module}.md

| Contract Operation | Guide Documentation | Status |
|-------------------|-------------------|---------|
| `operation1` | ✅ Documented | ✅ |
| `operation2` | ❌ Not documented | ❌ |

## TypeScript Adaptations Documented
- Document any TypeScript-specific adaptations and their rationale

## Extensions Beyond Contract
- List features implemented beyond contract requirements

## Compliance Score
- Contract Coverage: X/Y (Z%)
- Implementation Alignment: X/Y (Z%)
- Documentation Quality: X/Y (Z%)

## Action Items
- [ ] Document missing contract features
- [ ] Implement missing requirements
- [ ] Add documentation for extensions
```

### Step 5: File Management

#### Report Generation
- Save implementation reports as `typescript/docs/impl/reports/{module}.md`
- Save contract compliance reports as `typescript/docs/impl/reports/{module}.contract.md`
- Use the Write tool to create/update these files

#### Cleanup (if --clean flag is used)
- List all files in `typescript/docs/impl/`
- Identify outdated or redundant files
- Remove or archive old reports as needed
- Clean up the directory structure

### Step 6: Summary and Recommendations

After generating all reports, provide:
1. **Overall project health**: Summary of compliance across all modules
2. **Priority actions**: Most important items to address
3. **Next steps**: Recommended workflow for maintaining reports

## Module-Specific Guidance

### Cache Module
- Focus on core operations: get, set, has, remove, clear, size
- Check TTL and eviction policies
- Verify factory methods: createMemory, createPersistent
- Document Redis-specific features

### Config Module
- Analyze configuration loading and merging
- Check validation and schema support
- Verify multiple format support (JSON, YAML, TOML)
- Document environment variable integration

### Logger Module
- Check log levels and formatting
- Verify Pino integration
- Document structured logging features
- Analyze event system integration

### Error Module
- Focus on error creation and chaining
- Check category-based error handling
- Verify retry strategy integration
- Document error formatting utilities

### Result Module
- Analyze monadic operations (map, flatMap, match)
- Check functor and monad law compliance
- Verify factory methods (success, failure)
- Document composition patterns

## Error Handling

If any files are missing or inaccessible:
- Note the missing files in the report
- Provide recommendations for creating missing documentation
- Continue with available information
- Mark gaps clearly in the analysis

## Output Format

- Generate reports in markdown format
- Use consistent table formatting
- Include clear status indicators (✅ ❌ ⚠️)
- Provide actionable recommendations
- Include generation timestamp

Remember to use the sequential thinking MCP server for complex analysis and decision-making throughout this process.