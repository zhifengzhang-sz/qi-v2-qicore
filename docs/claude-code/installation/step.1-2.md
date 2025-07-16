```bash
#!/bin/bash

# ============================================
# STEP 1: Install MCP Servers (Use Existing Ones)
# ============================================

# File system operations
claude mcp add filesystem -s user -- npx -y @modelcontextprotocol/server-filesystem ~/dev/qi/github

# Git operations
claude mcp add git -s user -- npx -y @modelcontextprotocol/server-git

# GitHub integration
claude mcp add github -s user -- env GITHUB_TOKEN=your_token npx -y @modelcontextprotocol/server-github

# Sequential thinking (for complex workflows)
claude mcp add sequential-thinking -s user -- npx -y @modelcontextprotocol/server-sequential-thinking

# Memory (for context across sessions)
claude mcp add memory -s user -- npx -y @modelcontextprotocol/server-memory

# Verify they're all connected
claude mcp list

# ============================================
# STEP 2: Build Custom Commands in Claude Code CLI
# ============================================

# Create .claude directory for custom commands
mkdir -p ~/.claude/commands

# ============================================
# Custom Command 1: Design Documentation
# ============================================

cat > ~/.claude/commands/design-docs.md << 'EOF'
# Design Documentation Generator

You are a software architect. Analyze the current project and create comprehensive design documentation.

## Steps:
1. Read the project structure using the filesystem MCP
2. Analyze the codebase to understand architecture
3. Create the following documents in ./docs/design/:
   - architecture.md (overall system design)
   - api.md (API interfaces and endpoints)
   - data-models.md (data structures and types)

## Output Format:
- Use clear markdown formatting
- Include diagrams where helpful
- Focus on TypeScript/JavaScript patterns
- Document all public interfaces

## Success Criteria:
- All documents are created in ./docs/design/
- Architecture is clearly explained
- API contracts are documented
- Data models are defined
EOF

# ============================================
# Custom Command 2: Code Generation
# ============================================

cat > ~/.claude/commands/generate-code.md << 'EOF'
# Code Generation from Design

You are a TypeScript developer. Generate clean, type-safe code based on the design documentation.

## Steps:
1. Read design documents from ./docs/design/
2. Generate TypeScript code with proper:
   - Interface definitions
   - Class structures
   - Method implementations
   - Error handling
   - Type safety

## Code Standards:
- Use TypeScript strict mode
- Follow functional programming patterns where appropriate
- Include JSDoc comments
- Use proper error handling
- Follow existing project patterns

## Success Criteria:
- All interfaces are implemented
- Code is type-safe
- Follows design specifications
- Includes proper error handling
EOF

# ============================================
# Custom Command 3: Fix Stubs
# ============================================

cat > ~/.claude/commands/fix-stubs.md << 'EOF'
# Fix Stubs and Placeholders

You are a code quality expert. Find and fix all placeholder/stub code in the project.

## Steps:
1. Search for common stub patterns:
   - TODO comments
   - "throw new Error('Not implemented')"
   - "return null" or "return undefined"
   - Placeholder comments
   - Mock implementations

2. For each stub found:
   - Implement actual logic
   - Remove placeholder comments
   - Add proper error handling
   - Ensure implementation matches design

## Search Patterns:
- TODO
- FIXME  
- Not implemented
- Placeholder
- Mock data that should be real

## Success Criteria:
- No TODO/FIXME comments remain
- All methods have real implementations
- Proper error handling is added
- Code matches design specifications
EOF

# ============================================
# Custom Command 4: Type Check & Fix
# ============================================

cat > ~/.claude/commands/typecheck-fix.md << 'EOF'
# TypeScript Type Checking and Fixes

You are a TypeScript expert. Ensure all code passes strict type checking.

## Steps:
1. Run TypeScript compiler: `tsc --noEmit`
2. Analyze any type errors
3. Fix errors by:
   - Adding proper type annotations
   - Fixing type mismatches
   - Adding missing imports
   - Updating interface definitions

## Type Safety Rules:
- Use strict mode
- No any types (unless absolutely necessary)
- Proper null/undefined handling
- Correct generic usage
- Interface segregation

## Success Criteria:
- `tsc --noEmit` passes without errors
- All functions have proper type annotations
- No implicit any types
- Proper error handling for nulls/undefined
EOF

# ============================================
# Custom Command 5: Biome Check & Fix
# ============================================

cat > ~/.claude/commands/biome-fix.md << 'EOF'
# Biome Linting and Formatting

You are a code quality expert. Ensure code follows consistent style and quality rules.

## Steps:
1. Run Biome check: `npx @biomejs/biome check .`
2. Fix any linting errors:
   - Remove unused imports
   - Fix variable naming
   - Resolve complexity issues
   - Fix accessibility issues

3. Apply formatting: `npx @biomejs/biome format --write .`

## Quality Standards:
- Consistent code formatting
- No unused variables/imports
- Proper naming conventions
- Low complexity functions
- No console.log statements (except debugging)

## Success Criteria:
- Biome check passes without errors
- Code is consistently formatted
- No linting warnings
- Follows project style guide
EOF

# ============================================
# Custom Command 6: Generate Tests
# ============================================

cat > ~/.claude/commands/generate-tests.md << 'EOF'
# Unit Test Generation

You are a testing expert. Create comprehensive unit tests for all implemented code.

## Steps:
1. Analyze all source files in ./src/
2. For each class/function, create tests covering:
   - Happy path scenarios
   - Edge cases
   - Error conditions
   - Boundary conditions

## Testing Standards:
- Use Jest or Vitest
- Follow AAA pattern (Arrange, Act, Assert)
- Mock external dependencies
- Descriptive test names
- Group related tests

## Test Coverage Goals:
- >90% code coverage
- All public methods tested
- Error paths covered
- Edge cases included

## Success Criteria:
- Tests are in ./tests/ directory
- All tests pass
- High code coverage achieved
- Tests are maintainable
EOF

# ============================================
# Custom Command 7: Complete Workflow
# ============================================

cat > ~/.claude/commands/dev-workflow.md << 'EOF'
# Complete Software Development Workflow

Execute the complete development workflow for a TypeScript project.

## Workflow Steps:
1. **Design Documentation**: Create architectural and API documentation
2. **Code Generation**: Generate TypeScript code from design
3. **Consistency Check**: Ensure code matches design
4. **Fix Stubs**: Replace placeholders with real implementations
5. **Type Check**: Ensure TypeScript strict mode compliance
6. **Biome Check**: Apply linting and formatting
7. **Generate Tests**: Create comprehensive unit tests

## Execution:
Run each step sequentially, ensuring each completes successfully before proceeding to the next.

## Quality Gates:
- Each step must pass before proceeding
- Report any failures immediately
- Provide summary of what was accomplished

## Success Criteria:
- All 7 steps complete successfully
- Documentation is up to date
- Code is type-safe and well-formatted
- Tests provide good coverage
- No stubs or TODOs remain
EOF

# ============================================
# Custom Command 8: Quick Quality Check
# ============================================

cat > ~/.claude/commands/quick-check.md << 'EOF'
# Quick Quality Check

Perform a fast quality check on the current project.

## Quick Checks:
1. **TypeScript**: Run `tsc --noEmit`
2. **Linting**: Run `npx @biomejs/biome check .`
3. **Tests**: Run `npm test`
4. **Stubs**: Search for TODO/FIXME/Not implemented

## Report Format:
- ✅ PASS or ❌ FAIL for each check
- Brief description of any issues found
- Suggested fixes for failures

## Success Criteria:
- All checks pass
- No critical issues found
- Code is ready for commit
EOF

echo "✅ Custom commands created in ~/.claude/commands/"
echo ""
echo "Available commands:"
echo "• /design-docs - Generate design documentation"
echo "• /generate-code - Generate code from design"
echo "• /fix-stubs - Fix placeholder code"
echo "• /typecheck-fix - Fix TypeScript errors"
echo "• /biome-fix - Fix linting and formatting"
echo "• /generate-tests - Create unit tests"
echo "• /dev-workflow - Run complete workflow"
echo "• /quick-check - Quick quality check"
```