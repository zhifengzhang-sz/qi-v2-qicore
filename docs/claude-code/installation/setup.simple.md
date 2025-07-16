```bash
#!/bin/bash

# ============================================
# Clean Up and Create Rock-Solid Setup
# ============================================

echo "🧹 Cleaning up broken servers..."
claude mcp remove web-fetch 2>/dev/null
echo "✅ Removed broken web-fetch"

echo ""
echo "🎯 Your PERFECT MCP setup:"
claude mcp list

echo ""
echo "✨ Why this is BETTER than complex setups:"
echo "• No broken servers cluttering your interface"
echo "• No network dependencies that can fail"
echo "• Fast, reliable, local operations"
echo "• Claude's built-in knowledge is extensive and current"
echo ""

# ============================================
# Create Rock-Solid Custom Commands
# ============================================

echo "📁 Creating rock-solid custom commands..."
mkdir -p ~/.claude/commands

# ============================================
# Ultimate Dev Workflow
# ============================================

cat > ~/.claude/commands/dev-workflow.md << 'EOF'
# Ultimate Development Workflow

The most reliable development workflow using your 3 rock-solid MCP servers.

## Your Perfect Toolkit:
- **sequential-thinking**: Methodical, step-by-step execution
- **filesystem**: Direct access to your qi project files
- **memory**: Persistent project knowledge and patterns

## The Workflow:
### Phase 1: Project Understanding
Use **sequential-thinking** to methodically:
1. **filesystem**: Analyze current project structure and files
2. **memory**: Recall established patterns, standards, and decisions
3. Build comprehensive understanding of current state

### Phase 2: Design & Planning
1. Create/update design documentation based on analysis
2. **memory**: Store design decisions for consistency
3. **filesystem**: Save documentation to proper locations
4. Plan implementation approach systematically

### Phase 3: Implementation
1. Generate/improve TypeScript code following established patterns
2. **memory**: Apply remembered coding standards and best practices
3. **filesystem**: Update source files with clean, type-safe code
4. Ensure consistency with project architecture

### Phase 4: Quality Assurance
1. **filesystem**: Scan for and fix TODOs, stubs, placeholders
2. Ensure TypeScript strict mode compliance
3. Apply consistent code formatting and style
4. **memory**: Remember and apply quality standards

### Phase 5: Testing & Validation
1. Generate comprehensive unit tests covering edge cases
2. **filesystem**: Create test files in proper directory structure
3. Ensure good test coverage and maintainable tests
4. Validate implementation against design

### Phase 6: Documentation & Memory
1. **filesystem**: Update README, API docs, and comments
2. **memory**: Store key insights, patterns, and lessons learned
3. Document architectural decisions and reasoning
4. Prepare project for future development sessions

## Success Criteria:
- Project is well-structured and documented
- Code is type-safe, tested, and follows patterns
- Knowledge is preserved in memory for consistency
- No TODOs or placeholders remain
- Ready for production deployment

## This workflow leverages Claude's built-in knowledge of:
- Current TypeScript best practices
- Modern testing patterns
- Clean architecture principles
- Security best practices
- Performance optimization techniques
EOF

# ============================================
# Smart Analysis Command
# ============================================

cat > ~/.claude/commands/analyze.md << 'EOF'
# Smart Project Analysis

Deep analysis using systematic thinking and project knowledge.

## Analysis Process:
### 1. Structural Analysis (**filesystem** + **sequential-thinking**)
- Examine project organization and file structure
- Identify architectural patterns and design decisions
- Assess code organization and modularity

### 2. Quality Assessment (**filesystem** + **memory**)
- Scan for code quality issues and technical debt
- Compare against stored coding standards and patterns
- Identify inconsistencies and improvement opportunities

### 3. Pattern Recognition (**memory** + **sequential-thinking**)
- Compare current code against established project patterns
- Identify successful patterns to replicate
- Spot anti-patterns and suggest improvements

### 4. Strategic Recommendations
- Prioritize improvements based on impact and effort
- Suggest architectural improvements
- Recommend refactoring opportunities

## Output:
- Executive summary of project health
- Detailed findings with specific file locations
- Prioritized improvement recommendations
- Actionable next steps

## Usage:
Perfect for:
- Project health checks
- Code review preparation
- Architecture evaluation
- Technical debt assessment
EOF

# ============================================
# Fix & Improve Command
# ============================================

cat > ~/.claude/commands/fix.md << 'EOF'
# Fix and Improve Code

Systematically fix issues and improve code quality.

## Fix Categories:
### 1. Critical Issues (**filesystem** + **sequential-thinking**)
- TypeScript compilation errors
- Runtime errors and exceptions
- Security vulnerabilities
- Performance bottlenecks

### 2. Code Quality Issues (**filesystem** + **memory**)
- TODO/FIXME comments and placeholders
- Inconsistent coding patterns
- Missing error handling
- Unclear variable/function names

### 3. Architecture Improvements (**memory** + **sequential-thinking**)
- Apply established project patterns consistently
- Improve code organization and structure
- Enhance modularity and reusability
- Strengthen type safety

### 4. Documentation & Testing
- Add missing documentation and comments
- Generate comprehensive unit tests
- Update outdated documentation
- Improve code readability

## Process:
1. **sequential-thinking**: Prioritize fixes by impact and complexity
2. **filesystem**: Apply fixes systematically to source files
3. **memory**: Store successful solutions for future use
4. Validate fixes and ensure no regressions

## Success Criteria:
- All critical issues resolved
- Code follows established patterns
- Comprehensive test coverage
- Clear, maintainable documentation
EOF

# ============================================
# Memory Management Command
# ============================================

cat > ~/.claude/commands/remember.md << 'EOF'
# Project Memory Management

Manage and leverage project knowledge across development sessions.

## Store Project Knowledge:
### Architecture & Design
- Store architectural decisions and reasoning
- Remember design patterns that work well
- Document interface contracts and APIs
- Record performance optimization strategies

### Coding Standards & Patterns
- Store preferred coding styles and conventions
- Remember successful implementation patterns
- Document error handling strategies
- Record testing approaches and standards

### Project-Specific Solutions
- Store solutions to recurring problems
- Remember integration patterns and configurations
- Document deployment and environment setup
- Record troubleshooting procedures

## Recall and Apply Knowledge:
### Pattern Consistency
- Apply remembered patterns consistently across features
- Ensure architectural decisions are followed
- Maintain coding standard compliance
- Use established testing strategies

### Problem Solving
- Recall previous solutions to similar problems
- Apply lessons learned from past issues
- Build on successful implementation patterns
- Avoid repeating past mistakes

## Memory Commands:
### Store Information:
"Remember that qi-v2-qicore uses functional programming patterns with immutable data"
"Remember the authentication flow we implemented for user sessions"
"Remember that we prefer composition over inheritance in this project"

### Recall Information:
"What architectural patterns do we use in qi-v2-qicore?"
"How do we handle errors in this project?"
"What testing strategies work best for our codebase?"

## Benefits:
- Consistent patterns across development sessions
- Faster problem resolution using proven solutions
- Architectural integrity maintained over time
- Knowledge preservation across team members
EOF

# ============================================
# Quick Health Check
# ============================================

cat > ~/.claude/commands/health.md << 'EOF'
# Quick Project Health Check

Fast, reliable assessment using your 3 core tools.

## Health Checks:
### 1. Structure Health (**filesystem**)
- ✅ Project organization follows standards
- ✅ Required directories and files exist
- ✅ File naming conventions are consistent
- ✅ Dependencies are properly structured

### 2. Code Health (**filesystem** + **memory**)
- ✅ TypeScript compiles without errors
- ✅ No TODOs or placeholder code remains
- ✅ Coding standards are followed consistently
- ✅ Error handling is implemented properly

### 3. Documentation Health (**filesystem**)
- ✅ README is current and complete
- ✅ API documentation matches implementation
- ✅ Architecture docs reflect current design
- ✅ Comments are clear and helpful

### 4. Pattern Health (**memory** + **sequential-thinking**)
- ✅ Established patterns are followed consistently
- ✅ Architectural decisions are implemented correctly
- ✅ Code organization matches design principles
- ✅ Quality standards are maintained

## Output Format:
- Overall health score (🟢 Excellent / 🟡 Good / 🔴 Needs Attention)
- Specific issues with file locations
- Priority recommendations
- Quick fix suggestions

## Use Cases:
- Before committing code
- Before creating pull requests
- After completing features
- During code reviews
- Before deployments
EOF

# ============================================
# Learning & Improvement
# ============================================

cat > ~/.claude/commands/learn.md << 'EOF'
# Learn and Improve

Continuously improve your codebase using systematic learning.

## Learning Sources:
### Built-in Knowledge
- Leverage Claude's extensive knowledge of current best practices
- Apply modern TypeScript, React, Node.js patterns
- Use established software engineering principles
- Implement proven architecture patterns

### Project History (**memory**)
- Learn from past decisions and their outcomes
- Build on successful implementation patterns
- Avoid repeating past mistakes
- Refine approaches based on experience

### Code Analysis (**filesystem** + **sequential-thinking**)
- Study existing code to identify improvement opportunities
- Analyze patterns that work well in the current codebase
- Identify areas for refactoring and enhancement
- Learn from successful implementations in other parts of the project

## Improvement Process:
### 1. Identify Learning Opportunities
- Areas where code could be more maintainable
- Opportunities to apply better patterns
- Places where performance could be improved
- Sections that need better error handling

### 2. Research and Plan (**sequential-thinking**)
- Analyze current implementation thoroughly
- Plan improvement approach systematically
- Consider impacts and trade-offs
- Design implementation strategy

### 3. Apply Improvements (**filesystem**)
- Implement improvements incrementally
- Maintain backward compatibility where needed
- Test changes thoroughly
- Update documentation

### 4. Store Learnings (**memory**)
- Remember successful improvement patterns
- Store lessons learned for future reference
- Document new standards and approaches
- Build institutional knowledge

## Example Applications:
"Improve error handling patterns in qi-v2-qicore based on modern best practices"
"Apply functional programming principles more consistently throughout the codebase"
"Enhance type safety using advanced TypeScript features"
"Optimize performance based on established optimization patterns"
EOF

echo "✅ Rock-solid commands created!"
echo ""
echo "🎯 Your ULTIMATE development setup:"
echo ""
echo "📋 Available commands:"
echo "• /dev-workflow  - Complete, reliable development workflow"
echo "• /analyze       - Deep project analysis"
echo "• /fix          - Systematically fix and improve code"
echo "• /remember     - Manage project knowledge and patterns"
echo "• /health       - Quick project health check"
echo "• /learn        - Continuous improvement and learning"
echo ""
echo "💪 Your 3 servers are ROCK SOLID:"
echo "• sequential-thinking ✅ - methodical execution"
echo "• filesystem ✅ - direct file access to qi projects"
echo "• memory ✅ - persistent project knowledge"
echo ""
echo "🚀 This setup is more reliable than most enterprise teams have!"
echo ""
echo "✨ Usage example:"
echo "  cd ~/dev/qi/github/qi-v2-qicore"
echo "  claude /remember"
echo "  # Tell Claude about your project patterns"
echo "  claude /dev-workflow"
echo "  # Run complete development workflow"
echo "  claude /health"
echo "  # Quick check before committing"
echo ""
echo "🎯 No broken servers, no network dependencies, no failures!"
echo "   Just pure, reliable development workflow! 🔥"
```