# Custom Commands Usage Guide

## Overview
This guide explains how to use the custom slash commands available in this project for efficient development workflows.

## Available Custom Commands

### General Commands (`/general:*`)
These work across all project types and languages.

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/general:tech-check` | Load current technology patterns | Before any implementation |
| `/general:quality-check` | Full project quality verification | Before commits |
| `/general:commit-assist` | Git commit message generation | When committing changes |
| `/general:setup-new-project` | Setup workflow for new projects | Starting new projects |

### Haskell Commands (`/haskell:*`)
Specific to Haskell development and knowledge updates.

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/haskell:knowledge-update-haskell` | Update Haskell/Nix knowledge from web | Weekly or before major changes |

### TypeScript Commands (`/typescript:*`)
Specific to TypeScript development with modern 2025 patterns.

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/typescript:tech-check` | Load 2025 TypeScript patterns and check project | Before TypeScript development |
| `/typescript:implement-feature` | Implement with modern TypeScript patterns | During TypeScript development |
| `/typescript:quality-check` | TypeScript-specific quality pipeline | Before TypeScript commits |
| `/typescript:create-unit-tests` | Create comprehensive TypeScript tests | When adding test coverage |
| `/typescript:upgrade-2025` | Upgrade to 2025 best practices | After running tech-check |

### QiCore Commands (`/qicore:*`)
Specific to QiCore Foundation mathematical requirements.

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/qicore:design-first` | Load mathematical contracts and requirements | Before QiCore implementation |
| `/qicore:implement-feature` | Implement with mathematical rigor | During QiCore development |
| `/qicore:check-compliance` | Verify mathematical laws and consistency | After QiCore implementation |
| `/qicore:perfect-workflow` | Complete QiCore development cycle | Full development workflow |

## Common Workflow Patterns

### Daily Development in QiCore Foundation
```bash
# Start your development session
claude /general:tech-check                    # Load current tech patterns
claude /qicore:design-first                  # Load mathematical requirements

# Implement features
claude /qicore:implement-feature            # Mathematically rigorous implementation

# Quality assurance
claude /qicore:check-compliance             # Verify laws and consistency
claude /general:quality-check              # Full quality pipeline

# Commit changes
claude /general:commit-assist               # Generate meaningful commit message
```

### TypeScript Development Workflow
```bash
# Check and upgrade TypeScript setup
claude /typescript:tech-check               # Check for 2025 updates
claude /typescript:upgrade-2025             # Apply recommendations

# Implement TypeScript features
claude /typescript:implement-feature        # Use modern patterns

# Testing and quality
claude /typescript:create-unit-tests        # Add comprehensive tests
claude /typescript:quality-check           # TypeScript quality pipeline

# Commit
claude /general:commit-assist
```

### New Project Setup
```bash
# Setup new project workflow
claude /general:setup-new-project           # Initialize project workflow

# Choose language-specific setup
claude /typescript:tech-check               # For TypeScript projects
# OR
claude /haskell:knowledge-update-haskell    # For Haskell projects
```

### Weekly Knowledge Updates
```bash
# Keep knowledge current
claude /haskell:knowledge-update-haskell    # Update Haskell ecosystem knowledge
claude /typescript:tech-check               # Check for TypeScript updates
```

## Command Details

### Using `/typescript:tech-check`
**Purpose**: Updates knowledge with 2025 TypeScript ecosystem and checks project setup.

**What it does**:
1. Searches web for latest TypeScript, Bun, Biome, Vitest versions
2. Checks `typescript/package.json`, `typescript/tsconfig.json`, etc.
3. Compares project setup against 2025 best practices
4. Provides upgrade recommendations

**Example output**:
```
✅ TypeScript 5.8+ available (you have 5.3.0)
⚠️ Should upgrade Biome to v2.0 for type-aware linting
✅ Fast-check property testing setup correctly
```

### Using `/qicore:implement-feature`
**Purpose**: Implement QiCore features with mathematical law compliance.

**What it does**:
1. Loads QiCore behavioral contracts from `docs/contracts/`
2. Applies mathematical rigor (Functor, Monad, Applicative laws)
3. Ensures cross-language consistency with Haskell reference
4. Uses current technology patterns

**Example usage**:
```bash
claude /qicore:implement-feature
# Then provide specific task:
# "Implement Result<T>.sequence operation following QiCore mathematical contracts"
```

### Using `/general:commit-assist`
**Purpose**: Generate meaningful commit messages following project conventions.

**What it does**:
1. Analyzes `git status` and `git diff`
2. Determines change type (feat, fix, refactor, test, etc.)
3. Generates commit message with mathematical context (for QiCore)
4. Follows conventional commit format

**Example output**:
```
feat(base): implement Result<T>.sequence operation

Add sequence operation for Result<T> that transforms List<Result<T, E>> 
to Result<List<T>, E>. Implementation preserves all mathematical laws:
- Functor composition and identity
- Applicative homomorphism and interchange
- Maintains O(1) complexity per element

Property-based tests verify behavior with 1000+ iterations.
```

## Advanced Usage

### Chaining Commands
```bash
# Complete quality workflow
claude /typescript:tech-check && claude /typescript:upgrade-2025 && claude /typescript:quality-check
```

### Project-Specific Workflows
```bash
# QiCore mathematical implementation workflow
claude /general:tech-check
claude /qicore:design-first
claude "Implement Result<T>.flatMap with Monad law verification"
claude /qicore:check-compliance
```

### Integration with Build Tools
```bash
# After running commands, execute build tools
claude /typescript:quality-check
cd typescript && bun run check
```

## Tips for Effective Usage

### 1. Always Start with Tech Check
```bash
# Before any development session
claude /general:tech-check        # or
claude /typescript:tech-check     # for TypeScript work
```

### 2. Use Design-First for Complex Features
```bash
# Before implementing major features
claude /qicore:design-first       # Load all requirements first
```

### 3. Regular Knowledge Updates
```bash
# Weekly or before major changes
claude /haskell:knowledge-update-haskell
claude /typescript:tech-check     # Also updates from web
```

### 4. Quality Gates Before Commits
```bash
# Always verify before committing
claude /qicore:check-compliance   # or
claude /typescript:quality-check  # depending on what you're working on
claude /general:commit-assist     # Generate good commit message
```

## Troubleshooting

### Command Not Found
```bash
# Check available commands
claude /help

# Verify you're in the right directory
pwd
# Should be: /home/zzhang/dev/qi/github/qi-v2-qicore

# Check command files exist
ls .claude/commands/
```

### Command Doesn't Work as Expected
```bash
# Check the command file content
cat .claude/commands/typescript/tech-check.md

# Run commands step by step instead of chaining
```

### Out-of-Date Recommendations
```bash
# Update knowledge first
claude /haskell:knowledge-update-haskell
claude /typescript:tech-check  # This searches web for latest info
```

## Best Practices

1. **Start each session** with appropriate tech-check command
2. **Use project-specific commands** when available (e.g., `/qicore:*` for this project)
3. **Chain commands logically**: tech-check → implement → quality-check → commit
4. **Update knowledge regularly** to get latest recommendations
5. **Verify compliance** before every commit for mathematical correctness
6. **Use descriptive commit messages** generated by `/general:commit-assist`

**These custom commands automate repetitive workflows while ensuring consistent quality and mathematical rigor throughout the QiCore Foundation development process.**