# Claude Code Workflow Operation Guide

## Overview
Complete manual operation guide for using the Claude Code workflow in this project and new projects across multiple languages.

## QiCore Foundation (This Project)

### Daily Development Workflow
```bash
cd ~/dev/qi/github/qi-v2-qicore

# Phase 1: Load Knowledge
claude /tech-check                    # Global: TypeScript/Haskell patterns  
claude /qicore-design-first          # Project: Mathematical requirements

# Phase 2: Implement
claude /qicore-implement-feature     # Project: Mathematically rigorous implementation

# Phase 3: Verify  
claude /qicore-check-compliance      # Project: Mathematical laws + cross-language consistency

# Phase 4: Build & Test
cabal test --test-options="--quickcheck-tests=1000"  # Haskell
# When TypeScript implemented:
# bun run typecheck && bun run lint && bun run test
```

### Specific QiCore Tasks

**Implementing Result<T> Operations:**
```bash
claude /tech-check
claude /qicore-design-first
claude "Implement Result<T>.flatMap operation following QiCore mathematical contracts and current TypeScript patterns"
claude /qicore-check-compliance
```

**Cross-Language Consistency Check:**
```bash
claude "Compare Haskell and TypeScript Result<T> implementations for behavioral consistency"
claude /qicore-check-compliance
```

**Mathematical Law Verification:**
```bash
claude "Verify Functor laws for Result<T> implementation with property-based tests (1000+ iterations)"
claude "Run property tests: cabal test --test-options='--quickcheck-tests=1000'"
```

## New TypeScript Project

### Project Setup
```bash
cd /path/to/new/typescript/project

# Setup project workflow (one-time)
mkdir -p .claude/{commands,knowledge,scripts}
cp ~/.claude/templates/typescript/* .claude/commands/
# Customize .claude/commands/ files for your project

# Daily workflow
claude /tech-check                   # Global: Current TypeScript patterns
claude /design-first                 # Global: Design doc compliance
claude /implement-feature            # Global: Standard implementation  
claude /check-compliance             # Global: Standard compliance
```

### TypeScript-Specific Operations

**Modern TypeScript Implementation:**
```bash
claude /tech-check
claude "Implement authentication service using current Bun, Biome, and TypeScript patterns"
bun run typecheck && bun run lint && bun run test
```

**Adding New Framework (e.g., React):**
```bash
claude /tech-check
claude "Add React components following current TypeScript patterns and project design docs"
```

## New Python Project

### Project Setup
```bash
cd /path/to/new/python/project

# Setup project workflow (one-time)  
mkdir -p .claude/{commands,knowledge,scripts}
cp ~/.claude/templates/python/* .claude/commands/
# Customize .claude/commands/ files for your project

# Daily workflow
claude /tech-check                   # Global: Current Python patterns
claude /design-first                 # Global: Design doc compliance
claude /implement-feature            # Global: Standard implementation
claude /check-compliance             # Global: Standard compliance
```

### Python-Specific Operations

**Modern Python Implementation:**
```bash
claude /tech-check
claude "Implement FastAPI service using current Poetry, Ruff, and Python patterns"
poetry run pytest --cov
```

**Property-Based Testing:**
```bash
claude "Add Hypothesis property-based tests for the data validation functions"
```

## New Haskell Project

### Project Setup
```bash
cd /path/to/new/haskell/project

# Setup project workflow (one-time)
mkdir -p .claude/{commands,knowledge,scripts}  
cp ~/.claude/templates/haskell/* .claude/commands/
# Customize .claude/commands/ files for your project

# Daily workflow
claude /tech-check                   # Global: Current Haskell patterns
claude /design-first                 # Global: Design doc compliance
claude /implement-feature            # Global: Standard implementation
claude /check-compliance             # Global: Standard compliance
```

### Haskell-Specific Operations

**Modern Haskell Implementation:**
```bash
claude /tech-check
claude "Implement STM-based concurrent service using current Cabal and GHC patterns"
cabal test
```

**Mathematical Verification:**
```bash
claude "Add QuickCheck property tests with 1000+ iterations for mathematical laws"
```

## Multi-Language Projects

### Cross-Language Development (like QiCore)
```bash
# Load all language knowledge
claude /tech-check  # Will load TypeScript, Python, Haskell patterns

# Project-specific requirements
claude /project-design-first  # Load cross-language consistency requirements

# Implement with consistency
claude "Implement Result<T> monad in TypeScript maintaining behavioral consistency with Haskell reference implementation"

# Verify consistency
claude /project-check-compliance  # Check cross-language behavioral consistency
```

## Command Reference

### Global Commands (Available for all projects)
| Command | Purpose | Usage Context |
|---------|---------|---------------|
| `/tech-check` | Load current tech patterns | Before any implementation |
| `/design-first` | Read design documentation | Before feature development |
| `/implement-feature` | Standard implementation | During development |
| `/check-compliance` | Verify compliance | After implementation |

### QiCore-Specific Commands (This project only)
| Command | Purpose | QiCore Context |
|---------|---------|----------------|
| `/qicore-design-first` | Load mathematical requirements | Before QiCore implementation |
| `/qicore-implement-feature` | Mathematically rigorous implementation | QiCore development |
| `/qicore-check-compliance` | Verify mathematical laws | QiCore verification |
| `/qicore-perfect-workflow` | Complete QiCore workflow | Full development cycle |

## Language-Specific Patterns

### TypeScript
- **Tech Stack**: Bun + Biome + Vitest + Modern TypeScript
- **Patterns**: Satisfies operator, branded types, const assertions
- **Testing**: Vitest with fast-check for property-based testing
- **Build**: `bun run typecheck && bun run lint && bun run test`

### Python  
- **Tech Stack**: Poetry + Ruff + Pytest + Modern Python
- **Patterns**: Type hints, dataclasses, async/await
- **Testing**: Pytest with Hypothesis for property-based testing
- **Build**: `poetry run pytest --cov && poetry run ruff check`

### Haskell
- **Tech Stack**: Cabal + GHC 9.10+ + Nix + Modern Haskell
- **Patterns**: GHC2024, STM, strict evaluation
- **Testing**: QuickCheck with 1000+ iterations for mathematical laws
- **Build**: `cabal test --test-options="--quickcheck-tests=1000"`

## Troubleshooting

### Command Not Found
```bash
# Check you're in the right directory
pwd

# Check command files exist
ls .claude/commands/       # Project commands
ls ~/.claude/commands/     # Global commands
```

### Outdated Patterns
```bash
# Update global knowledge
~/.claude/scripts/update-tech-knowledge.sh

# Reload in new session
claude /tech-check
```

### Project-Specific Issues
```bash
# Check project setup
ls .claude/
cat .claude/knowledge/project-requirements.md

# Reload project context
claude /[project]-design-first
```

## Best Practices

1. **Always start with `/tech-check`** to load current patterns
2. **Use project-specific commands** when available (e.g., `/qicore-*`)
3. **Verify compliance** after every significant change
4. **Update knowledge base** regularly for latest patterns
5. **Customize project commands** for your specific requirements
6. **Test mathematical laws** with 1000+ iterations for rigorous projects
7. **Maintain cross-language consistency** in multi-language projects