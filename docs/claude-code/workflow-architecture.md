# Claude Code Workflow Architecture

## Overview
Multi-language development workflow system with global reusable components and project-specific extensions.

## Architecture Design

### Global Components (~/.claude/)
**Reusable across ALL projects of the same language**

```
~/.claude/
├── commands/
│   ├── tech-check.md              # Generic tech stack loading
│   ├── design-first.md            # Generic design doc compliance  
│   ├── implement-feature.md       # Generic implementation workflow
│   └── check-compliance.md        # Generic compliance checking
├── knowledge/
│   ├── tech-stacks/
│   │   ├── typescript.md          # Current TypeScript patterns
│   │   ├── python.md              # Current Python patterns
│   │   ├── haskell.md             # Current Haskell patterns
│   │   ├── bun.md                 # Current Bun patterns
│   │   ├── biome.md               # Current Biome patterns
│   │   └── vitest.md              # Current Vitest patterns
│   └── patterns/
│       ├── testing.md             # Generic testing patterns
│       ├── error-handling.md      # Generic error patterns
│       └── performance.md         # Generic performance patterns
└── scripts/
    ├── update-tech-knowledge.sh   # Auto-update tech patterns
    └── setup-new-project.sh       # Setup workflow for new project
```

### Project-Specific Components (project/.claude/)
**Specific to individual projects**

```
project/.claude/
├── commands/
│   ├── {project}-design-first.md     # Project-specific design requirements
│   ├── {project}-implement-feature.md # Project-specific implementation rules
│   ├── {project}-check-compliance.md # Project-specific compliance checks
│   └── {project}-perfect-workflow.md # Complete project workflow
├── knowledge/
│   ├── project-requirements.md       # Project-specific requirements
│   ├── architecture-patterns.md      # Project-specific architecture
│   └── tech-stack.md                # Project tech stack configuration
└── scripts/
    └── project-specific-automation.sh # Project automation
```

## Language Support Matrix

| Language   | Global Commands | Tech Stack Files | Project Extensions |
|------------|----------------|------------------|-------------------|
| TypeScript | ✅ Complete    | ✅ Bun/Biome/Vitest | ✅ Available |
| Python     | 🔄 In Progress | 🔄 Need to add | 🔄 Template ready |
| Haskell    | 🔄 In Progress | 🔄 Need to add | 🔄 Template ready |

## Installation Process

### 1. Install Global Workflow System
```bash
# Run the setup script
bash ~/.claude/scripts/setup-workflow.sh

# Or manual installation:
mkdir -p ~/.claude/{commands,knowledge/tech-stacks,knowledge/patterns,scripts}
# Copy global command files...
```

### 2. Setup Project-Specific Workflow
```bash
cd /path/to/your/project

# Auto-setup (recommended)
bash ~/.claude/scripts/setup-new-project.sh

# Or manual setup:
mkdir -p .claude/{commands,knowledge,scripts}
# Copy and customize project-specific templates...
```

## Manual Operation Guide

### For QiCore Foundation (This Project)
```bash
cd ~/dev/qi/github/qi-v2-qicore

# Phase 1: Load Knowledge
claude /tech-check                    # Global: TypeScript/Haskell patterns
claude /qicore-design-first          # Project: QiCore mathematical requirements

# Phase 2: Implement
claude /qicore-implement-feature     # Project: Mathematically rigorous implementation

# Phase 3: Verify
claude /qicore-check-compliance      # Project: Mathematical laws + cross-language consistency
```

### For New TypeScript Project
```bash
cd /path/to/new/typescript/project

# Setup project workflow
bash ~/.claude/scripts/setup-new-project.sh

# Use workflow
claude /tech-check                   # Global: Current TypeScript patterns
claude /design-first                 # Global: Design doc compliance
claude /implement-feature            # Global: Standard implementation
claude /check-compliance             # Global: Standard compliance
```

### For New Python Project
```bash
cd /path/to/new/python/project

# Setup project workflow  
bash ~/.claude/scripts/setup-new-project.sh --language python

# Use workflow
claude /tech-check                   # Global: Current Python patterns
claude /design-first                 # Global: Design doc compliance  
claude /implement-feature            # Global: Standard implementation
claude /check-compliance             # Global: Standard compliance
```

## Language-Specific Workflows

### TypeScript Projects
- **Global**: Bun, Biome, Vitest, modern TypeScript patterns
- **Project**: Can add React, Next.js, specific frameworks
- **Testing**: Vitest with property-based testing (fast-check)

### Python Projects  
- **Global**: Poetry, Ruff, Pytest, modern Python patterns
- **Project**: Can add Django, FastAPI, specific frameworks
- **Testing**: Pytest with property-based testing (Hypothesis)

### Haskell Projects
- **Global**: Cabal, GHC, modern Haskell patterns
- **Project**: Can add specific libraries, STM patterns
- **Testing**: QuickCheck with property-based testing

### Multi-Language Projects (like QiCore Foundation)
- **Global**: All language tech stacks
- **Project**: Cross-language consistency requirements
- **Testing**: Property-based testing in all languages with identical contracts

## Benefits

### Reusability
- Global commands work across all projects of same language
- Technology knowledge updates benefit all projects
- Consistent development patterns

### Flexibility  
- Project-specific requirements don't pollute global tools
- Easy to add new languages or frameworks
- Can mix and match global + project components

### Scalability
- New projects get instant workflow setup
- Technology updates propagate automatically
- Consistent quality across all projects

## Next Steps

1. **Complete Language Support**: Add Python and Haskell global patterns
2. **Auto-Setup Scripts**: Complete setup automation for new projects
3. **Cross-Language Patterns**: Define consistency patterns for multi-language projects
4. **Integration Testing**: Verify workflows across different project types