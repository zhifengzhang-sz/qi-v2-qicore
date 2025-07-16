# Claude Code Workflow System

## Overview
Multi-language development workflow system with global reusable components and project-specific extensions.

## Directory Structure

```
docs/claude-code/
├── README.md                    # This overview
├── workflow-architecture.md    # System architecture design
├── installation/
│   ├── README.md               # Complete installation guide
│   ├── setup-guide.md          # Knowledge setup steps
│   ├── setup.simple.md         # Simple setup
│   ├── step.1-2.md            # Setup steps 1-2
│   └── step.3.md              # Setup step 3
├── operation/
│   ├── README.md               # Manual operation guide
│   ├── usage-overview.md       # Usage overview
│   └── usage.md               # Basic usage
├── reference/
│   ├── design-guide.md         # Design compliance system
│   ├── solution-summary.md     # Complete solution summary
│   └── mcp-servers.md          # MCP server information
└── architecture/
    └── (reserved for future architecture docs)
```

## Quick Start

### 1. Installation
```bash
# Install global workflow system
bash ~/.claude/scripts/setup-workflow.sh

# Setup this project
cd ~/dev/qi/github/qi-v2-qicore
bash ~/.claude/scripts/setup-project-workflow.sh
```

### 2. Daily Usage (QiCore Foundation)
```bash
cd ~/dev/qi/github/qi-v2-qicore

claude /general:tech-check                    # Load current tech patterns
claude /qicore:design-first                  # Load mathematical requirements  
claude /qicore:implement-feature             # Implement with rigor
claude /qicore:check-compliance              # Verify laws and consistency
```

### 3. TypeScript Development
```bash
cd ~/dev/qi/github/qi-v2-qicore

claude /typescript:tech-check                # Check 2025 TypeScript updates
claude /typescript:upgrade-2025              # Apply latest best practices
claude /typescript:implement-feature         # Implement with modern patterns
claude /typescript:quality-check             # TypeScript-specific quality
```

### 4. Custom Commands Available
**See [Custom Commands Guide](operation/custom-commands.md) for complete usage documentation.**

**Available command namespaces:**
- `/general:*` - Cross-language commands
- `/haskell:*` - Haskell-specific commands  
- `/typescript:*` - TypeScript-specific commands
- `/qicore:*` - QiCore Foundation mathematical commands

## Language Support

| Language   | Status | Global Knowledge | Project Templates |
|------------|--------|------------------|-------------------|
| TypeScript | ✅ Complete | Bun, Biome, Vitest | QiCore Foundation |
| Python     | ✅ Complete | Poetry, Ruff, Pytest | Template ready |
| Haskell    | ✅ Complete | Cabal, GHC, QuickCheck | Template ready |

## Architecture Benefits

### Global Reusability
- `~/.claude/` contains reusable patterns for all projects
- Technology knowledge updates benefit all projects
- Consistent development patterns across projects

### Project Specificity  
- `.claude/` in each project for specific requirements
- QiCore Foundation has mathematical rigor requirements
- Project-specific compliance rules don't pollute global tools

### Multi-Language Support
- Same workflow commands work across TypeScript, Python, Haskell
- Language-specific tech patterns loaded automatically
- Cross-language consistency for multi-language projects

## Key Features

### For QiCore Foundation
- **Mathematical Laws**: Automated verification of Functor, Monad, Applicative laws
- **Cross-Language Consistency**: Haskell ↔ TypeScript behavioral consistency  
- **Property-Based Testing**: 1000+ iterations for mathematical verification
- **Performance Contracts**: O(1) complexity guarantees

### For General Projects
- **Current Tech Patterns**: Always uses latest best practices
- **Design Compliance**: Automated adherence to design documentation
- **Quality Assurance**: Integrated linting, testing, type checking
- **Knowledge Management**: Persistent learning across sessions

## Next Steps

1. **Read the guides**:
   - [Installation Guide](installation/README.md)
   - [Operation Guide](operation/README.md)  
   - [Architecture Design](workflow-architecture.md)

2. **Try the workflow**:
   - Test with this project (QiCore Foundation)
   - Setup a new project using templates
   - Customize for your specific needs

3. **Extend the system**:
   - Add new languages or frameworks
   - Create project-specific patterns
   - Share improvements with the team

## Documentation Index

- **Installation**: Complete setup instructions for global and project components
- **Operation**: Manual operation guides for daily development workflow
- **Reference**: Design guides, solution summaries, and technical references
- **Architecture**: System design and component interaction patterns

This workflow system solves the core Claude Code challenges of outdated knowledge and inconsistent design compliance while providing a scalable foundation for multi-language development.