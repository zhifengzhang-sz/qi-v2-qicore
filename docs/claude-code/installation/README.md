# Claude Code Workflow Installation Guide

## Overview
Complete installation guide for the Claude Code workflow system with global reusable components and project-specific extensions.

## Prerequisites
- Claude Code CLI installed and configured
- MCP servers available (memory, filesystem, sequential-thinking)
- Git repository for your project

## Installation Methods

### Method 1: Automated Installation (Recommended)
```bash
# Install global workflow system
bash ~/.claude/scripts/setup-workflow.sh

# Setup this project's workflow
cd ~/dev/qi/github/qi-v2-qicore
bash ~/.claude/scripts/setup-project-workflow.sh
```

### Method 2: Manual Installation

#### Step 1: Install Global Components
Create global directory structure:
```bash
mkdir -p ~/.claude/{commands,knowledge/tech-stacks,knowledge/patterns,scripts}
```

Install global command files:
```bash
# Copy command files
cp docs/claude-code/installation/global-commands/* ~/.claude/commands/

# Or create manually:
# ~/.claude/commands/tech-check.md
# ~/.claude/commands/design-first.md  
# ~/.claude/commands/implement-feature.md
# ~/.claude/commands/check-compliance.md
```

Install technology knowledge base:
```bash
# Copy tech stack knowledge
cp docs/claude-code/installation/global-knowledge/* ~/.claude/knowledge/tech-stacks/

# Run update script to get latest versions
~/.claude/scripts/update-tech-knowledge.sh
```

#### Step 2: Setup Project-Specific Components
Create project structure:
```bash
cd ~/dev/qi/github/qi-v2-qicore
mkdir -p .claude/{commands,knowledge,scripts}
```

Install QiCore-specific commands:
```bash
# Already exists in this project:
# .claude/commands/qicore-design-first.md
# .claude/commands/qicore-implement-feature.md
# .claude/commands/qicore-check-compliance.md
# .claude/commands/qicore-perfect-workflow.md
```

## Language Support Setup

### TypeScript Projects (Complete)
Global knowledge already includes:
- `~/.claude/knowledge/tech-stacks/typescript.md`
- `~/.claude/knowledge/tech-stacks/bun.md`
- `~/.claude/knowledge/tech-stacks/biome.md`
- `~/.claude/knowledge/tech-stacks/vitest.md`

### Python Projects (Setup Required)
```bash
# Create Python knowledge files
cat > ~/.claude/knowledge/tech-stacks/python.md << 'EOF'
# Python Current Best Practices (2024)

## Package Management
- **Poetry**: Preferred for dependency management
- **Ruff**: Fast linting and formatting
- **Pytest**: Testing framework

## Modern Python Patterns
- Type hints with `typing` module
- Dataclasses for structured data
- Context managers for resource handling
- Async/await for concurrent operations
EOF
```

### Haskell Projects (Setup Required)
```bash
# Create Haskell knowledge files  
cat > ~/.claude/knowledge/tech-stacks/haskell.md << 'EOF'
# Haskell Current Best Practices (2024)

## Build System
- **Cabal**: Standard build tool
- **GHC 9.10+**: Modern compiler
- **Nix**: Reproducible development environments

## Modern Haskell Patterns
- GHC2024 language edition
- STM for concurrent programming
- QuickCheck for property-based testing
- Strict evaluation with BangPatterns
EOF
```

## Project Setup for New Projects

### TypeScript Project
```bash
cd /path/to/new/typescript/project

# Create project workflow structure
mkdir -p .claude/{commands,knowledge,scripts}

# Copy template project commands
cp ~/.claude/templates/typescript/* .claude/commands/

# Customize for your project
# Edit .claude/commands/project-design-first.md
# Edit .claude/commands/project-implement-feature.md
```

### Python Project
```bash
cd /path/to/new/python/project

# Create project workflow structure
mkdir -p .claude/{commands,knowledge,scripts}

# Copy template project commands
cp ~/.claude/templates/python/* .claude/commands/

# Customize for your project
```

### Haskell Project
```bash
cd /path/to/new/haskell/project

# Create project workflow structure
mkdir -p .claude/{commands,knowledge,scripts}

# Copy template project commands
cp ~/.claude/templates/haskell/* .claude/commands/

# Customize for your project
```

## Verification

### Test Global Installation
```bash
# Check directory structure
ls -la ~/.claude/
ls -la ~/.claude/commands/
ls -la ~/.claude/knowledge/tech-stacks/

# Test knowledge update
~/.claude/scripts/update-tech-knowledge.sh

# Test command availability
claude /tech-check
```

### Test Project Installation  
```bash
cd ~/dev/qi/github/qi-v2-qicore

# Check project structure
ls -la .claude/
ls -la .claude/commands/

# Test QiCore commands
claude /qicore-design-first
claude /qicore-perfect-workflow
```

## Troubleshooting

### Common Issues

**Commands not found:**
```bash
# Check directory exists
ls ~/.claude/commands/

# Check file permissions
chmod +r ~/.claude/commands/*.md
```

**Knowledge base empty:**
```bash
# Run update script
~/.claude/scripts/update-tech-knowledge.sh

# Check files exist
ls ~/.claude/knowledge/tech-stacks/
```

**Project commands not working:**
```bash
# Check you're in project directory
pwd
# Should be: /home/zzhang/dev/qi/github/qi-v2-qicore

# Check project structure exists
ls .claude/commands/
```

### Reset Installation
```bash
# Remove and reinstall global
rm -rf ~/.claude
bash docs/claude-code/installation/setup-workflow.sh

# Remove and reinstall project
rm -rf .claude
mkdir -p .claude/{commands,knowledge,scripts}
# Copy files manually
```

## Next Steps

1. **Test the workflow**: Try the operation guide
2. **Customize for your needs**: Modify commands and knowledge
3. **Add more languages**: Create additional tech stack knowledge
4. **Share with team**: Document project-specific setup