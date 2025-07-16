# Setup New Project

**INSTRUCTION FOR AI ASSISTANT**: Help set up a new project with the Claude Code workflow system.

## Project Setup Process

### 1. Analyze Project Type (**filesystem**)
- Check existing files to determine project type:
  - `package.json` → TypeScript/JavaScript project
  - `pyproject.toml` or `requirements.txt` → Python project
  - `*.cabal` or `stack.yaml` → Haskell project
  - Multiple languages → Multi-language project

### 2. Create Project Workflow Structure
```bash
mkdir -p .claude/{commands,knowledge,scripts}
```

### 3. Language-Specific Setup

**For TypeScript Projects:**
- Copy TypeScript command templates
- Set up Bun, Biome, Vitest configuration
- Create TypeScript-specific knowledge files
- Configure modern TypeScript patterns

**For Python Projects:**
- Copy Python command templates
- Set up Poetry, Ruff, Pytest configuration
- Create Python-specific knowledge files
- Configure modern Python patterns

**For Haskell Projects:**
- Copy Haskell command templates
- Set up Cabal, GHC, QuickCheck configuration
- Create Haskell-specific knowledge files
- Configure modern Haskell patterns

**For Multi-Language Projects:**
- Copy all relevant language templates
- Set up cross-language consistency requirements
- Create behavioral contracts
- Configure language-specific build systems

### 4. Create Project-Specific Commands

**Required Commands:**
- `project-design-first.md` - Project-specific design requirements
- `project-implement-feature.md` - Project-specific implementation rules
- `project-check-compliance.md` - Project-specific compliance checks
- `project-workflow.md` - Complete project workflow

### 5. Customize for Project Requirements

**Update based on:**
- Project domain and requirements
- Specific design patterns needed
- Quality gates and testing requirements
- Integration with existing tools
- Team conventions and standards

### 6. Create Project Knowledge Base

**Files to create:**
- `.claude/knowledge/project-requirements.md`
- `.claude/knowledge/architecture-patterns.md`
- `.claude/knowledge/tech-stack.md`
- `.claude/knowledge/team-conventions.md`

### 7. Verify Setup

**Test that commands work:**
- `/tech-check` - Loads technology patterns
- `/project-design-first` - Loads project requirements
- `/project-implement-feature` - Works with project context
- `/project-check-compliance` - Validates project standards

## Template Commands

### Basic Project Design First
```markdown
# Project Design First

**INSTRUCTION FOR AI ASSISTANT**: Load project-specific design requirements.

## Project Context
[Customize based on project]

## Design Documents to Read
- README.md
- docs/architecture.md
- docs/api.md
- [Add project-specific design docs]

## Requirements to Remember
- [Project-specific requirements]
- [Quality standards]
- [Technology constraints]
```

### Basic Project Implementation
```markdown
# Project Implement Feature

**INSTRUCTION FOR AI ASSISTANT**: Implement features following project standards.

## Implementation Requirements
- Follow project architecture patterns
- Use approved technology stack
- Maintain quality standards
- Follow team conventions

## Quality Gates
- [Project-specific quality requirements]
- Testing requirements
- Performance requirements
- Documentation requirements
```

## Project Types

### Standard Web Application
- TypeScript + React/Next.js
- Modern build tools (Vite/Bun)
- Testing with Vitest
- Standard web patterns

### Backend Service
- TypeScript/Python/Haskell
- API design patterns
- Database integration
- Microservice architecture

### Library/Framework
- Mathematical rigor (like QiCore)
- Cross-language consistency
- Property-based testing
- Performance contracts

### Data Science Project
- Python + Jupyter
- Data processing patterns
- Statistical testing
- Visualization standards

## Success Criteria

**Project setup complete when:**
- [ ] `.claude/` directory structure created
- [ ] Language-specific commands installed
- [ ] Project-specific commands customized
- [ ] Knowledge base populated
- [ ] Commands tested and working
- [ ] Team onboarded to workflow

**Provide a complete, working project workflow tailored to the specific project needs.**