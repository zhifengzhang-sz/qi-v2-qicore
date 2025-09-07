# QiCore Foundation Claude Code Workflow

Project-specific Claude Code workflow for QiCore Foundation development.

## Directory Structure

```
.claude/
├── commands/           # QiCore-specific commands
│   ├── qicore-design-first.md
│   ├── qicore-implement-feature.md
│   ├── qicore-check-compliance.md
│   └── qicore-perfect-workflow.md
├── knowledge/          # QiCore-specific knowledge
│   ├── qicore-foundation.md
│   └── project-stack.md
└── scripts/           # QiCore-specific automation
```

## QiCore-Specific Commands

| Command | Purpose | Usage |
|---------|---------|-------|
| `/qicore-design-first` | Read QiCore contracts and requirements | Before any implementation |
| `/qicore-implement-feature` | Implement with mathematical rigor | During development |
| `/qicore-check-compliance` | Verify laws and cross-language consistency | After implementation |
| `/qicore-perfect-workflow` | Complete QiCore development cycle | Full workflow |

## Integration with Global Workflow

This project workflow **extends** the global Claude Code workflow:

1. **Global First**: Use `claude /tech-check` to load current TypeScript patterns
2. **Project Second**: Use `claude /qicore-design-first` to load QiCore requirements
3. **Combined Implementation**: Use `claude /qicore-implement-feature` for compliant development

## QiCore Foundation Requirements

### Mathematical Laws (Non-Negotiable)
- **Functor Laws**: identity, composition
- **Monad Laws**: left identity, right identity, associativity  
- **Applicative Laws**: identity, composition, homomorphism, interchange

### Cross-Language Consistency
- Haskell reference implementation compatibility
- Identical behavioral semantics across languages
- Performance contract compliance (O(1) operations)

### Quality Standards
- Property-based testing (minimum 1000 iterations)
- Result<T> monad usage (no exceptions)
- STM concurrency patterns (Haskell)
- Behavioral contract adherence

## Example Usage

```bash
cd ~/dev/qi/github/qi-v2-qicore

# Load global + project knowledge
claude /tech-check
claude /qicore-design-first

# Implement QiCore feature
claude "Implement Result<T>.flatMap following QiCore mathematical contracts"

# Verify compliance
claude /qicore-check-compliance
```

This ensures both current technology patterns AND QiCore mathematical rigor.