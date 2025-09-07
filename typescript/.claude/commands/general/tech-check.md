# Tech Check

**INSTRUCTION FOR AI ASSISTANT**: Load current technology knowledge and verify project configuration.

## Technology Knowledge Loading

### 1. Read Current Tech Patterns (**filesystem**)
- Read `/home/zzhang/.claude/knowledge/tech-stacks/typescript.md` - Current TypeScript patterns
- Read `/home/zzhang/.claude/knowledge/tech-stacks/bun.md` - Current Bun patterns  
- Read `/home/zzhang/.claude/knowledge/tech-stacks/biome.md` - Current Biome patterns
- Read `/home/zzhang/.claude/knowledge/tech-stacks/vitest.md` - Current Vitest patterns
- Read `/home/zzhang/.claude/knowledge/tech-stacks/haskell.md` - Current Haskell patterns

### 2. Check Project Configuration (**filesystem**)
- Read `package.json` (if exists) - Check versions and scripts
- Read `tsconfig.json` (if exists) - Verify TypeScript settings
- Read `biome.json` (if exists) - Check linting/formatting rules
- Read `vitest.config.ts` (if exists) - Verify test configuration  
- Read `cabal.project` and `*.cabal` files - Check Haskell configuration
- Read `CLAUDE.md` - Project-specific instructions

### 3. Store Knowledge (**memory**)
- Store current technology patterns for this session
- Remember project-specific configuration details
- Note any version conflicts or compatibility issues

## Project Context (QiCore Foundation)
This is a multi-language project with:
- **Haskell**: Reference implementation with mathematical rigor
- **TypeScript**: Production implementation (planned)
- **Mathematical Requirements**: Functor, Monad, Applicative laws
- **Testing**: Property-based testing with 1000+ iterations
- **Performance**: O(1) complexity guarantees

## Expected Outcome
After running this command, you should:
- Have current technology patterns loaded
- Understand project configuration
- Be ready to implement features with current best practices
- Know which tools and versions are being used

**Always run this before implementing new features.**