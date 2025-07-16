```bash
#!/bin/bash

# ============================================
# Knowledge Update System for Claude Code
# Solve the outdated knowledge problem
# ============================================

echo "🧠 Setting up Knowledge Update System..."

# ============================================
# Solution 1: Create Project Knowledge Base
# ============================================

# Create knowledge directory structure
mkdir -p ~/.claude/knowledge
mkdir -p ~/.claude/knowledge/tech-stacks
mkdir -p ~/.claude/knowledge/patterns
mkdir -p ~/.claude/knowledge/best-practices

echo "📚 Creating current technology knowledge base..."

# ============================================
# Current Technology Knowledge Files
# ============================================

# Bun knowledge (2024)
cat > ~/.claude/knowledge/tech-stacks/bun.md << 'EOF'
# Bun Current Best Practices (2024)

## Installation & Setup
```bash
# Install Bun
curl -fsSL https://bun.sh/install | bash

# Initialize project
bun init

# Install dependencies
bun install
bun add package-name
bun add -d dev-package
```

## Package.json Scripts
```json
{
  "scripts": {
    "dev": "bun run --watch src/index.ts",
    "build": "bun build src/index.ts --outdir ./dist",
    "test": "bun test",
    "start": "bun run dist/index.js"
  }
}
```

## Bun Test Framework
```typescript
import { expect, test, describe } from "bun:test";

describe("example", () => {
  test("2 + 2", () => {
    expect(2 + 2).toBe(4);
  });
});
```

## Current Patterns (2024)
- Use `bun run` instead of `npm run`
- Native TypeScript support (no transpilation needed)
- Built-in bundler and test runner
- Superior performance to npm/yarn
EOF

# Biome knowledge (2024)
cat > ~/.claude/knowledge/tech-stacks/biome.md << 'EOF'
# Biome Current Best Practices (2024)

## Installation & Setup
```bash
# Install Biome
bun add -d @biomejs/biome

# Initialize config
bunx @biomejs/biome init
```

## biome.json Configuration
```json
{
  "$schema": "https://biomejs.dev/schemas/1.4.1/schema.json",
  "organizeImports": {
    "enabled": true
  },
  "linter": {
    "enabled": true,
    "rules": {
      "recommended": true,
      "correctness": {
        "noUnusedVariables": "error"
      },
      "style": {
        "useConst": "error"
      }
    }
  },
  "formatter": {
    "enabled": true,
    "formatWithErrors": false,
    "indentStyle": "space",
    "indentSize": 2,
    "lineWidth": 100
  },
  "javascript": {
    "formatter": {
      "quoteStyle": "single",
      "trailingComma": "es5"
    }
  }
}
```

## Commands (2024)
```bash
# Check and fix
bunx @biomejs/biome check --apply .

# Format only
bunx @biomejs/biome format --write .

# Lint only
bunx @biomejs/biome lint .
```

## Package.json Scripts
```json
{
  "scripts": {
    "lint": "biome check .",
    "lint:fix": "biome check --apply .",
    "format": "biome format --write ."
  }
}
```
EOF

# Vitest knowledge (2024)
cat > ~/.claude/knowledge/tech-stacks/vitest.md << 'EOF'
# Vitest Current Best Practices (2024)

## Installation
```bash
bun add -d vitest @vitest/ui
bun add -d jsdom # for DOM testing
```

## vitest.config.ts
```typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['./src/test/setup.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'html'],
      exclude: ['node_modules/', 'src/test/']
    }
  }
})
```

## Package.json Scripts
```json
{
  "scripts": {
    "test": "vitest",
    "test:ui": "vitest --ui",
    "test:run": "vitest run",
    "test:coverage": "vitest run --coverage"
  }
}
```

## Current Testing Patterns (2024)
```typescript
import { describe, it, expect, vi, beforeEach } from 'vitest'

describe('Component', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('should work', () => {
    expect(true).toBe(true)
  })
})
```

## Mocking (2024)
```typescript
// Mock modules
vi.mock('../module', () => ({
  default: vi.fn(),
  namedExport: vi.fn()
}))

// Mock functions
const mockFn = vi.fn()
mockFn.mockReturnValue('value')
```
EOF

# TypeScript knowledge (2024)
cat > ~/.claude/knowledge/tech-stacks/typescript.md << 'EOF'
# TypeScript Current Best Practices (2024)

## tsconfig.json (Modern Setup)
```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "noEmit": true,
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noImplicitOverride": true,
    "isolatedModules": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
```

## Current Type Patterns (2024)
```typescript
// Use satisfies operator
const config = {
  theme: 'dark',
  lang: 'en'
} satisfies Config

// Better error handling
type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E }

// Template literal types
type EventName = `on${Capitalize<string>}`

// Branded types
type UserId = string & { readonly brand: unique symbol }

// Const assertions
const fruits = ['apple', 'banana'] as const
type Fruit = typeof fruits[number]
```

## Modern Utility Types
```typescript
// Pick specific keys
type UserPreview = Pick<User, 'id' | 'name'>

// Make all optional
type PartialUser = Partial<User>

// Make all required
type RequiredUser = Required<User>

// Exclude keys
type UserWithoutPassword = Omit<User, 'password'>
```
EOF

# ============================================
# Solution 2: Knowledge Update Commands
# ============================================

cat > ~/.claude/commands/update-knowledge.md << 'EOF'
# Update Technology Knowledge

Before any implementation, update knowledge about current technologies and best practices.

## Knowledge Sources to Check:
1. **Project Files**: Read package.json, tsconfig.json, biome.json, etc.
2. **Local Knowledge**: Check ~/.claude/knowledge/ for current patterns
3. **Memory Storage**: Recall previously learned current practices

## Technology Stack Updates:
### Before using Bun:
- Read ~/.claude/knowledge/tech-stacks/bun.md
- Check project's package.json for current Bun version
- Verify current Bun commands and patterns

### Before using Biome:
- Read ~/.claude/knowledge/tech-stacks/biome.md  
- Check project's biome.json configuration
- Use current Biome commands and rule sets

### Before using Vitest:
- Read ~/.claude/knowledge/tech-stacks/vitest.md
- Check project's vitest.config.ts
- Use current testing patterns and APIs

### Before TypeScript implementation:
- Read ~/.claude/knowledge/tech-stacks/typescript.md
- Check project's tsconfig.json settings
- Apply modern TypeScript patterns (satisfies, branded types, etc.)

## Update Process:
1. **filesystem**: Read current project configuration files
2. **memory**: Store any new patterns discovered
3. **sequential-thinking**: Apply current best practices systematically
4. Never use outdated patterns or deprecated APIs

## Success Criteria:
- All implementations use current best practices
- Project configuration is respected
- Modern patterns are applied consistently
- No deprecated APIs or patterns used
EOF

# ============================================
# Solution 3: Pre-Implementation Check
# ============================================

cat > ~/.claude/commands/tech-check.md << 'EOF'
# Technology Stack Check

Always run this before any implementation to ensure current knowledge.

## Pre-Implementation Checklist:
Use **sequential-thinking** to systematically:

### 1. Read Project Configuration (**filesystem**)
- package.json: Check versions and scripts
- tsconfig.json: Verify TypeScript settings  
- biome.json: Check linting/formatting rules
- vitest.config.ts: Verify test configuration
- Any other config files in project root

### 2. Load Current Knowledge (**filesystem**)
- Read ~/.claude/knowledge/tech-stacks/[technology].md
- Apply current best practices for each technology
- **memory**: Recall any project-specific patterns

### 3. Verify Compatibility
- Ensure all technologies work together
- Check for version conflicts
- Verify configuration compatibility

### 4. Apply Current Patterns
- Use modern syntax and patterns
- Follow current architectural recommendations
- Implement with latest best practices

## Example Usage:
"Before implementing authentication, run tech-check to ensure I'm using current Bun, TypeScript, and Vitest patterns"

## Output:
- Summary of current project tech stack
- Current best practices to follow
- Any compatibility issues found
- Recommended patterns for implementation
EOF

# ============================================
# Solution 4: Project-Specific Knowledge Storage
# ============================================

echo "📁 Creating project-specific knowledge storage..."

# Function to create project knowledge
create_project_knowledge() {
    local project_path=$1
    local project_name=$(basename "$project_path")
    
    mkdir -p "$project_path/.claude"
    mkdir -p "$project_path/.claude/knowledge"
    
    cat > "$project_path/.claude/knowledge/tech-stack.md" << EOF
# $project_name Technology Stack

## Current Dependencies
\`\`\`json
$(cat "$project_path/package.json" 2>/dev/null | jq '.dependencies // {}' 2>/dev/null || echo "{}")
\`\`\`

## Dev Dependencies  
\`\`\`json
$(cat "$project_path/package.json" 2>/dev/null | jq '.devDependencies // {}' 2>/dev/null || echo "{}")
\`\`\`

## Configuration Files
- TypeScript: $([ -f "$project_path/tsconfig.json" ] && echo "✅" || echo "❌")
- Biome: $([ -f "$project_path/biome.json" ] && echo "✅" || echo "❌")
- Vitest: $([ -f "$project_path/vitest.config.ts" ] && echo "✅" || echo "❌")

## Project-Specific Patterns
- Add patterns and conventions used in this project
- Document architectural decisions
- Note any deviations from standard practices

## Last Updated
$(date)
EOF

    echo "✅ Created knowledge base for $project_name"
}

# Create knowledge for qi projects
if [ -d "$HOME/dev/qi/github/qi-v2-qicore" ]; then
    create_project_knowledge "$HOME/dev/qi/github/qi-v2-qicore"
fi

# ============================================
# Solution 5: Auto-Knowledge Update Script
# ============================================

cat > ~/.claude/scripts/update-tech-knowledge.sh << 'EOF'
#!/bin/bash

# Auto-update technology knowledge from official sources
echo "🔄 Updating technology knowledge..."

# Update Bun knowledge
echo "📦 Updating Bun knowledge..."
curl -s https://api.github.com/repos/oven-sh/bun/releases/latest | \
    jq -r '.tag_name' > ~/.claude/knowledge/tech-stacks/bun-version.txt

# Update TypeScript knowledge  
echo "📘 Updating TypeScript knowledge..."
curl -s https://api.github.com/repos/microsoft/TypeScript/releases/latest | \
    jq -r '.tag_name' > ~/.claude/knowledge/tech-stacks/typescript-version.txt

# Update Biome knowledge
echo "🌿 Updating Biome knowledge..."
curl -s https://api.github.com/repos/biomejs/biome/releases/latest | \
    jq -r '.tag_name' > ~/.claude/knowledge/tech-stacks/biome-version.txt

echo "✅ Knowledge base updated!"
echo "Latest versions:"
echo "- Bun: $(cat ~/.claude/knowledge/tech-stacks/bun-version.txt)"
echo "- TypeScript: $(cat ~/.claude/knowledge/tech-stacks/typescript-version.txt)"  
echo "- Biome: $(cat ~/.claude/knowledge/tech-stacks/biome-version.txt)"
EOF

chmod +x ~/.claude/scripts/update-tech-knowledge.sh

echo "✅ Knowledge update system created!"
echo ""
echo "📚 Your knowledge base locations:"
echo "• ~/.claude/knowledge/tech-stacks/ - Current technology patterns"
echo "• project/.claude/knowledge/ - Project-specific knowledge"
echo "• ~/.claude/scripts/update-tech-knowledge.sh - Auto-updater"
echo ""
echo "🔄 Usage in Claude Code:"
echo "claude /update-knowledge"
echo "claude /tech-check"
echo ""
echo "💡 Before any implementation:"
echo "1. Run /tech-check to load current knowledge"
echo "2. Reference project .claude/knowledge files"
echo "3. Store new patterns in memory for consistency"
```