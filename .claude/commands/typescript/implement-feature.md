# TypeScript Implement Feature

**INSTRUCTION FOR AI ASSISTANT**: Implement TypeScript features using current best practices and modern patterns.

## TypeScript Implementation Process

### 1. Load Current TypeScript Patterns (**filesystem** + **memory**)
- Apply modern TypeScript syntax and patterns from 2025 web search updates
- Use current Bun, Biome, Vitest best practices (latest 2025 versions)
- Follow project-specific TypeScript configuration in `typescript/` directory
- Reference QiCore behavioral contracts in `docs/contracts/`

### 2. Modern TypeScript Implementation (**sequential-thinking**)
**Use current TypeScript features:**
- **Type Safety**: Strict mode, noUncheckedIndexedAccess
- **Modern Syntax**: Satisfies operator, template literals
- **Utility Types**: Pick, Omit, Record, Partial, Required
- **Branded Types**: For type-safe IDs and values
- **Const Assertions**: For readonly data structures

### 3. Integration with Build Tools
**Bun Integration:**
- Use native TypeScript support
- Leverage Bun's fast bundling and testing
- Apply Bun-specific optimizations

**Biome Integration:**
- Follow configured linting rules
- Use Biome formatting standards
- Apply import organization

**Vitest Integration:**
- Use TypeScript-native testing
- Apply proper mocking patterns
- Use fast-check for property-based testing

### 4. Code Quality Standards
**TypeScript Best Practices:**
- Explicit return types for public functions
- Proper error handling patterns
- Modular architecture with clear interfaces
- Performance-conscious implementations

## Implementation Guidelines

### Error Handling Patterns
```typescript
// Use Result<T, E> pattern instead of exceptions
type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E }
```

### Type-Safe Patterns
```typescript
// Branded types for IDs
type UserId = string & { readonly brand: unique symbol }

// Template literal types
type EventName = `on${Capitalize<string>}`

// Const assertions
const config = {
  theme: 'dark',
  lang: 'en'
} as const satisfies Config
```

### Testing Integration
```typescript
// Property-based testing with fast-check
import { fc, test } from 'fast-check'
import { expect } from 'vitest'

test('property holds', () => {
  fc.assert(fc.property(
    fc.integer(),
    (n) => expect(operation(n)).toBeDefined()
  ))
})
```

## Success Criteria

**Implementation should:**
- Use modern TypeScript syntax and patterns
- Follow current Bun/Biome/Vitest best practices
- Maintain type safety with strict checking
- Include proper error handling
- Have comprehensive tests with fast-check
- Follow project conventions and architecture

**Ready for production TypeScript code using 2025 best practices.**