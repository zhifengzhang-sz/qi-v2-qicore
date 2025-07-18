# API Stability Enforcement Setup

This repository enforces API documentation stability to prevent accidental breaking changes. After api-ts-v-1.0, the API documentation represents the authoritative contract that implementations must follow.

## Automatic Validation

### Package Scripts

API validation is integrated into the standard development workflow:

```bash
# Run all checks including API validation
cd typescript && bun run check

# Run only API validation
cd typescript && bun run validate-api

# API validation runs automatically before publishing
bun run prepublishOnly
```

### Pre-commit Hook (Recommended)

Install the pre-commit hook to catch API breaking changes before commit:

```bash
# Install pre-commit hook
git config core.hooksPath .githooks

# Test the hook
cd typescript && git commit -m "test commit"
```

The hook will:
- ✅ Run API validation before every commit
- ❌ Block commits that break API consistency  
- 💡 Provide clear error messages and fix guidance
- 🚫 Can be bypassed with `git commit --no-verify` (not recommended)

## API Stability Rules

1. **API Documentation is Authoritative**: After api-ts-v-1.0, implementations must match documentation (not the reverse)

2. **100% Consistency Required**: All modules must maintain 100% API documentation consistency

3. **Breaking Changes Require Version Bump**: API breaking changes require explicit api-ts-v-2.0 version bump

4. **Implementation Changes OK**: Internal implementation improvements are allowed if they maintain API compatibility

## Validation Commands

Use these commands to check API consistency:

```bash
# Validate all modules
/validate-api-docs

# Validate specific module
/validate-api-docs result
/validate-api-docs error  
/validate-api-docs config
/validate-api-docs logger
/validate-api-docs cache
```

## Validation Reports

Check detailed validation reports in:
- `typescript/docs/api/reports/result.validation.md`
- `typescript/docs/api/reports/error.validation.md`
- `typescript/docs/api/reports/config.validation.md`
- `typescript/docs/api/reports/logger.validation.md`
- `typescript/docs/api/reports/cache.validation.md`

## Troubleshooting

### If API validation fails:

1. **Check the validation report** for specific issues
2. **Fix implementation** to match documented API (preferred)
3. **Update documentation** only if API change is intentional and approved
4. **Version bump** if breaking changes are required

### If you need to make breaking changes:

1. Get approval for API breaking change
2. Update version to api-ts-v-2.0
3. Update documentation first
4. Update implementation to match
5. Ensure all validation passes

## Benefits

- 🛡️ **Prevents accidental API breakage**
- 📋 **Maintains documentation accuracy**  
- 🔄 **Enforces implementation-documentation consistency**
- 🚀 **Enables confident refactoring**
- 📈 **Supports API evolution tracking**