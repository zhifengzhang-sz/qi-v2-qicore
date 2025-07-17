# QiCore Haskell Implementation Issues

## Contract Compliance Issue: qi/core Dependency on Removed Operations

**Problem**: qi/core uses `Result.sequence` operation that has been removed from qi/base contracts.

**Details**:
- File: `haskell/src/qi/core/Qi/Core/Config.hs`, line 263
- Code: `envPairs <- Result.sequence (map parseEnvLine envLines)`
- The base contracts no longer specify `sequence` operation (sections 4-6 removed)

**Required Fix**:
Either:
1. Remove `sequence` from qi/base exports and implement local sequence in qi/core
2. Rewrite Config.hs to use manual folding instead of `Result.sequence`

**Impact**: 
- Haskell qi/base currently exports operations not in contracts
- qi/core depends on non-contracted operations
- Contract compliance violation