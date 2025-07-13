# Build QiBase Library

Build the qi/base component with GHC 9.12.1 and comprehensive warnings.

This command builds the QiCore Foundation mathematical types (Result<T> and QiError) using modern Haskell with cutting-edge compiler features and strict quality settings.

## What This Builds

- **qi/base Library**: Mathematical foundation types
  - `Qi.Base.Result`: Category theory-based Result<T> with 60+ operations
  - `Qi.Base.Error`: QiError with 14 ErrorCategory values and error chaining

## Build Configuration

- **Compiler**: GHC 9.12.1 with GHC2024 language edition
- **Environment**: Nix development shell with reproducible dependencies
- **Warnings**: Comprehensive warning set enabled (-Wall, -Wcompat, etc.)
- **Extensions**: Modern Haskell extensions (OverloadedStrings, StrictData, etc.)

## Quality Checks

### Compilation Standards
- **Zero Warnings**: All code must compile cleanly
- **Type Safety**: Strict type checking with modern extensions
- **Performance**: Optimized compilation settings
- **Documentation**: Haddock documentation generation ready

### Dependencies Verified
- **base >= 4.19**: GHC 9.12.1 compatible base library
- **aeson 2.2+**: Modern JSON with KeyMap support
- **text >= 2.0**: Modern Text implementation
- **containers**: Map for error context storage
- **time**: UTCTime for error timestamps

## Build Outputs

Successful build produces:
- Compiled qi-base library ready for use
- Type-checked implementations with zero fake code
- Haddock-ready documentation
- Package ready for testing and distribution

## Common Build Issues

### Dependency Problems
- **GHC Version**: Ensure using GHC 9.12.1 via `nix develop`
- **Package Bounds**: May need jailbreak for bleeding-edge compatibility
- **Cabal Cache**: Clear with `cabal clean` if needed

### Environment Issues
- **Nix Setup**: Verify flake.nix and development shell
- **Path Issues**: Ensure in haskell/ directory for builds

## Usage

Run this command to:
- Verify code compiles correctly after changes
- Check for type errors and warnings
- Prepare for testing or documentation generation
- Validate dependency compatibility

!cd haskell && nix develop --command cabal build qi-base