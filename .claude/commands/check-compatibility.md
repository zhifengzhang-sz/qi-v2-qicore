# Check GHC Compatibility

Check GHC 9.12.1 ecosystem compatibility status for development tools.

This command monitors the compatibility of advanced development tools with our cutting-edge GHC 9.12.1 setup. Due to ecosystem lag, some tools like HLS, ghcid, ormolu, and hlint are temporarily incompatible.

## What This Checks

1. **Tool Versions**: Latest releases of incompatible tools from GitHub
2. **Compatibility Status**: Current working vs incompatible tool status  
3. **Environment Health**: Verify GHC 9.12.1 and build system working
4. **Nix Ecosystem**: Check nixpkgs availability and package status

## Key Tools Monitored

### Currently Incompatible (Ecosystem Lag)
- **haskell-language-server**: Build failures with GHC 9.12.1
- **ghcid**: Dependency compatibility issues
- **ormolu**: Version bound problems  
- **hlint**: Compatibility lag

### Working Tools
- **GHC 9.12.1**: Full support via nixpkgs-unstable
- **Cabal**: All operations working
- **QuickCheck/Tasty**: Testing framework fully functional
- **Core Dependencies**: aeson, text, containers, time

## Usage Scenarios

- **Monthly Monitoring**: Check if incompatible tools gained GHC 9.12.1 support
- **Environment Issues**: Verify current setup is working correctly
- **Tool Updates**: See if new tool versions are available
- **Documentation Updates**: Generate reports for updating compatibility docs

## Generated Reports

Creates timestamped compatibility reports with:
- Latest tool version information
- Current compatibility status
- Environment verification results
- Next action recommendations

!./scripts/check-ghc-compatibility.sh $ARGUMENTS