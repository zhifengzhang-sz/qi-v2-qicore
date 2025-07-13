# QiCore Haskell Nix Development Setup - 2025 Edition

This document provides comprehensive guidance for setting up the QiCore Haskell development environment using modern Nix flakes and GHC 9.12.

## Overview

The QiCore Haskell implementation uses:
- **GHC 9.12.1**: Latest stable release with modern Haskell features
- **Nix Flakes**: Reproducible development environments
- **Cabal 3.14**: Modern Haskell package management
- **nixpkgs-unstable**: Access to latest GHC 9.12 packages

## Prerequisites

### Nix Installation

Ensure you have Nix with flakes enabled:

```bash
# Check Nix version (should be >= 2.4)
nix --version

# Enable flakes in nix.conf
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

### Git Configuration

The project requires Git for version control:
```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

## Flake Configuration

### Core Flake Structure

Our `flake.nix` uses the modern nixpkgs approach with GHC 9.12:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";  # GHC 9.12 support
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc9121;  # GHC 9.12.1
      in {
        # Development shell and package definitions
      });
}
```

### Key Design Decisions

1. **nixpkgs-unstable**: Required for GHC 9.12.1 support
2. **haskellPackages.shellFor**: Proper Haskell development environment
3. **callCabal2nix**: Automatic dependency resolution from .cabal files
4. **Minimal tooling**: Only essential tools to avoid version conflicts

## Development Environment

### Entering the Development Shell

```bash
# Enter the Nix development shell
cd haskell
nix develop

# You should see:
# 🚀 QiCore Base Component Development Environment
# 📦 GHC Version: The Glorious Glasgow Haskell Compilation System, version 9.12.1
```

### Available Tools

Inside the development shell:

```bash
# Core Haskell tools
ghc --version          # GHC 9.12.1
cabal --version        # Cabal 3.14.x

# Development tools  
ghcid                  # Fast reload development (when available)

# System tools
git                    # Version control
curl                   # HTTP client
jq                     # JSON processing
```

### Common Development Commands

```bash
# Build the library
cabal build qi-base

# Run tests
cabal test qi-base-test

# Run tests with detailed output
cabal test qi-base-test --test-show-details=direct

# Clean build artifacts
cabal clean

# Generate documentation (when needed)
cabal haddock qi-base
```

## GHC 9.12.1 Features

### Key Improvements

GHC 9.12.1 includes several improvements relevant to QiCore:

1. **Better Error Messages**: Enhanced type error reporting
2. **Performance**: Improved compilation and runtime performance
3. **Language Extensions**: Support for latest Haskell language features
4. **Compatibility**: Better ecosystem compatibility

### Language Extensions Used

Our project uses these modern extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}     -- String literals
{-# LANGUAGE TypeApplications #-}      -- Type applications
{-# LANGUAGE DerivingStrategies #-}    -- Explicit deriving strategies
{-# LANGUAGE DeriveGeneric #-}         -- Generic deriving
{-# LANGUAGE DeriveAnyClass #-}        -- Anyclass deriving
```

## Dependency Management

### Cabal Configuration

Our `qi-base.cabal` file specifies:

```cabal
cabal-version: 3.0
tested-with: GHC ==9.12.1

common shared-properties
  default-language: Haskell2010
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wmissing-home-modules
    -Wpartial-fields
    -Wredundant-constraints
```

### Package Set Management

We use the nixpkgs Haskell package set based on:
- **Stackage LTS**: Stable, tested package combinations
- **Hackage Latest**: Latest versions where appropriate
- **Manual Overrides**: Only when necessary for compatibility

### Handling Broken Packages

If packages are broken in the current package set:

1. **Check Compatibility**: Verify GHC 9.12 support
2. **Version Bounds**: Adjust cabal file constraints if needed
3. **Override in Flake**: Use `pkgs.haskell.lib` overrides sparingly
4. **Upstream Fixes**: Contribute fixes to nixpkgs when possible

## Best Practices

### Reproducible Builds

1. **Pin nixpkgs**: Always specify exact nixpkgs revision
2. **Lock File**: Commit `flake.lock` for reproducibility
3. **CI Consistency**: Use same Nix setup in CI/CD

### Development Workflow

```bash
# Start development session
nix develop

# Fast feedback loop
ghcid --command="cabal repl qi-base"

# Run tests continuously
cabal test --test-show-details=streaming

# Exit and re-enter shell for dependency changes
exit
nix develop
```

### Performance Optimization

1. **Binary Cache**: Use cachix for faster builds
2. **Parallel Builds**: Enable parallel building in Nix
3. **Incremental**: Use cabal's incremental compilation

## Troubleshooting

### Common Issues

#### GHC Version Mismatch
```bash
# Verify GHC version in shell
nix develop --command ghc --version

# Should show: version 9.12.1
```

#### Missing Dependencies
```bash
# Regenerate cabal files
cabal configure

# Clean and rebuild
cabal clean && cabal build
```

#### Flake Evaluation Errors
```bash
# Check flake validity
nix flake check

# Show flake info
nix flake show
```

### Build Cache

To speed up builds, consider using the Nix binary cache:

```bash
# Add to ~/.config/nix/nix.conf
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

## Future Considerations

### Upgrading GHC

When newer GHC versions become available:

1. **Check nixpkgs**: Verify availability in nixpkgs
2. **Test Compatibility**: Ensure our code compiles
3. **Update Flake**: Change `ghc9121` to newer version
4. **Update Documentation**: Update this guide

### Alternative Approaches

- **haskell.nix**: For more complex multi-package projects
- **Stack + Nix**: If Stack integration is needed
- **DevContainer**: For consistent containerized development

## Monitoring Updates

Stay informed about:
- **GHC Releases**: New versions and features
- **nixpkgs Updates**: Package availability and updates
- **Cabal Changes**: New features and best practices
- **Ecosystem Tools**: New development tools and integrations

This setup provides a robust, reproducible development environment for QiCore Haskell development using the latest tools and best practices available in 2025.