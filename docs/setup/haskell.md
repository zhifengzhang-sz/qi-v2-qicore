# QiCore Workspace Haskell Development Setup

This guide provides comprehensive setup instructions for Haskell development in the QiCore workspace using GHC 9.12 and modern Nix tooling.

## Overview

The QiCore workspace uses a standardized Haskell development environment across all projects:

- **GHC Version**: 9.12.1 (latest stable)
- **Language Edition**: GHC2024 (modern defaults)
- **Build System**: Cabal with Nix integration
- **Development Environment**: Nix flakes for reproducibility
- **Testing Framework**: Tasty + QuickCheck for comprehensive testing

## Prerequisites

### System Requirements

- **Operating System**: Linux, macOS, or Windows (WSL2)
- **Nix Package Manager**: Version 2.13+ with flakes enabled
- **Git**: For version control and flake inputs

### Install Nix (if not already installed)

```bash
# Install Nix with flakes enabled
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Or using the official installer with manual flakes configuration
sh <(curl -L https://nixos.org/nix/install) --daemon

# Enable flakes in ~/.config/nix/nix.conf
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

### Configure Binary Caches (Recommended)

Add to `~/.config/nix/nix.conf`:
```
substituters = https://cache.nixos.org/ https://haskell.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= haskell.cachix.org-1:pKhUSTKaF9kOJlKwLdABxDYJsXa6Nz8rMGxnZKZqlSA=
```

## Quick Start

### 1. Clone and Enter Development Environment

```bash
# Navigate to any Haskell project in the workspace
cd workspace/haskell/project-name

# Enter Nix development shell with all tools
nix develop

# Verify setup
ghc --version  # Should show GHC 9.12.1
cabal --version
haskell-language-server --version
```

### 2. Basic Development Workflow

```bash
# Build project
cabal build

# Run tests
cabal test

# Start fast feedback development
ghcid

# Format code
ormolu --mode inplace src/**/*.hs

# Lint code
hlint src/
```

## Standard Project Structure

All QiCore Haskell projects follow this standardized structure:

```
project-name/
├── flake.nix                    # Nix development environment
├── flake.lock                   # Locked dependency versions
├── cabal.project                # Multi-package configuration
├── project-name.cabal           # Package definition
├── src/                         # Source code
│   └── ProjectName/
│       └── Module.hs
├── test/                        # Test suite
│   └── Main.hs
├── docs/                        # Project documentation
├── scripts/                     # Development scripts
│   └── update-haskell-ecosystem.sh
└── README.md                    # Project overview
```

## flake.nix Template

Use this template for new Haskell projects:

```nix
{
  description = "QiCore [Project Name] - Haskell Implementation";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Standard GHC version for all QiCore projects
        ghcVersion = "ghc9121";
        hpkgs = pkgs.haskell.packages.${ghcVersion};
        
        # Standard development tools
        devTools = with pkgs; [
          # Core Haskell tools
          hpkgs.ghc
          hpkgs.cabal-install
          
          # Modern development tools
          hpkgs.haskell-language-server
          hpkgs.ghcid
          hpkgs.ormolu
          hpkgs.hlint
          
          # Nix and system tools
          nixpkgs-fmt
          git curl jq
        ];
        
      in {
        devShells.default = pkgs.mkShell {
          name = "project-name-dev";
          buildInputs = devTools;
          
          shellHook = ''
            echo "🚀 QiCore [Project Name] Development Environment"
            echo "📦 GHC Version: $(ghc --version)"
            echo "🔧 Ready for development!"
            echo ""
            echo "Available commands:"
            echo "  cabal build             - Build project"
            echo "  cabal test              - Run tests" 
            echo "  ghcid                   - Fast feedback compilation"
            echo "  ormolu --mode inplace **/*.hs - Format code"
            echo "  hlint src/              - Lint code"
            echo ""
          '';
        };
        
        packages.default = hpkgs.callCabal2nix "project-name" ./. {};
        formatter = pkgs.nixpkgs-fmt;
      });
}
```

## Cabal Configuration

### project-name.cabal Template

```cabal
cabal-version: 3.8
name: project-name
version: 0.1.0
synopsis: QiCore [Project Description]
description: Detailed description of the project
license: MIT
author: QiCore Team
maintainer: team@qicore.dev
category: Development
build-type: Simple
tested-with: GHC ==9.12.1

common shared-properties
  default-language: GHC2024
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

library
  import: shared-properties
  exposed-modules:
    ProjectName.Module
  build-depends:
    base >= 4.19 && < 5,
    text >= 2.0,
    aeson >= 2.2,
    containers >= 0.6
  hs-source-dirs: src

test-suite project-name-test
  import: shared-properties
  type: exitcode-stdio-1.0
  main-is: Main.hs
  build-depends:
    base,
    project-name,
    tasty >= 1.5,
    tasty-quickcheck >= 0.10,
    QuickCheck >= 2.15
  hs-source-dirs: test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

### cabal.project Template

```cabal
packages: .

-- Use GHC 9.12.1 optimizations
optimization: 2

-- Enable all warnings for development
package *
  documentation: True
  haddock-all: True

-- Test configuration
tests: True
test-show-details: direct

-- Documentation
documentation: True
doc-index-file: $datadir/doc/index.html
```

## IDE Integration

### VS Code Setup

1. **Install Extensions**:
   - [Haskell Language Extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
   - [Nix Extension](https://marketplace.visualstudio.com/items?itemName=bbenoist.Nix)

2. **Configure Workspace Settings** (`.vscode/settings.json`):
```json
{
  "haskell.manageHLS": "PATH",
  "haskell.serverExecutablePath": "haskell-language-server",
  "haskell.trace.server": "messages",
  "files.watcherExclude": {
    "**/dist-newstyle/**": true,
    "**/.stack-work/**": true
  }
}
```

### Vim/Neovim Setup

Use with `direnv` and `nix-direnv`:

```bash
# Install direnv
nix profile install nixpkgs#direnv

# Add to shell configuration
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc  # or ~/.zshrc

# Create .envrc in project root
echo "use flake" > .envrc
direnv allow
```

## Development Workflow

### Daily Development

```bash
# Start development session
cd project-directory
nix develop

# Fast feedback development
ghcid --command="cabal repl" --test="main"

# Run specific tests
cabal test project-name-test --test-show-details=direct

# Format and lint before commit
ormolu --mode inplace src/**/*.hs test/**/*.hs
hlint src/ test/
```

### Quality Assurance

```bash
# Comprehensive testing
cabal test --test-options="--quickcheck-tests=1000"

# Generate documentation
cabal haddock --enable-doc-index

# Performance testing
cabal bench

# Check for unused dependencies
cabal-plan tred --full | dot -Tpng -o deps.png
```

### Ecosystem Updates

```bash
# Run the update script
./scripts/update-haskell-ecosystem.sh --check

# Update flake inputs
nix flake update

# Test with updated dependencies
cabal build --enable-tests
cabal test
```

## Troubleshooting

### Common Issues

#### 1. GHC Version Mismatch
```bash
# Check current GHC version
nix develop --command ghc --version

# Rebuild development environment
nix develop --rebuild
```

#### 2. Package Not Found
```bash
# Update flake inputs
nix flake update

# Clear cabal cache
cabal clean
rm -rf dist-newstyle/
```

#### 3. Build Failures
```bash
# Check for missing system dependencies
nix develop --command cabal configure --enable-tests

# Verbose build for debugging
cabal build --verbose=3
```

#### 4. HLS Not Working
```bash
# Restart HLS
pkill haskell-language-server

# Check HLS version compatibility
haskell-language-server --version
ghc --version
```

### Performance Optimization

#### Build Performance
```bash
# Enable parallel builds
echo "jobs: $nproc" >> ~/.config/cabal/config

# Use faster linker (if available)
echo "ghc-options: -fuse-ld=gold" >> cabal.project.local
```

#### Development Environment
```bash
# Pre-download dependencies
nix develop --command cabal update
nix develop --command cabal build --dependencies-only

# Use build cache
cachix use haskell
```

## Project Templates

### Create New Project

```bash
# Create project structure
mkdir my-project && cd my-project

# Copy templates
cp ../existing-project/flake.nix .
cp ../existing-project/cabal.project .

# Customize for new project
sed -i 's/existing-project/my-project/g' flake.nix
sed -i 's/ExistingProject/MyProject/g' *.cabal

# Initialize
nix develop
cabal configure
```

### Testing Setup

Standard test setup for all projects:

```haskell
-- test/Main.hs
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree  
properties = testGroup "Properties" 
  [ QC.testProperty "Example property" $ \x ->
      length (reverse (x :: [Int])) == length x
  ]
```

## Continuous Integration

### GitHub Actions Template

```yaml
name: Haskell CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - uses: DeterminateSystems/nix-installer-action@main
    - uses: DeterminateSystems/magic-nix-cache-action@main
    
    - name: Build
      run: nix develop --command cabal build
      
    - name: Test
      run: nix develop --command cabal test --test-show-details=direct
      
    - name: Documentation
      run: nix develop --command cabal haddock
```

## Best Practices

### Code Organization
- Use qualified imports to avoid name conflicts
- Follow consistent module naming: `ProjectName.Component.Module`
- Keep modules focused and cohesive
- Use `LANGUAGE` pragmas at the top of files

### Testing Strategy
- Write property-based tests for mathematical properties
- Use unit tests for specific behavior verification
- Test edge cases and error conditions
- Maintain test coverage above 80%

### Documentation
- Write Haddock documentation for all public functions
- Include examples in documentation
- Keep README updated with setup instructions
- Document architectural decisions

### Performance
- Use strict data types where appropriate
- Profile code with `-prof` flag when needed
- Optimize hot paths identified by profiling
- Use `INLINE` pragmas judiciously

## Resources

### Official Documentation
- [GHC User Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/)
- [Cabal User Guide](https://cabal.readthedocs.io/)
- [Nix Manual](https://nixos.org/manual/nix/stable/)

### QiCore Specific
- [Behavioral Contracts](../contracts/qi.base.contracts.md)
- [Testing Guide](../haskell/docs/TESTING_GUIDE_2025.md)
- [GHC Version Notes](../haskell/docs/GHC_VERSION_NOTES.md)

### Community Resources
- [Haskell Discourse](https://discourse.haskell.org/)
- [NixOS Discourse](https://discourse.nixos.org/)
- [Haskell Reddit](https://www.reddit.com/r/haskell/)

---

**Last Updated**: July 13, 2025  
**Maintained by**: QiCore Development Team  
**Next Review**: October 2025