#!/usr/bin/env bash

# QiCore Haskell Project Setup Script
# Creates a new Haskell project following QiCore workspace standards

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Project configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"
TEMPLATE_DIR="$WORKSPACE_ROOT/haskell"  # Use existing haskell project as template

# Logging functions
log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# Show usage information
show_usage() {
    cat << EOF
Usage: $0 <project-name> [OPTIONS]

Create a new Haskell project following QiCore workspace standards.

Arguments:
  project-name    Name of the new project (e.g., 'qi-new-component')

Options:
  --type TYPE     Project type: 'library' (default), 'executable', 'service'
  --description   Project description
  --author        Author name (default: 'QiCore Team')
  --help          Show this help message

Examples:
  $0 qi-analytics                           # Create library project
  $0 qi-cli --type executable               # Create executable project
  $0 qi-server --type service --author "John Doe"

The script will:
1. Create project directory structure
2. Generate flake.nix with GHC 9.12 configuration
3. Create cabal file with QiCore standards
4. Set up test framework with Tasty + QuickCheck
5. Configure development environment
EOF
}

# Validate project name
validate_project_name() {
    local name="$1"
    
    if [[ ! "$name" =~ ^[a-z][a-z0-9-]*$ ]]; then
        error "Invalid project name: $name"
        error "Project name must start with lowercase letter and contain only lowercase letters, numbers, and hyphens"
        exit 1
    fi
    
    if [ ${#name} -lt 3 ]; then
        error "Project name must be at least 3 characters long"
        exit 1
    fi
    
    if [ ${#name} -gt 50 ]; then
        error "Project name must be less than 50 characters long"
        exit 1
    fi
}

# Convert kebab-case to PascalCase for module names
to_pascal_case() {
    echo "$1" | sed -r 's/(^|-)([a-z])/\U\2/g'
}

# Convert kebab-case to snake_case for internal use
to_snake_case() {
    echo "$1" | tr '-' '_'
}

# Create project directory structure
create_project_structure() {
    local project_name="$1"
    local project_type="$2"
    
    log "Creating project structure for $project_name..."
    
    # Create main directories
    mkdir -p "$project_name"/{src,test,docs,scripts}
    
    # Create module directories
    local module_name=$(to_pascal_case "$project_name")
    mkdir -p "$project_name/src/$module_name"
    
    success "Created project directory structure"
}

# Generate flake.nix
generate_flake() {
    local project_name="$1"
    local description="$2"
    local project_dir="$project_name"
    
    log "Generating flake.nix..."
    
    cat > "$project_dir/flake.nix" << EOF
{
  description = "QiCore $description - Haskell Implementation with GHC 9.12";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Optional: flake-compat for legacy nix support
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.\${system};
        
        # Use GHC 9.12.1 - QiCore workspace standard
        ghcVersion = "ghc9121";
        hpkgs = pkgs.haskell.packages.\${ghcVersion};
        
        # Package-specific overrides if needed
        haskellPackages = hpkgs.override {
          overrides = hself: hsuper: {
            # Example: Override specific packages if needed
            # $project_name = hself.callCabal2nix "$project_name" ./. {};
          };
        };
        
        # Development tools following QiCore 2025 standards
        devTools = with pkgs; [
          # Core Haskell tools
          haskellPackages.ghc
          haskellPackages.cabal-install
          
          # Modern development tools
          haskellPackages.haskell-language-server  # HLS for IDE support
          haskellPackages.ghcid                    # Fast feedback
          haskellPackages.ormolu                   # Code formatting
          haskellPackages.hlint                    # Linting
          
          # Nix and system tools
          nixpkgs-fmt    # Nix formatting
          git curl jq    # Basic utilities
        ];
        
      in {
        # Development shell with comprehensive tooling
        devShells.default = pkgs.mkShell {
          name = "$project_name-dev";
          buildInputs = devTools;
          
          shellHook = ''
            echo "🚀 QiCore $description Development Environment"
            echo "📦 GHC Version: \$(ghc --version)"
            echo "🔧 Available commands:"
            echo "  cabal build $project_name         - Build library"
            echo "  cabal test                        - Run all tests"
            echo "  cabal test $project_name-test     - Run project tests"
            echo "  ghcid                            - Fast feedback compilation"
            echo "  ormolu --mode inplace **/*.hs    - Format Haskell code"
            echo "  hlint src/                       - Lint source code"
            echo ""
            echo "📚 Quality Commands:"
            echo "  cabal test --test-options='--quickcheck-tests=1000'  - Extended testing"
            echo "  cabal haddock                    - Generate documentation"
            echo ""
          '';
        };
        
        # CI/CD shell
        devShells.ci = pkgs.mkShell {
          name = "$project_name-ci";
          buildInputs = [
            haskellPackages.ghc
            haskellPackages.cabal-install
            pkgs.git
          ];
        };
        
        # Package outputs
        packages = {
          default = haskellPackages.callCabal2nix "$project_name" ./. {};
          $project_name = haskellPackages.callCabal2nix "$project_name" ./. {};
        };
        
        # Formatter for the flake itself
        formatter = pkgs.nixpkgs-fmt;
        
        # Checks for CI/CD
        checks = {
          # Build the package
          $project_name-build = self.packages.\${system}.$project_name;
          
          # Run tests
          $project_name-test = pkgs.runCommand "$project_name-test"
            { buildInputs = [ haskellPackages.ghc haskellPackages.cabal-install ]; }
            ''
              cd \${./.}
              cabal test $project_name-test
              touch \$out
            '';
        };
      });
  
  # Nix compatibility configuration
  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://haskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "haskell.cachix.org-1:pKhUSTKaF9kOJlKwLdABxDYJsXa6Nz8rMGxnZKZqlSA="
    ];
  };
}
EOF

    success "Generated flake.nix"
}

# Generate cabal file
generate_cabal_file() {
    local project_name="$1"
    local project_type="$2"
    local description="$3"
    local author="$4"
    local project_dir="$project_name"
    local module_name=$(to_pascal_case "$project_name")
    
    log "Generating $project_name.cabal..."
    
    cat > "$project_dir/$project_name.cabal" << EOF
cabal-version: 3.8
name: $project_name
version: 0.1.0
synopsis: QiCore $description
description: 
  $description implementation following QiCore Foundation patterns
  and mathematical principles.
license: MIT
author: $author
maintainer: team@qicore.dev
category: Development
build-type: Simple
tested-with: GHC ==9.12.1

extra-source-files:
  README.md
  CHANGELOG.md

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
    -Wunused-packages

library
  import: shared-properties
  exposed-modules:
    $module_name
  other-modules:
    $module_name.Internal
  build-depends:
    base >= 4.19 && < 5,
    text >= 2.0,
    containers >= 0.6,
    aeson >= 2.2,
    time >= 1.12
  hs-source-dirs: src
  default-extensions:
    OverloadedStrings
    DeriveGeneric
    DerivingStrategies
    GeneralizedNewtypeDeriving

test-suite $project_name-test
  import: shared-properties
  type: exitcode-stdio-1.0
  main-is: Main.hs
  other-modules:
    Test.$module_name.Properties
    Test.$module_name.Unit
  build-depends:
    base,
    $project_name,
    tasty >= 1.5,
    tasty-quickcheck >= 0.10,
    tasty-hunit >= 0.10,
    QuickCheck >= 2.15,
    text,
    containers
  hs-source-dirs: test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  default-extensions:
    OverloadedStrings
EOF

    # Add executable section if needed
    if [ "$project_type" = "executable" ] || [ "$project_type" = "service" ]; then
        cat >> "$project_dir/$project_name.cabal" << EOF

executable $project_name
  import: shared-properties
  main-is: Main.hs
  other-modules:
    $module_name.CLI
  build-depends:
    base,
    $project_name,
    optparse-applicative >= 0.17
  hs-source-dirs: app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  default-extensions:
    OverloadedStrings
EOF
        
        # Create app directory for executable
        mkdir -p "$project_dir/app"
    fi

    success "Generated $project_name.cabal"
}

# Generate source files
generate_source_files() {
    local project_name="$1"
    local project_type="$2"
    local description="$3"
    local project_dir="$project_name"
    local module_name=$(to_pascal_case "$project_name")
    
    log "Generating source files..."
    
    # Main module
    cat > "$project_dir/src/$module_name.hs" << EOF
{-|
Module      : $module_name
Description : $description
Copyright   : (c) QiCore Team, $(date +%Y)
License     : MIT
Maintainer  : team@qicore.dev

$description implementation following QiCore Foundation patterns.
-}

module $module_name
  ( -- * Core Types
    -- | Main types and data structures
    
    -- * Operations
    -- | Primary operations and functions
    
    -- * Utilities
    -- | Helper functions and utilities
    
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Example function demonstrating QiCore patterns
-- 
-- >>> example "hello"
-- "Hello, world!"
example :: Text -> Text
example input = "Hello, " <> input <> "!"
EOF

    # Internal module
    cat > "$project_dir/src/$module_name/Internal.hs" << EOF
{-|
Module      : $module_name.Internal
Description : Internal implementation details
Copyright   : (c) QiCore Team, $(date +%Y)
License     : MIT
Maintainer  : team@qicore.dev

Internal implementation details for $module_name.
This module is not part of the public API and may change without notice.
-}

module $module_name.Internal where

import Data.Text (Text)

-- | Internal helper function
internalHelper :: Text -> Text
internalHelper = id
EOF

    success "Generated source files"
}

# Generate test files
generate_test_files() {
    local project_name="$1"
    local project_dir="$project_name"
    local module_name=$(to_pascal_case "$project_name")
    
    log "Generating test files..."
    
    # Create test directories
    mkdir -p "$project_dir/test/Test/$module_name"
    
    # Main test file
    cat > "$project_dir/test/Main.hs" << EOF
{-|
Module      : Main
Description : Test suite for $module_name
Copyright   : (c) QiCore Team, $(date +%Y)
License     : MIT

Comprehensive test suite following QiCore testing standards.
Uses Tasty + QuickCheck for property-based testing.
-}

import Test.Tasty
import qualified Test.$module_name.Properties as Properties
import qualified Test.$module_name.Unit as Unit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "$module_name Tests"
  [ Properties.tests
  , Unit.tests
  ]
EOF

    # Property-based tests
    cat > "$project_dir/test/Test/$module_name/Properties.hs" << EOF
{-|
Module      : Test.$module_name.Properties
Description : Property-based tests for $module_name
Copyright   : (c) QiCore Team, $(date +%Y)
License     : MIT

Property-based tests using QuickCheck to verify mathematical properties
and behavioral contracts.
-}

module Test.$module_name.Properties (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import $module_name
import Data.Text (Text)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Property Tests"
  [ testGroup "Core Properties"
    [ QC.testProperty "example preserves input length" prop_exampleLength
    , QC.testProperty "example is idempotent on result" prop_exampleIdempotent
    ]
  ]

-- | Property: example function preserves essential input characteristics
prop_exampleLength :: Text -> Property
prop_exampleLength input = 
  not (T.null input) ==> 
    T.length (example input) > T.length input

-- | Property: applying example to result of example is deterministic
prop_exampleIdempotent :: Text -> Bool
prop_exampleIdempotent input = 
  let result = example input
      twice = example result
  in result /= twice  -- Not actually idempotent, just an example
EOF

    # Unit tests
    cat > "$project_dir/test/Test/$module_name/Unit.hs" << EOF
{-|
Module      : Test.$module_name.Unit
Description : Unit tests for $module_name
Copyright   : (c) QiCore Team, $(date +%Y)
License     : MIT

Unit tests for specific behaviors and edge cases.
-}

module Test.$module_name.Unit (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import $module_name
import Data.Text (Text)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Unit Tests"
  [ testGroup "Basic Functionality"
    [ testCase "example with simple input" test_exampleSimple
    , testCase "example with empty input" test_exampleEmpty
    ]
  ]

test_exampleSimple :: Assertion
test_exampleSimple = do
  let result = example "world"
  result @?= "Hello, world!"

test_exampleEmpty :: Assertion  
test_exampleEmpty = do
  let result = example ""
  result @?= "Hello, !"
EOF

    success "Generated test files"
}

# Generate additional project files
generate_additional_files() {
    local project_name="$1"
    local description="$2"
    local project_dir="$project_name"
    
    log "Generating additional project files..."
    
    # README.md
    cat > "$project_dir/README.md" << EOF
# $project_name

$description

## Overview

This project follows QiCore Foundation patterns and is built with:

- **GHC 9.12.1** with GHC2024 language edition
- **Nix flakes** for reproducible development environment
- **Tasty + QuickCheck** for comprehensive testing
- **Modern Haskell** patterns and best practices

## Quick Start

\`\`\`bash
# Enter development environment
nix develop

# Build the project
cabal build

# Run tests
cabal test

# Generate documentation
cabal haddock
\`\`\`

## Development

### Prerequisites

- Nix with flakes enabled
- Git

### Development Workflow

\`\`\`bash
# Fast feedback development
ghcid

# Format code
ormolu --mode inplace src/**/*.hs test/**/*.hs

# Lint code
hlint src/ test/

# Extended testing
cabal test --test-options="--quickcheck-tests=1000"
\`\`\`

### Project Structure

\`\`\`
$project_name/
├── src/                     # Source code
├── test/                    # Test suite
├── docs/                    # Documentation
├── scripts/                 # Development scripts
├── flake.nix               # Nix development environment
└── $project_name.cabal     # Package definition
\`\`\`

## Documentation

- [Setup Guide](../../docs/setup/haskell.md)
- [Testing Guide](../../docs/haskell/TESTING_GUIDE_2025.md)
- [Behavioral Contracts](../../docs/contracts/)

## License

MIT License - see LICENSE file for details.
EOF

    # CHANGELOG.md
    cat > "$project_dir/CHANGELOG.md" << EOF
# Changelog

All notable changes to $project_name will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project setup with QiCore Foundation patterns
- GHC 9.12.1 and GHC2024 language edition support
- Comprehensive test suite with Tasty + QuickCheck
- Nix flakes development environment
- Modern Haskell tooling integration

## [0.1.0] - $(date +%Y-%m-%d)

### Added
- Initial release
- Core module structure
- Basic functionality
- Test suite foundation
EOF

    # cabal.project
    cat > "$project_dir/cabal.project" << EOF
packages: .

-- Optimization settings
optimization: 2

-- Documentation
documentation: True
doc-index-file: \$datadir/doc/index.html

-- Test configuration
tests: True
test-show-details: direct

-- Package-specific configurations
package *
  documentation: True
  haddock-all: True

-- QiCore specific settings
package $project_name
  ghc-options: -Werror
EOF

    # .gitignore
    cat > "$project_dir/.gitignore" << EOF
# Haskell
dist-newstyle/
.ghc.environment.*
*.hi
*.o
*.prof
*.aux
*.hp
*.eventlog
*.tix
.HTF/
.hsenv
.cabal-sandbox/
cabal.sandbox.config
*.prof
*.aux
*.hp
*.eventlog
.stack-work/
cabal.project.local
cabal.project.local~

# Nix
result
result-*

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Temporary files
*.tmp
*.temp
EOF

    # Development script
    cp "$WORKSPACE_ROOT/haskell/scripts/update-haskell-ecosystem.sh" "$project_dir/scripts/" 2>/dev/null || {
        mkdir -p "$project_dir/scripts"
        echo "#!/bin/bash" > "$project_dir/scripts/update-haskell-ecosystem.sh"
        echo "# TODO: Copy update script from workspace template" >> "$project_dir/scripts/update-haskell-ecosystem.sh"
        chmod +x "$project_dir/scripts/update-haskell-ecosystem.sh"
    }

    success "Generated additional project files"
}

# Initialize project
initialize_project() {
    local project_name="$1"
    local project_dir="$project_name"
    
    log "Initializing project..."
    
    cd "$project_dir"
    
    # Initialize git repository if not already in one
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        git init
        git add .
        git commit -m "Initial commit: $project_name project setup

- Generated with QiCore Haskell project template
- GHC 9.12.1 with GHC2024 language edition
- Nix flakes development environment
- Tasty + QuickCheck testing framework
- Modern Haskell tooling integration"
        success "Initialized git repository"
    fi
    
    # Test nix environment
    if timeout 60 nix develop --command echo "✅ Nix flake evaluation successful"; then
        success "Nix flake environment verified"
    else
        warning "Nix flake environment test failed - please check manually"
    fi
    
    cd ..
}

# Main function
main() {
    local project_name=""
    local project_type="library"
    local description=""
    local author="QiCore Team"
    
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --type)
                project_type="$2"
                shift 2
                ;;
            --description)
                description="$2"
                shift 2
                ;;
            --author)
                author="$2"
                shift 2
                ;;
            --help)
                show_usage
                exit 0
                ;;
            -*)
                error "Unknown option: $1"
                show_usage
                exit 1
                ;;
            *)
                if [ -z "$project_name" ]; then
                    project_name="$1"
                else
                    error "Multiple project names provided: $project_name and $1"
                    exit 1
                fi
                shift
                ;;
        esac
    done
    
    # Validate arguments
    if [ -z "$project_name" ]; then
        error "Project name is required"
        show_usage
        exit 1
    fi
    
    validate_project_name "$project_name"
    
    # Set default description if not provided
    if [ -z "$description" ]; then
        case $project_type in
            library) description="$project_name Library Component" ;;
            executable) description="$project_name Command Line Tool" ;;
            service) description="$project_name Service Application" ;;
            *) description="$project_name Component" ;;
        esac
    fi
    
    # Check if project already exists
    if [ -d "$project_name" ]; then
        error "Directory $project_name already exists"
        exit 1
    fi
    
    echo "🚀 Creating QiCore Haskell Project"
    echo "=================================="
    echo "Project Name: $project_name"
    echo "Project Type: $project_type"
    echo "Description:  $description"
    echo "Author:       $author"
    echo ""
    
    # Create project
    create_project_structure "$project_name" "$project_type"
    generate_flake "$project_name" "$description"
    generate_cabal_file "$project_name" "$project_type" "$description" "$author"
    generate_source_files "$project_name" "$project_type" "$description"
    generate_test_files "$project_name"
    generate_additional_files "$project_name" "$description"
    initialize_project "$project_name"
    
    echo ""
    success "✅ Project $project_name created successfully!"
    echo ""
    echo "Next steps:"
    echo "  cd $project_name"
    echo "  nix develop"
    echo "  cabal build"
    echo "  cabal test"
    echo ""
    echo "Happy coding! 🎉"
}

# Run main function
main "$@"