{
  description = "QiCore Crypto MCP Platform - Mathematical Actor System in Haskell";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # Use latest GHC 9.10 with modern Haskell features (2024-2025)
          basePackages = pkgs.haskell.packages.ghc910;
          
          # Package overrides and dependencies
          packages = {
            # Core dependencies for our project
            aeson.source = "2.2.1.0";
            servant.source = "0.20.1";
            servant-server.source = "0.20";
            stm.source = "2.5.2.1";
            wreq.source = "0.5.4.3";
            http-client.source = "0.7.17";
            containers.source = "0.6.8";
            time.source = "1.12.2";
            text.source = "2.0.2";
            warp.source = "3.3.30";
          };
          
          # Development shell configuration
          devShell = {
            hlsCheck.enable = true;  # Enable Haskell Language Server
            tools = hp: {
              # Development tools
              cabal-install = hp.cabal-install;
              ghcid = hp.ghcid;
              haskell-language-server = hp.haskell-language-server;
              hlint = hp.hlint;
              ormolu = hp.ormolu;  # Code formatter
              retrie = hp.retrie;  # Refactoring tool
            };
            
            # Additional development packages
            extraLibraries = hp: {
              # Test frameworks
              hspec = hp.hspec;
              QuickCheck = hp.QuickCheck;
              tasty = hp.tasty;
              tasty-hspec = hp.tasty-hspec;
              tasty-quickcheck = hp.tasty-quickcheck;
            };
          };
          
          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ];
        };
        
        # Custom development shell with additional tools
        devShells.default = pkgs.mkShell {
          name = "qi-crypto-mcp-dev-shell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            # System development tools
            git
            curl
            jq
            
            # Nix tools
            nixpkgs-fmt
            nix-tree
            
            # Optional: Database tools for development
            postgresql
            redis
            
            # Optional: Network tools
            netcat
            socat
          ];
          
          shellHook = ''
            echo "🚀 QiCore Crypto MCP Development Environment"
            echo "📦 GHC Version: $(ghc --version)"
            echo "🔧 Available commands:"
            echo "  cabal build     - Build all packages"
            echo "  cabal test      - Run tests"
            echo "  cabal run       - Run the MCP server"
            echo "  ghcid           - Auto-reload development"
            echo "  hlint .         - Lint the code"
            echo "  ormolu --mode inplace **/*.hs - Format code"
            echo ""
          '';
        };
        
        # Applications that can be run with 'nix run'
        apps = {
          default = {
            type = "app";
            program = "${self'.packages.qi-crypto-mcp-server}/bin/qi-crypto-mcp-server";
          };
        };
        
        # Formatter for nix files
        formatter = pkgs.nixpkgs-fmt;
      };
    };
}