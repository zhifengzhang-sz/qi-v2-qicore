{
  description = "QiCore Foundation - Mathematical Result<T> in Haskell with GHC 9.12.2 (Minimal)";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Use GHC 9.12.2 - minimal setup with only working tools
        ghcVersion = "ghc9122";
        hpkgs = pkgs.haskell.packages.${ghcVersion};
        
        # MINIMAL essential tools that work with GHC 9.12.2
        devTools = [
          # Core tools (verified working)
          hpkgs.ghc
          hpkgs.cabal-install
          
          # System utilities
          pkgs.git
          pkgs.curl
          pkgs.jq
          pkgs.nixpkgs-fmt
          
          # Note: Advanced tools temporarily excluded due to GHC 9.12.2 compatibility:
          # - haskell-language-server (build failures with GHC 9.12.2)
          # - ghcid (dependency issues)
          # - ormolu (version bound problems)
          # - hlint (compatibility lag)
        ];
        
      in {
        # Minimal but functional development shell
        devShells.default = pkgs.mkShell {
          name = "qi-base-ghc912-minimal";
          buildInputs = devTools;
          
          shellHook = ''
            echo "🚀 QiCore Foundation - GHC 9.12.2 Minimal Environment"
            echo "📦 GHC Version: $(ghc --version)"
            echo ""
            echo "✅ WORKING TOOLS:"
            echo "  cabal build qi-base           - Build library (✅ WORKS)"
            echo "  cabal test qi-base-test       - Run tests (✅ WORKS)"
            echo "  cabal repl                    - Interactive development (✅ WORKS)"
            echo "  cabal haddock                 - Generate docs (✅ WORKS)"
            echo ""
            echo "⏳ TEMPORARILY EXCLUDED (Ecosystem lag):"
            echo "  haskell-language-server       - Build failures with GHC 9.12.2"
            echo "  ghcid                        - Dependency compatibility issues"
            echo "  ormolu                       - Version bound problems"
            echo "  hlint                        - Compatibility lag"
            echo ""
            echo "💡 DEVELOPMENT WORKFLOW:"
            echo "  1. Edit code in your favorite editor"
            echo "  2. Use 'cabal build' for fast feedback"
            echo "  3. Use 'cabal test' for verification"
            echo "  4. Use 'cabal repl' for interactive development"
            echo ""
            echo "🔥 You have CUTTING EDGE GHC 9.12.2 with core functionality!"
            echo "   Advanced tooling will follow as ecosystem catches up."
            echo ""
          '';
        };
        
        # Package outputs (core functionality works)
        packages = {
          default = hpkgs.callCabal2nix "qi-base" ./. {};
          qi-base = hpkgs.callCabal2nix "qi-base" ./. {};
        };
        
        # CI shell (minimal dependencies)
        devShells.ci = pkgs.mkShell {
          buildInputs = [
            hpkgs.ghc
            hpkgs.cabal-install
            pkgs.git
          ];
        };
        
        # Formatter
        formatter = pkgs.nixpkgs-fmt;
      });
  
  # Nix configuration
  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    extra-substituters = [ "https://cache.nixos.org/" ];
    extra-trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
  };
}