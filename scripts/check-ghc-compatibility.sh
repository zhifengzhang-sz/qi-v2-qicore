#!/usr/bin/env bash

# GHC 9.12.1 Compatibility Checker
# Automated monitoring for ecosystem tool compatibility

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"
COMPATIBILITY_DOC="$WORKSPACE_ROOT/haskell/docs/GHC_9_12_1_COMPATIBILITY.md"
REPORT_FILE="$WORKSPACE_ROOT/GHC_COMPATIBILITY_$(date +%Y%m%d_%H%M%S).md"

# Logging functions
log() {
    echo -e "${BLUE}[COMPAT]${NC} $1"
}

section() {
    echo -e "\n${CYAN}=== $1 ===${NC}"
}

success() {
    echo -e "${GREEN}✅ $1${NC}"
}

warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

error() {
    echo -e "${RED}❌ $1${NC}" >&2
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Get latest release from GitHub
get_latest_release() {
    local repo="$1"
    if command_exists curl && command_exists jq; then
        curl -s "https://api.github.com/repos/$repo/releases/latest" | jq -r '.tag_name' 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

# Check tool compatibility
check_tool_compatibility() {
    local tool="$1"
    local repo="$2"
    local status="$3"
    
    echo "🔍 Checking $tool..."
    local latest_version=$(get_latest_release "$repo")
    
    if [ "$latest_version" != "unknown" ]; then
        echo "   Latest version: $latest_version"
        if [ "$status" = "incompatible" ]; then
            warning "$tool - $latest_version (check if GHC 9.12.1 support added)"
        else
            success "$tool - $latest_version (compatible)"
        fi
    else
        warning "$tool - unable to fetch version info"
    fi
    
    return 0
}

# Generate compatibility report
generate_compatibility_report() {
    log "Generating GHC 9.12.1 compatibility report..."
    
    cat > "$REPORT_FILE" << EOF
# GHC 9.12.1 Compatibility Report

**Generated**: $(date '+%Y-%m-%d %H:%M:%S')  
**Purpose**: Automated ecosystem compatibility check

## 🎯 Tool Status Check

EOF

    section "Core Development Tools"
    
    echo "### Haskell Language Server" >> "$REPORT_FILE"
    check_tool_compatibility "haskell-language-server" "haskell/haskell-language-server" "incompatible"
    echo "" >> "$REPORT_FILE"
    
    echo "### Development Tools" >> "$REPORT_FILE"
    check_tool_compatibility "ghcid" "ndmitchell/ghcid" "incompatible"
    check_tool_compatibility "ormolu" "tweag/ormolu" "incompatible"  
    check_tool_compatibility "hlint" "ndmitchell/hlint" "incompatible"
    echo "" >> "$REPORT_FILE"
    
    section "Nix Ecosystem Check"
    
    if command_exists nix; then
        echo "### Nix Package Availability" >> "$REPORT_FILE"
        echo "Checking GHC packages in nixpkgs..." >> "$REPORT_FILE"
        
        # Check available GHC versions
        if nix eval --impure --expr 'builtins.attrNames (import <nixpkgs> {}).haskell.compiler' 2>/dev/null | grep -q "ghc9121"; then
            success "GHC 9.12.1 available in nixpkgs"
            echo "✅ GHC 9.12.1 available in nixpkgs" >> "$REPORT_FILE"
        else
            warning "GHC 9.12.1 may not be available in current nixpkgs"
            echo "⚠️ GHC 9.12.1 may not be available in current nixpkgs" >> "$REPORT_FILE"
        fi
    else
        warning "Nix not available for ecosystem check"
        echo "⚠️ Nix not available for ecosystem check" >> "$REPORT_FILE"
    fi
    
    echo "" >> "$REPORT_FILE"
    
    section "Environment Verification"
    
    # Test current environment
    if [ -f "$WORKSPACE_ROOT/haskell/flake.nix" ]; then
        cd "$WORKSPACE_ROOT/haskell"
        
        echo "### Build System Test" >> "$REPORT_FILE"
        if nix develop --command ghc --version 2>/dev/null | grep -q "9.12.1"; then
            success "GHC 9.12.1 working in development environment"
            echo "✅ GHC 9.12.1 working in development environment" >> "$REPORT_FILE"
        else
            error "GHC 9.12.1 not working in development environment"
            echo "❌ GHC 9.12.1 not working in development environment" >> "$REPORT_FILE"
        fi
        
        if nix develop --command cabal build qi-base 2>/dev/null; then
            success "Project builds successfully with GHC 9.12.1"
            echo "✅ Project builds successfully with GHC 9.12.1" >> "$REPORT_FILE"
        else
            error "Project build failed with GHC 9.12.1"
            echo "❌ Project build failed with GHC 9.12.1" >> "$REPORT_FILE"
        fi
    fi
    
    cat >> "$REPORT_FILE" << EOF

## 🔄 Next Actions

### If Tools Became Compatible
1. Update flake.nix to include newly compatible tools
2. Update GHC_9_12_1_COMPATIBILITY.md documentation
3. Test tools in development environment
4. Notify team of available tools

### If Critical Issues Found
1. Review compatibility blockers
2. Assess workaround strategies
3. Consider alternative tools
4. Update development workflow

## 📅 Next Check

**Schedule**: $(date -d "+1 month" '+%Y-%m-%d')  
**Command**: \`./scripts/check-ghc-compatibility.sh\`

---

**Report Location**: $REPORT_FILE  
**Documentation**: haskell/docs/GHC_9_12_1_COMPATIBILITY.md
EOF

    success "Compatibility report generated: $REPORT_FILE"
}

# Update compatibility documentation
update_compatibility_docs() {
    local new_info="$1"
    
    if [ -f "$COMPATIBILITY_DOC" ]; then
        # Add timestamp to last check section
        local check_date=$(date '+%Y-%m-%d %H:%M:%S')
        local temp_file=$(mktemp)
        
        # Update the "Next Review" date in the documentation
        sed "s/\*\*Next Review\*\*:.*/\*\*Next Review\*\*: $(date -d "+1 month" '+%Y-%m-%d')/" "$COMPATIBILITY_DOC" > "$temp_file"
        mv "$temp_file" "$COMPATIBILITY_DOC"
        
        success "Updated compatibility documentation with latest check"
    else
        warning "Compatibility documentation not found at $COMPATIBILITY_DOC"
    fi
}

# Check specific tool
check_specific_tool() {
    local tool="$1"
    
    case "$tool" in
        "hls"|"haskell-language-server")
            check_tool_compatibility "haskell-language-server" "haskell/haskell-language-server" "incompatible"
            ;;
        "ghcid")
            check_tool_compatibility "ghcid" "ndmitchell/ghcid" "incompatible"
            ;;
        "ormolu")
            check_tool_compatibility "ormolu" "tweag/ormolu" "incompatible"
            ;;
        "hlint")
            check_tool_compatibility "hlint" "ndmitchell/hlint" "incompatible"
            ;;
        *)
            error "Unknown tool: $tool"
            echo "Available tools: hls, ghcid, ormolu, hlint"
            exit 1
            ;;
    esac
}

# Main function
main() {
    case "${1:-all}" in
        --tool)
            if [ -z "${2:-}" ]; then
                error "Tool name required. Usage: $0 --tool <tool_name>"
                exit 1
            fi
            check_specific_tool "$2"
            ;;
        --report)
            generate_compatibility_report
            ;;
        --update-docs)
            update_compatibility_docs "manual update"
            ;;
        --quiet)
            generate_compatibility_report > /dev/null
            echo "$REPORT_FILE"
            ;;
        --help)
            cat << EOF
Usage: $0 [OPTION] [TOOL]

GHC 9.12.1 Compatibility Checker

Options:
  --tool <name>     Check specific tool (hls, ghcid, ormolu, hlint)
  --report          Generate full compatibility report
  --update-docs     Update compatibility documentation
  --quiet           Generate report silently, output filename only
  --help            Show this help message
  (no args)         Quick compatibility check

Examples:
  $0                      # Quick check all tools
  $0 --tool hls           # Check HLS specifically
  $0 --report             # Generate full report
  $0 --quiet              # Silent report generation

EOF
            ;;
        *)
            log "Quick GHC 9.12.1 compatibility check"
            section "Critical Development Tools"
            check_tool_compatibility "haskell-language-server" "haskell/haskell-language-server" "incompatible"
            check_tool_compatibility "ormolu" "tweag/ormolu" "incompatible"
            
            echo ""
            log "For full report: $0 --report"
            log "For specific tool: $0 --tool <tool_name>"
            ;;
    esac
}

# Ensure required commands are available
if ! command_exists curl; then
    warning "curl not found - version checks will be limited"
fi

if ! command_exists jq; then
    warning "jq not found - JSON parsing will be limited"
fi

# Run main function
main "$@"