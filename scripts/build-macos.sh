#!/bin/bash
# macOS native build script for pplacer
# Creates binaries on macOS systems (Intel and Apple Silicon)

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Source common functions
source "$SCRIPT_DIR/build-common.sh"

# Platform-specific configuration
PLATFORM="macos"
ARCHITECTURE="$(uname -m)"
# Map x86_64 to more familiar intel name
if [[ "$ARCHITECTURE" == "x86_64" ]]; then
    ARCH_NAME="intel"
else
    ARCH_NAME="$ARCHITECTURE"
fi
OUTPUT_NAME="pplacer-${PLATFORM}-${ARCH_NAME}.zip"

# Install system dependencies via Homebrew
install_system_deps() {
    log_info "Installing system dependencies for macOS"
    
    # Check if Homebrew is installed
    if ! command -v brew &> /dev/null; then
        log_error "Homebrew not found. Please install Homebrew first:"
        log_error "  /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
        exit 1
    fi
    
    log_info "Installing dependencies via Homebrew"
    brew install pkg-config gsl sqlite3 zlib
    
    log_success "System dependencies installed"
}

# Install opam
install_opam() {
    if command -v opam &> /dev/null; then
        log_info "opam already installed: $(opam --version)"
        return
    fi
    
    log_info "Installing opam via Homebrew"
    brew install opam
    
    log_success "opam installed: $(opam --version)"
}

# Create macOS-specific dune configuration with Homebrew paths
create_macos_dune_config() {
    log_info "Creating macOS dune configuration with Homebrew paths"
    
    # Backup original dune file
    if [[ -f "dune" ]] && [[ ! -f "dune-dynamic" ]]; then
        cp dune dune-dynamic
    fi
    
    cat > dune << 'EOF'
(include_subdirs unqualified)

(executables
 (public_names pplacer guppy rppr -)
 (names pplacer guppy rppr tests)
 (flags :standard -w -7-9-36)
 (foreign_stubs
  (language c)
  (names linear_c unix_support caml_pam pam
         cddcore caml_cdd cddio cddlib cddlp cddmp cddproj pplacer_cdd setoper)
  (flags -I/opt/homebrew/opt/gsl/include -I/opt/homebrew/opt/sqlite/include -I/opt/homebrew/opt/zlib/include))
 (libraries batteries sqlite3 camlzip gsl csv xmlm mcl ounit2))

(subdir pplacer_src
 (dirs)
 (ocamllex newick_lexer)
 (ocamlyacc newick_parser))

(subdir json_src
 (ocamllex jsonlex)
 (ocamlyacc jsonparse))
EOF

    log_success "macOS dune configuration created"
}

# Set up macOS build environment
setup_macos_env() {
    log_info "Setting up macOS build environment"
    
    # Set up PKG_CONFIG_PATH for Homebrew
    export PKG_CONFIG_PATH="/opt/homebrew/opt/gsl/lib/pkgconfig:/opt/homebrew/opt/sqlite/lib/pkgconfig:/opt/homebrew/opt/zlib/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
    
    # Set up compiler flags for Homebrew
    export CPPFLAGS="-I/opt/homebrew/opt/gsl/include -I/opt/homebrew/opt/sqlite/include -I/opt/homebrew/opt/zlib/include ${CPPFLAGS:-}"
    export LDFLAGS="-L/opt/homebrew/opt/gsl/lib -L/opt/homebrew/opt/sqlite/lib -L/opt/homebrew/opt/zlib/lib ${LDFLAGS:-}"
    
    # Also check /usr/local for Intel Macs
    if [[ -d "/usr/local/opt/gsl" ]]; then
        export PKG_CONFIG_PATH="/usr/local/opt/gsl/lib/pkgconfig:/usr/local/opt/sqlite/lib/pkgconfig:/usr/local/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"
        export CPPFLAGS="-I/usr/local/opt/gsl/include -I/usr/local/opt/sqlite/include -I/usr/local/opt/zlib/include $CPPFLAGS"
        export LDFLAGS="-L/usr/local/opt/gsl/lib -L/usr/local/opt/sqlite/lib -L/usr/local/opt/zlib/lib $LDFLAGS"
    fi
    
    log_info "PKG_CONFIG_PATH: $PKG_CONFIG_PATH"
    log_info "CPPFLAGS: $CPPFLAGS"
    log_info "LDFLAGS: $LDFLAGS"
}

# Check binary dependencies (macOS-specific)
check_binary_deps() {
    local binary_path="$1"
    log_info "Checking dependencies for $binary_path"
    
    if command -v otool &> /dev/null; then
        log_info "Dynamic library dependencies:"
        otool -L "$binary_path" | grep -E "(dylib|so)" | head -10 || log_info "No external dynamic libraries found"
    fi
}

# Main build function
main() {
    log_info "=== macOS native build for pplacer ==="
    log_info "Platform: $PLATFORM"
    log_info "Architecture: $ARCHITECTURE ($ARCH_NAME)"
    log_info "OCaml version: $OCAML_VERSION"
    log_info "Output: $OUTPUT_NAME"
    
    cd "$PROJECT_ROOT"
    
    # Check if --deps flag was passed
    if [[ "$1" == "--deps" ]]; then
        install_system_deps
        install_opam
        log_success "Dependencies installed. Run without --deps to build."
        exit 0
    fi
    
    # Verify dependencies
    if ! command -v brew &> /dev/null; then
        log_error "Homebrew not found. Run with --deps to install dependencies."
        exit 1
    fi
    
    if ! command -v opam &> /dev/null; then
        log_error "opam not found. Run with --deps to install dependencies."
        exit 1
    fi
    
    # Set up environment
    setup_macos_env
    
    # Build process
    init_opam "$OCAML_VERSION"
    install_ocaml_deps
    build_mcl
    
    # Create macOS-specific configuration (no static linking on macOS)
    create_macos_dune_config
    
    build_pplacer
    
    # Check binary dependencies
    check_binary_deps "_build/default/pplacer.exe"
    
    package_binaries "$OUTPUT_NAME"
    restore_dune_config
    
    log_success "Build complete! Output: $OUTPUT_NAME"
}

# Handle script arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [OPTIONS]"
        echo ""
        echo "Options:"
        echo "  --deps    Install system dependencies via Homebrew"
        echo "  --help    Show this help message"
        echo ""
        echo "Environment variables:"
        echo "  OCAML_VERSION   OCaml version to use (default: $DEFAULT_OCAML_VERSION)"
        echo ""
        echo "Examples:"
        echo "  $0 --deps              # Install dependencies only"
        echo "  $0                     # Build pplacer"
        echo "  OCAML_VERSION=4.14.2 $0  # Build with specific OCaml version"
        echo ""
        echo "Requirements:"
        echo "  - Homebrew (https://brew.sh/)"
        echo "  - Xcode command line tools: xcode-select --install"
        exit 0
        ;;
    *)
        main "$@"
        ;;
esac