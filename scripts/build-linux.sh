#!/bin/bash
# Linux native build script for pplacer
# Creates static binaries on Linux systems

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Source common functions
source "$SCRIPT_DIR/build-common.sh"

# Platform-specific configuration
PLATFORM="linux"
ARCHITECTURE="$(uname -m)"
OUTPUT_NAME="pplacer-${PLATFORM}-${ARCHITECTURE}.zip"

# Install system dependencies (requires sudo)
install_system_deps() {
    log_info "Installing system dependencies for Linux"
    
    # Check if we're on Ubuntu/Debian
    if command -v apt-get &> /dev/null; then
        log_info "Detected Debian/Ubuntu system"
        sudo apt-get update
        sudo apt-get install -y \
            git \
            build-essential \
            pkg-config \
            m4 \
            wget \
            curl \
            unzip \
            zip \
            libgsl-dev \
            zlib1g-dev \
            libsqlite3-dev \
            libc6-dev \
            bubblewrap \
            rsync
    # Check if we're on RHEL/CentOS/Fedora
    elif command -v yum &> /dev/null || command -v dnf &> /dev/null; then
        local pkg_manager="yum"
        if command -v dnf &> /dev/null; then
            pkg_manager="dnf"
        fi
        
        log_info "Detected RHEL/CentOS/Fedora system"
        sudo "$pkg_manager" install -y \
            git \
            gcc \
            gcc-c++ \
            make \
            pkgconfig \
            m4 \
            wget \
            curl \
            unzip \
            zip \
            gsl-devel \
            zlib-devel \
            sqlite-devel \
            glibc-devel \
            rsync
    else
        log_warning "Unknown package manager. Please install dependencies manually:"
        log_warning "  - Build tools (gcc, make, pkg-config, m4)"
        log_warning "  - Libraries (gsl, zlib, sqlite3)"
        log_warning "  - Utilities (git, wget, curl, unzip, zip, rsync)"
    fi
    
    log_success "System dependencies installed"
}

# Install opam
install_opam() {
    if command -v opam &> /dev/null; then
        log_info "opam already installed: $(opam --version)"
        return
    fi
    
    log_info "Installing opam"
    local opam_url="https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-$(uname -m)-linux"
    
    curl -L "$opam_url" -o /tmp/opam
    sudo install /tmp/opam /usr/local/bin/opam
    rm /tmp/opam
    
    log_success "opam installed: $(opam --version)"
}

# Main build function
main() {
    log_info "=== Linux native build for pplacer ==="
    log_info "Platform: $PLATFORM"
    log_info "Architecture: $ARCHITECTURE"
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
    if ! command -v opam &> /dev/null; then
        log_error "opam not found. Run with --deps to install dependencies, or install opam manually."
        exit 1
    fi
    
    # Build process
    init_opam "$OCAML_VERSION"
    install_ocaml_deps
    build_mcl
    
    # Create static configuration with Linux-specific flags
    create_static_dune_config "-ccopt -static"
    
    build_pplacer
    
    # Verify static linking
    verify_static_binary "_build/default/pplacer.exe"
    
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
        echo "  --deps    Install system dependencies (requires sudo)"
        echo "  --help    Show this help message"
        echo ""
        echo "Environment variables:"
        echo "  OCAML_VERSION   OCaml version to use (default: $DEFAULT_OCAML_VERSION)"
        echo ""
        echo "Examples:"
        echo "  $0 --deps              # Install dependencies only"
        echo "  $0                     # Build pplacer"
        echo "  OCAML_VERSION=4.14.2 $0  # Build with specific OCaml version"
        exit 0
        ;;
    *)
        main "$@"
        ;;
esac