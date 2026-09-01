#!/bin/bash
# Common build functions and configuration for pplacer
# This script is sourced by platform-specific build scripts

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored output
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Configuration
DEFAULT_OCAML_VERSION="5.2.1"
OCAML_VERSION="${OCAML_VERSION:-$DEFAULT_OCAML_VERSION}"
PPLACER_REPO_URL="http://matsen.github.io/pplacer-opam-repository"

# OCaml dependencies - using arrays instead of associative arrays for compatibility
OCAML_DEPS_4_14="dune.3.19.1 csv.2.4 ounit2.2.2.7 xmlm.1.4.0 batteries.3.8.0 gsl.1.25.0 sqlite3.5.2.0 camlzip.1.11 ocamlfind"
OCAML_DEPS_5_X="dune csv ounit2 xmlm batteries gsl sqlite3 camlzip ocamlfind"

# Initialize opam environment
init_opam() {
    local compiler_version="$1"
    log_info "Initializing opam with OCaml $compiler_version"
    
    if ! command -v opam &> /dev/null; then
        log_error "opam not found. Please install opam first."
        exit 1
    fi
    
    opam init -y --compiler="$compiler_version" --disable-sandboxing
    eval $(opam env)
    
    log_info "Adding pplacer opam repository"
    opam repo add pplacer-deps "$PPLACER_REPO_URL" || true
    opam update
}

# Install OCaml dependencies
install_ocaml_deps() {
    log_info "Installing OCaml dependencies for version $OCAML_VERSION"
    
    eval $(opam env)
    
    if [[ "$OCAML_VERSION" == "4.14"* ]]; then
        # Install with version pinning for OCaml 4.x
        opam install -y $OCAML_DEPS_4_14
    else
        # Install latest versions for OCaml 5.x
        opam install -y $OCAML_DEPS_5_X
    fi
    
    log_success "OCaml dependencies installed"
}

# Build MCL
build_mcl() {
    log_info "Building MCL"
    
    if [[ ! -d "mcl" ]]; then
        log_error "MCL directory not found. Please ensure you're in the pplacer root directory."
        exit 1
    fi
    
    cd mcl
    ./configure || log_warning "MCL configure had issues, continuing anyway"
    make || log_warning "MCL make had issues, continuing anyway"
    cd ..
    
    # Verify MCL libraries were built
    local mcl_libs=(
        "mcl/src/clew/libclew.a"
        "mcl/src/impala/libimpala.a"
        "mcl/src/mcl/libmcl.a"
        "mcl/util/libutil.a"
    )
    
    log_info "Checking MCL libraries..."
    for lib in "${mcl_libs[@]}"; do
        if [[ -f "$lib" ]]; then
            log_success "Found $lib"
        else
            log_warning "Missing $lib"
        fi
    done
}

# Create static dune configuration
create_static_dune_config() {
    local static_flags="$1"
    log_info "Creating static dune configuration"
    
    # Backup original dune file
    if [[ -f "dune" ]] && [[ ! -f "dune-dynamic" ]]; then
        cp dune dune-dynamic
    fi
    
    cat > dune << EOF
(include_subdirs unqualified)

(executables
 (public_names pplacer guppy rppr -)
 (names pplacer guppy rppr tests)
 (flags :standard -w -7-9-36)
 (link_flags ($static_flags -cclib -lgsl -cclib -lgslcblas))
 (foreign_stubs
  (language c)
  (names linear_c unix_support caml_pam pam
         cddcore caml_cdd cddio cddlib cddlp cddmp cddproj pplacer_cdd setoper))
 (libraries batteries sqlite3 camlzip gsl csv xmlm mcl ounit2))

(subdir pplacer_src
 (dirs)
 (ocamllex newick_lexer)
 (ocamlyacc newick_parser))

(subdir json_src
 (ocamllex jsonlex)
 (ocamlyacc jsonparse))
EOF

    log_success "Static dune configuration created"
}

# Build pplacer
build_pplacer() {
    log_info "Building pplacer"
    
    eval $(opam env)
    dune build
    
    # Verify binaries were built
    local binaries=("pplacer.exe" "guppy.exe" "rppr.exe")
    for binary in "${binaries[@]}"; do
        if [[ -f "_build/default/$binary" ]]; then
            log_success "Built $binary"
        else
            log_error "Failed to build $binary"
            exit 1
        fi
    done
}

# Package binaries
package_binaries() {
    local output_name="$1"
    log_info "Packaging binaries as $output_name"
    
    rm -rf build-output
    mkdir -p build-output
    
    # Copy binaries (remove .exe extension for consistency)
    cp _build/default/pplacer.exe build-output/pplacer
    cp _build/default/guppy.exe build-output/guppy
    cp _build/default/rppr.exe build-output/rppr
    
    # Copy and make scripts executable
    if [[ -d "scripts" ]]; then
        chmod +x scripts/*.py 2>/dev/null || true
        cp scripts/*.py build-output/ 2>/dev/null || true
    fi
    
    # Create zip file
    cd build-output
    zip "../$output_name" *
    cd ..
    
    log_success "Created $output_name"
    ls -lh "$output_name"
}

# Restore original dune configuration
restore_dune_config() {
    if [[ -f "dune-dynamic" ]]; then
        log_info "Restoring original dune configuration"
        mv dune-dynamic dune
    fi
}

# Clean up build artifacts
clean_build() {
    log_info "Cleaning build artifacts"
    rm -rf build-output
    rm -rf _build
    if [[ -f "dune-dynamic" ]]; then
        mv dune-dynamic dune
    fi
}

# Verify binary is static (Linux only)
verify_static_binary() {
    local binary_path="$1"
    
    if [[ "$(uname)" == "Linux" ]] && command -v ldd &> /dev/null; then
        log_info "Checking if $binary_path is statically linked"
        if ldd "$binary_path" 2>&1 | grep -q "not a dynamic executable"; then
            log_success "$binary_path is statically linked"
        else
            log_warning "$binary_path has dynamic dependencies:"
            ldd "$binary_path" || true
        fi
    fi
}