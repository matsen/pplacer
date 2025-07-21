#!/bin/bash
# Docker build script for pplacer
# Creates static binaries using Docker for Linux platforms

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Source common functions
source "$SCRIPT_DIR/build-common.sh"

# Docker-specific configuration
DEFAULT_PLATFORM="linux/amd64"
DEFAULT_OCAML_VERSION="5.2.1"

# Configuration
PLATFORM="${PLATFORM:-$DEFAULT_PLATFORM}"
OCAML_VERSION="${OCAML_VERSION:-$DEFAULT_OCAML_VERSION}"

# Parse platform for naming
case "$PLATFORM" in
    "linux/amd64")
        PLATFORM_NAME="linux-x86_64"
        ;;
    "linux/arm64")
        PLATFORM_NAME="linux-arm64"
        ;;
    *)
        PLATFORM_NAME="$(echo "$PLATFORM" | tr '/' '-')"
        ;;
esac

OUTPUT_NAME="pplacer-${PLATFORM_NAME}.zip"
IMAGE_TAG="pplacer:${PLATFORM_NAME}-$(echo "$OCAML_VERSION" | tr '.' '-')"

# Build Docker image
build_docker_image() {
    log_info "Building Docker image for platform $PLATFORM"
    log_info "Image tag: $IMAGE_TAG"
    log_info "OCaml version: $OCAML_VERSION"
    
    cd "$PROJECT_ROOT"
    
    # Check if multi-platform build is needed
    if [[ "$PLATFORM" != "linux/amd64" ]] && [[ "$(uname -m)" == "x86_64" ]]; then
        log_info "Cross-platform build detected, setting up buildx"
        
        # Ensure buildx is available
        if ! docker buildx version &> /dev/null; then
            log_error "Docker buildx not available. Please update Docker to a version that supports buildx."
            exit 1
        fi
        
        # Create builder if it doesn't exist
        docker buildx create --name pplacer-builder --use 2>/dev/null || docker buildx use pplacer-builder
        
        # Build with buildx
        docker buildx build \
            --platform "$PLATFORM" \
            --build-arg OCAML_VERSION="$OCAML_VERSION" \
            -t "$IMAGE_TAG" \
            --load \
            .
    else
        # Standard build
        docker build \
            --platform "$PLATFORM" \
            --build-arg OCAML_VERSION="$OCAML_VERSION" \
            -t "$IMAGE_TAG" \
            .
    fi
    
    log_success "Docker image built successfully"
}

# Extract binaries from Docker image
extract_binaries() {
    log_info "Extracting binaries from Docker image"
    
    # Create temporary container
    local container_name="pplacer-extract-$(date +%s)"
    docker create --name "$container_name" "$IMAGE_TAG"
    
    # Extract binaries
    docker cp "${container_name}:/pplacer.zip" "./$OUTPUT_NAME"
    
    # Clean up container
    docker rm "$container_name"
    
    log_success "Extracted binaries to $OUTPUT_NAME"
    ls -lh "$OUTPUT_NAME"
}

# Test extracted binaries (if possible)
test_binaries() {
    log_info "Testing extracted binaries"
    
    # Create temporary directory
    local test_dir="/tmp/pplacer-test-$$"
    mkdir -p "$test_dir"
    
    cd "$test_dir"
    unzip "$PROJECT_ROOT/$OUTPUT_NAME"
    
    # Test if binaries can run (basic check)
    if [[ "$PLATFORM" == "linux/amd64" ]] && [[ "$(uname)" == "Linux" ]] && [[ "$(uname -m)" == "x86_64" ]]; then
        log_info "Running native tests on compatible platform"
        
        ./pplacer --help > /dev/null && log_success "pplacer runs successfully"
        ./guppy --help > /dev/null && log_success "guppy runs successfully"
        ./rppr --help > /dev/null && log_success "rppr runs successfully"
        
        # Check if binaries are static (Linux only)
        if command -v ldd &> /dev/null; then
            if ldd ./pplacer 2>&1 | grep -q "not a dynamic executable"; then
                log_success "pplacer is statically linked"
            else
                log_warning "pplacer has dynamic dependencies:"
                ldd ./pplacer || true
            fi
        fi
    else
        log_info "Skipping native tests (different platform or architecture)"
    fi
    
    # Clean up
    cd "$PROJECT_ROOT"
    rm -rf "$test_dir"
}

# Main build function
main() {
    log_info "=== Docker build for pplacer ==="
    log_info "Platform: $PLATFORM"
    log_info "Platform name: $PLATFORM_NAME"
    log_info "OCaml version: $OCAML_VERSION"
    log_info "Output: $OUTPUT_NAME"
    
    # Check if Docker is available
    if ! command -v docker &> /dev/null; then
        log_error "Docker not found. Please install Docker first."
        exit 1
    fi
    
    if ! docker version &> /dev/null; then
        log_error "Docker daemon not running. Please start Docker."
        exit 1
    fi
    
    build_docker_image
    extract_binaries
    
    # Test binaries if --test flag is provided
    if [[ "$1" == "--test" ]]; then
        test_binaries
    fi
    
    log_success "Docker build complete! Output: $OUTPUT_NAME"
}

# Clean up Docker resources
cleanup() {
    log_info "Cleaning up Docker resources"
    
    # Remove image if it exists
    if docker image inspect "$IMAGE_TAG" &> /dev/null; then
        docker rmi "$IMAGE_TAG"
        log_success "Removed Docker image: $IMAGE_TAG"
    fi
    
    # Remove builder if it exists
    if docker buildx ls | grep -q "pplacer-builder"; then
        docker buildx rm pplacer-builder
        log_success "Removed buildx builder: pplacer-builder"
    fi
}

# Handle script arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [OPTIONS]"
        echo ""
        echo "Options:"
        echo "  --test    Test extracted binaries after build"
        echo "  --clean   Clean up Docker resources and exit"
        echo "  --help    Show this help message"
        echo ""
        echo "Environment variables:"
        echo "  PLATFORM        Target platform (default: $DEFAULT_PLATFORM)"
        echo "  OCAML_VERSION   OCaml version to use (default: $DEFAULT_OCAML_VERSION)"
        echo ""
        echo "Supported platforms:"
        echo "  linux/amd64    Linux x86_64 (Intel/AMD 64-bit)"
        echo "  linux/arm64    Linux ARM64 (Apple Silicon, ARM 64-bit)"
        echo ""
        echo "Examples:"
        echo "  $0                                    # Build for linux/amd64"
        echo "  $0 --test                             # Build and test binaries"
        echo "  PLATFORM=linux/arm64 $0              # Build for ARM64"
        echo "  OCAML_VERSION=4.14.2 $0              # Build with OCaml 4.14.2"
        echo "  $0 --clean                            # Clean up Docker resources"
        echo ""
        echo "Requirements:"
        echo "  - Docker with buildx support for cross-platform builds"
        exit 0
        ;;
    --clean)
        cleanup
        exit 0
        ;;
    --test)
        main --test
        ;;
    *)
        main "$@"
        ;;
esac