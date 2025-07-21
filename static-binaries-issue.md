# Issue: Create Self-Contained Static Binaries and Simplify Build Process

## Problem

Currently, pplacer binaries built via Docker are dynamically linked and depend on system libraries (libgsl, libsqlite3, libz). This creates runtime dependencies that make distribution more complex.

Additionally, the macOS build process is handled separately with a dedicated script (`build-macos-native-ci.sh`) that duplicates much of the configuration logic from the Dockerfile, making maintenance harder.

## Current State

### Docker Build
- Creates dynamically linked binaries
- Dependencies: libgsl, libsqlite3, libz, libSystem
- Works for Linux x86_64 and arm64

### macOS Build  
- Uses separate `build-macos-native-ci.sh` script
- Handles Homebrew paths and environment setup
- Duplicates dependency installation logic from Dockerfile

## Proposed Solutions

### 1. Static Binary Creation

Modify the Dockerfile to create self-contained static binaries:
- Add `-ccopt -static` flag to dune configuration
- Ensure static libraries are available for all C dependencies
- This eliminates runtime library dependencies

**Benefits:**
- Truly portable binaries
- No runtime dependency issues
- Simpler deployment

### 2. Unified Build Script

Create a shared build script that can be used both locally and in GitHub Actions:
- Extract common build logic from `build-macos-native-ci.sh` and Dockerfile
- Support multiple platforms (Linux, macOS) 
- Allow local developers to build the same way as CI

**Structure:**
```
scripts/
├── build-common.sh      # Shared build logic
├── build-linux.sh      # Linux-specific wrapper
├── build-macos.sh      # macOS-specific wrapper
└── build-docker.sh     # Docker build wrapper
```

### 3. Platform-Specific Optimizations

- **Linux**: Use Alpine or multi-stage builds for smaller static binaries
- **macOS**: Handle both Intel and ARM architectures
- **Windows**: Consider adding Windows support using cross-compilation

## Implementation Plan

1. **Phase 1**: Static binaries in Docker
   - Modify Dockerfile to create static binaries
   - Test that binaries work without runtime dependencies
   - Verify builds still work on both x86_64 and arm64

2. **Phase 2**: Refactor build scripts
   - Extract common logic into reusable scripts
   - Update GitHub Actions to use new scripts
   - Provide documentation for local builds

3. **Phase 3**: Testing and validation
   - Verify static binaries work across different Linux distributions
   - Test local build process on different developer machines
   - Update documentation

## Expected Outcomes

- Self-contained binaries that run anywhere without dependencies
- Simplified build process for developers
- Consistent builds between local development and CI
- Easier maintenance of build configuration

## Breaking Changes

- None expected for end users (binaries will work the same or better)
- Developers may need to update their local build process
- CI artifacts will be truly static (this is an improvement)