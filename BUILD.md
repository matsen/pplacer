# Building pplacer

This document describes how to build pplacer binaries on different platforms using the unified build system.

## Quick Start

### Docker (Recommended for Linux)
```bash
# Build static binaries for Linux x86_64
./scripts/build-docker.sh

# Build for ARM64
PLATFORM=linux/arm64 ./scripts/build-docker.sh

# Test binaries after build
./scripts/build-docker.sh --test
```

### macOS
```bash
# Install dependencies first (requires Homebrew)
./scripts/build-macos.sh --deps

# Build pplacer
./scripts/build-macos.sh
```

### Linux (Native)
```bash
# Install dependencies first (requires sudo)
./scripts/build-linux.sh --deps

# Build static binaries
./scripts/build-linux.sh
```

## Build Scripts

The build system consists of several scripts in the `scripts/` directory:

- **`build-common.sh`** - Shared functions and configuration
- **`build-docker.sh`** - Docker-based builds (creates static binaries)
- **`build-macos.sh`** - Native macOS builds
- **`build-linux.sh`** - Native Linux builds (creates static binaries)

## Static vs Dynamic Binaries

### Docker Builds (Linux)
- Creates **static binaries** that don't require external libraries
- Portable across different Linux distributions
- Self-contained executables

### macOS Builds
- Creates **dynamic binaries** linked to Homebrew libraries
- Requires GSL, SQLite3, and zlib to be installed
- Works on both Intel and Apple Silicon Macs

### Linux Native Builds
- Creates **static binaries** when possible
- Falls back to dynamic linking if static libraries unavailable

## Configuration Options

### Environment Variables

- **`OCAML_VERSION`** - OCaml version to use (default: 5.2.1)
  - Supported: `5.2.1`, `4.14.2`
- **`PLATFORM`** - Target platform for Docker builds (default: linux/amd64)
  - Supported: `linux/amd64`, `linux/arm64`

### Examples

```bash
# Build with OCaml 4.14.2
OCAML_VERSION=4.14.2 ./scripts/build-macos.sh

# Build for ARM64 Linux
PLATFORM=linux/arm64 ./scripts/build-docker.sh

# Build and test
./scripts/build-docker.sh --test
```

## Build Process

All builds follow the same general process:

1. **Install dependencies** - System libraries and OCaml packages
2. **Initialize opam** - Set up OCaml environment
3. **Build MCL** - Compile MCL clustering library
4. **Configure dune** - Set up build configuration (static/dynamic)
5. **Build pplacer** - Compile main binaries
6. **Package outputs** - Create zip files with binaries and scripts

## Output

All builds create a zip file containing:
- `pplacer` - Main phylogenetic placement tool
- `guppy` - Placement analysis and visualization
- `rppr` - Reference package operations
- `*.py` - Python helper scripts

Output file naming:
- `pplacer-linux-x86_64.zip` - Linux Intel/AMD 64-bit
- `pplacer-linux-arm64.zip` - Linux ARM64
- `pplacer-macos-arm64.zip` - macOS Apple Silicon
- `pplacer-macos-intel.zip` - macOS Intel

## Requirements

### Docker Builds
- Docker with buildx support
- No other dependencies required

### macOS Builds
- Homebrew package manager
- Xcode command line tools: `xcode-select --install`

### Linux Native Builds
- Package manager (apt, yum, or dnf)
- Build tools (gcc, make, pkg-config)
- Development libraries (gsl-devel, zlib-devel, sqlite3-devel)

## Troubleshooting

### Common Issues

1. **opam initialization fails**
   ```bash
   # Clear opam state and retry
   rm -rf ~/.opam
   ./scripts/build-macos.sh --deps
   ```

2. **MCL build fails**
   - This is expected in some cases
   - Build continues with warning
   - MCL functionality may be limited

3. **Missing static libraries (Linux)**
   - Install `-dev` packages: `libgsl-dev`, `zlib1g-dev`, `libsqlite3-dev`
   - Or use Docker build instead

4. **Homebrew path issues (macOS)**
   - Ensure Homebrew is properly installed
   - Check paths: `/opt/homebrew` (Apple Silicon) or `/usr/local` (Intel)

### Getting Help

```bash
# Show help for any build script
./scripts/build-docker.sh --help
./scripts/build-macos.sh --help
./scripts/build-linux.sh --help
```

## Development Workflow

For active development:

1. **Make changes** to source code
2. **Test locally**:
   ```bash
   # Quick build without packaging
   eval $(opam env)
   dune build
   
   # Test specific binary
   ./_build/default/pplacer.exe --help
   ```
3. **Full build and test**:
   ```bash
   ./scripts/build-macos.sh  # or appropriate platform
   ```

## CI/CD Integration

GitHub Actions automatically builds releases using these scripts:
- Linux builds use `build-docker.sh`
- macOS builds use `build-macos.sh`
- Artifacts uploaded as release assets

See `.github/workflows/build-release.yml` for details.