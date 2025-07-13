#!/bin/bash
set -e

echo "=== Native macOS CI build for pplacer ==="

# 1. Install system dependencies via Homebrew
echo "Installing system dependencies..."
brew install pkg-config gsl sqlite3 zlib

# 2. Install opam
echo "Installing opam..."
brew install opam

# 3. Initialize opam with OCaml 5.2.1
echo "Initializing opam..."
opam init -y --compiler=5.2.1 --disable-sandboxing

# 4. Set up opam environment
eval $(opam env)

# 5. Add pplacer opam repository
echo "Adding pplacer opam repository..."
opam repo add pplacer-deps http://matsen.github.io/pplacer-opam-repository || true
opam update

# 6. Install OCaml dependencies
echo "Installing OCaml dependencies..."
opam install -y \
    dune \
    csv \
    ounit2 \
    xmlm \
    batteries \
    gsl \
    sqlite3 \
    camlzip \
    ocamlfind

# 7. Build MCL (continue even if some parts fail)
echo "Building MCL..."
cd mcl
./configure || true
make || true
cd ..

# 8. Update dune file with proper include paths for homebrew libraries
echo "Updating dune file with homebrew include paths..."
# Check if dune file already has the flags, if not, add them
if ! grep -q "opt/homebrew" dune; then
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
fi

# 9. Set up environment for build
export PKG_CONFIG_PATH="/opt/homebrew/opt/gsl/lib/pkgconfig:/opt/homebrew/opt/sqlite/lib/pkgconfig:/opt/homebrew/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"
export CPPFLAGS="-I/opt/homebrew/opt/gsl/include -I/opt/homebrew/opt/sqlite/include -I/opt/homebrew/opt/zlib/include $CPPFLAGS"
export LDFLAGS="-L/opt/homebrew/opt/gsl/lib -L/opt/homebrew/opt/sqlite/lib -L/opt/homebrew/opt/zlib/lib $LDFLAGS"

# 10. Build pplacer
echo "Building pplacer..."
eval $(opam env)
dune build

# 11. Package binaries
echo "Packaging binaries..."
rm -rf build-output
mkdir -p build-output
cp _build/default/pplacer.exe build-output/pplacer
cp _build/default/guppy.exe build-output/guppy
cp _build/default/rppr.exe build-output/rppr

# Make scripts executable and copy
chmod +x scripts/*.py
cp scripts/*.py build-output/

# Create zip file (matching Docker build output)
cd build-output
zip ../pplacer-macos-arm64.zip *
cd ..

echo "Build complete! Output: pplacer-macos-arm64.zip"
ls -lh pplacer-macos-arm64.zip