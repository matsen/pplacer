# pplacer Installation Guide

This guide provides instructions for building pplacer using Docker or locally on your system. The project is being upgraded to support modern OCaml versions (4.14.x and 5.x) with opam 2.x.

## Docker Build Instructions

### Building the Docker Image

The Dockerfile supports building with different OCaml versions. You can specify the version using build arguments:

```bash
git submodule update --init --recursive

# Build with OCaml 4.14.2 (for LTS)
docker build --build-arg OCAML_VERSION=4.14.2 -t pplacer:ocaml4 .

# Build with OCaml 5.2.1 (latest)
docker build --build-arg OCAML_VERSION=5.2.1 -t pplacer:ocaml5 .
```

### Using pplacer in Docker

Once inside the container:
```bash
# The binaries are installed in /usr/local/bin
pplacer --version
guppy --version
rppr --version
```

## Local Build Instructions

Based on the Dockerfile, here are the steps to build pplacer locally:

### System Requirements

- Ubuntu 24.04 (or compatible Linux distribution)
- OCaml 4.14.x or 5.x (via opam 2.x)

### 1. Install System Dependencies

```bash
# Update package manager
sudo apt-get update

# Install build tools and libraries
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
  libgsl27 \
  zlib1g-dev \
  zlib1g \
  libsqlite3-dev \
  sqlite3 \
  python3 \
  python3-pip \
  pipx \
  bubblewrap \
  rsync
```

### 2. Install opam 2.x

```bash
# Download and install opam 2.2.1
curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-x86_64-linux -o /usr/local/bin/opam
sudo chmod +x /usr/local/bin/opam

# Initialize opam with your chosen OCaml version
# For OCaml 4.14.2:
opam switch create 4.14.2

# OR for OCaml 5.2.1:
opam switch create 5.2.1

# Activate environment
opam switch 4.14.2 && eval $(opam env)
```

### 3. Add pplacer opam Repository

```bash
opam repo add pplacer-deps http://matsen.github.io/pplacer-opam-repository
opam update
```

### 4. Install OCaml Dependencies

For OCaml 4.14.x (with specific versions for compatibility):
```bash
opam install -y \
  dune.3.19.1 \
  csv.2.4 \
  ounit2.2.2.7 \
  xmlm.1.4.0 \
  batteries.3.8.0 \
  gsl.1.25.0 \
  sqlite3.5.2.0 \
  camlzip.1.11 \
  ocamlfind
```

For OCaml 5.x (latest versions):
```bash
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
```

### 5. Build MCL (Custom Package)

MCL is a custom C library required by pplacer:

```bash
cd pplacer/mcl
./configure
make

# Verify libraries were built
ls -la src/clew/libclew.a
ls -la src/impala/libimpala.a
ls -la src/mcl/libmcl.a
ls -la util/libutil.a
```

### 6. Build pplacer

```bash
cd ../  # Back to pplacer directory
eval $(opam env)
dune build
```

### 7. Install Binaries and Scripts

Option 1: Install in opam environment (recommended):
```bash
# Install using dune (installs to opam environment)
dune install
```

Option 2: Install system-wide:
```bash
# Copy binaries to a system location
sudo cp _build/default/pplacer.exe /usr/local/bin/pplacer
sudo cp _build/default/guppy.exe /usr/local/bin/guppy
sudo cp _build/default/rppr.exe /usr/local/bin/rppr

# Install Python scripts
cd scripts
chmod +x *.py
sudo cp *.py /usr/local/bin/
```

### 8. Verify Installation

```bash
pplacer --version
guppy --version
rppr --version
```
