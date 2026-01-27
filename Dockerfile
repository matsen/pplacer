## pplacer-build
## Build pplacer from source with OCaml and opam 2.x
## Use: docker build --build-arg OCAML_VERSION=4.14.2 -t pplacer:ocaml4 .
##      docker build --build-arg OCAML_VERSION=5.2.1 -t pplacer:ocaml5 .

FROM ubuntu:22.04

# Build argument for OCaml version
ARG OCAML_VERSION=5.2.1

ENV DEBIAN_FRONTEND=noninteractive
ENV NO_AT_BRIDGE=1

# Install system dependencies (including static libraries)
RUN apt-get update && apt-get install -y \
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
  libc6-dev \
  gsl-bin \
  python3 \
  python3-pip \
  pipx \
  bubblewrap \
  rsync \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Install opam 2.x - detect architecture
RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then \
        OPAM_ARCH="x86_64"; \
    elif [ "$ARCH" = "aarch64" ]; then \
        OPAM_ARCH="arm64"; \
    else \
        echo "Unsupported architecture: $ARCH" && exit 1; \
    fi && \
    curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-${OPAM_ARCH}-linux -o /usr/local/bin/opam && \
    chmod +x /usr/local/bin/opam

# Initialize opam with specified OCaml version
RUN opam init --disable-sandboxing -y --compiler=${OCAML_VERSION} \
  && eval $(opam env)

# Add pplacer opam repository
RUN eval $(opam env) \
  && opam repo add pplacer-deps http://matsen.github.io/pplacer-opam-repository \
  && opam update

# Install OCaml dependencies with version pinning for OCaml 4.x
RUN eval $(opam env) \
  && if [ "${OCAML_VERSION}" = "4.14.2" ]; then \
  opam install -y \
  dune.3.19.1 \
  csv.2.4 \
  ounit2.2.2.7 \
  xmlm.1.4.0 \
  batteries.3.8.0 \
  gsl.1.25.0 \
  sqlite3.5.2.0 \
  camlzip.1.11 \
  ocamlfind; \
  else \
  opam install -y \
  dune \
  csv \
  ounit2 \
  xmlm \
  batteries \
  gsl \
  sqlite3 \
  camlzip \
  ocamlfind; \
  fi

# Copy pplacer source code
RUN mkdir -p /pplacer/src
WORKDIR /pplacer/src
COPY ./ /pplacer/src/

# Copy and build mcl source code
WORKDIR /pplacer/src/mcl
# Remove any stale object files that may have been checked in (e.g., from different platform)
RUN find . -name "*.o" -delete
RUN eval $(opam env) \
  && ./configure \
  && make
RUN eval $(opam env)
RUN echo "Checking MCL libraries..." \
  && ls -la src/clew/libclew.a \
  && ls -la src/impala/libimpala.a \
  && ls -la src/mcl/libmcl.a \
  && ls -la util/libutil.a \
  && echo "All MCL libraries built successfully!"

# Build pplacer with static linking - change to src directory first
WORKDIR /pplacer/src

# Create static dune configuration for Docker build  
RUN cp dune dune-dynamic && \
    echo '(include_subdirs unqualified)' > dune && \
    echo '' >> dune && \
    echo '(executables' >> dune && \
    echo ' (public_names pplacer guppy rppr -)' >> dune && \
    echo ' (names pplacer guppy rppr tests)' >> dune && \
    echo ' (flags :standard -w -7-9-36)' >> dune && \
    echo ' (foreign_stubs' >> dune && \
    echo '  (language c)' >> dune && \
    echo '  (names linear_c unix_support caml_pam pam' >> dune && \
    echo '         cddcore caml_cdd cddio cddlib cddlp cddmp cddproj pplacer_cdd setoper))' >> dune && \
    echo ' (libraries batteries sqlite3 camlzip gsl csv xmlm mcl ounit2))' >> dune && \
    echo '' >> dune && \
    echo '(subdir pplacer_src' >> dune && \
    echo ' (dirs)' >> dune && \
    echo ' (ocamllex newick_lexer)' >> dune && \
    echo ' (ocamlyacc newick_parser))' >> dune && \
    echo '' >> dune && \
    echo '(subdir json_src' >> dune && \
    echo ' (ocamllex jsonlex)' >> dune && \
    echo ' (ocamlyacc jsonparse))' >> dune
# Build pplacer
RUN eval $(opam env) \
  && dune build

# Install binaries
RUN cp _build/default/pplacer.exe /usr/local/bin/pplacer \
  && cp _build/default/guppy.exe /usr/local/bin/guppy \
  && cp _build/default/rppr.exe /usr/local/bin/rppr

# Install pplacer scripts
WORKDIR /pplacer/src/scripts
RUN chmod +x *.py \
  && cp *.py /usr/local/bin/

# Clean up install
# Package binaries and scripts
WORKDIR /pplacer/src
RUN mkdir -p /pplacer/bin \
  && cp _build/default/*.exe /pplacer/bin/ \
  && cd /pplacer/bin \
  && zip /pplacer.zip * \
  && cd /pplacer/src \
  && zip /pplacer.zip ./scripts/*

# Set working directory for data
WORKDIR /data
