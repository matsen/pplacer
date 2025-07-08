## pplacer-build
## Build pplacer from source with OCaml and opam 2.x
## Use: docker build --build-arg OCAML_VERSION=4.14.2 -t pplacer:ocaml4 .
##      docker build --build-arg OCAML_VERSION=5.2.1 -t pplacer:ocaml5 .

FROM ubuntu:24.04

# Build argument for OCaml version
ARG OCAML_VERSION=5.2.1

ENV DEBIAN_FRONTEND=noninteractive
ENV NO_AT_BRIDGE=1

# Install general-purpose tools
RUN apt-get update && apt-get install -y \
  vim

# Install system dependencies
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
  python3 \
  python3-pip \
  pipx \
  bubblewrap \
  rsync \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Install opam 2.x
RUN curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-x86_64-linux -o /usr/local/bin/opam \
  && chmod +x /usr/local/bin/opam

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
RUN mkdir -p /mcl/src
WORKDIR /mcl/src
COPY ./mcl /mcl/src/
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
WORKDIR /pplacer/src
RUN rm -rf mcl
RUN ln -s /mcl/src mcl

# Build pplacer
WORKDIR /pplacer/src
RUN eval $(opam env) \
  && dune build

# Install binaries
RUN cp _build/default/pplacer.exe /usr/local/bin/pplacer \
  && cp _build/default/guppy.exe /usr/local/bin/guppy \
  && cp _build/default/rppr.exe /usr/local/bin/rppr

# Package binaries and scripts
WORKDIR /pplacer/src
RUN mkdir -p /pplacer/bin \
  && cp _build/default/*.exe /pplacer/bin/ \
  && cd /pplacer/bin \
  && zip /pplacer.zip * \
  && cd /pplacer/src \
  && zip /pplacer.zip ./scripts/*

# Install pplacer scripts
WORKDIR /pplacer/src/scripts
RUN chmod +x *.py \
  && cp *.py /usr/local/bin/

# Set working directory for data
WORKDIR /data