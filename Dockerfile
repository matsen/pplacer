## pplacer-build
## Build pplacer from source with OCaml 4.14.x and opam 2.x

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV NO_AT_BRIDGE=1

# Install general-purpose tools
RUN apt-get update && apt-get install -y \
  vim \
  openjdk-8-jdk

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

# Initialize opam with OCaml 4.14.2
RUN opam init --disable-sandboxing -y --compiler=4.14.2 \
  && eval $(opam env)

# Add pplacer opam repository
RUN eval $(opam env) \
  && opam repo add pplacer-deps http://matsen.github.io/pplacer-opam-repository \
  && opam update

# Install OCaml dependencies
RUN eval $(opam env) \
  && opam install -y \
  dune.3.19.1 \
  csv.2.4 \
  ounit2.2.2.7 \
  xmlm.1.4.0 \
  batteries.3.8.0 \
  gsl.1.25.0 \
  sqlite3.5.2.0 \
  camlzip.1.11 \
  ocamlfind

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
