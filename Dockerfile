# pplacer-build
# Build pplacer from source.

FROM ubuntu:trusty
MAINTAINER Frederick A. Matsen, matsen@fhcrc.org

RUN apt-get -y update
RUN apt-get install -y software-properties-common gawk libgsl0-dev libsqlite3-dev libz-dev m4 make patch
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get update
RUN apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam

# configure opam
RUN opam init
RUN opam repo add pplacer-deps http://matsen.github.com/pplacer-opam-repository
RUN opam update pplacer-deps
RUN command -v ocamlc && ( ocamlc -version | grep -q 3.12.1 ) || opam switch install 3.12.1
RUN eval `opam config env`

# make pplacer
RUN git clone https://github.com/matsen/pplacer.git
WORKDIR /pplacer
RUN cat opam-requirements.txt | xargs opam install -y
RUN make
