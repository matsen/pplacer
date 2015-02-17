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
# RUN command -v ocamlc && ( ocamlc -version | grep -q 3.12.1 ) || opam switch install 3.12.1
RUN eval `opam config env`

# install dependencies (note will have to re-run without cache if they change)
RUN apt-get install -y curl


RUN opam install -y csv.1.3.3 ounit.2.0.0 xmlm.1.2.0 mcl.12-068oasis4 batteries.2.3.1 ocaml-gsl.0.6.3 sqlite3.1.6.3 camlzip.1.05


# RUN curl https://raw.githubusercontent.com/matsen/pplacer/master/opam-requirements.txt | xargs opam install -y

# make pplacer
CMD git clone https://github.com/matsen/pplacer.git && \
    cd pplacer && \
    make
