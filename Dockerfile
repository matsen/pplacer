## pplacer-build
## Build pplacer from source

FROM ubuntu:14.04
RUN apt-get -y update
RUN apt-get -y install \
camlp4-extra=4.01.0-3ubuntu3.1 \
gawk=1:4.0.1+dfsg-2.1ubuntu2 \
libgsl0-dev=1.16+dfsg-1ubuntu1 \
libsqlite3-dev=3.8.2-1ubuntu2.1 \
zlib1g-dev=1:1.2.8.dfsg-1ubuntu1 \
m4=1.4.17-2ubuntu1 \
wget=1.15-1ubuntu1.14.04.4 \
ocaml=4.01.0-3ubuntu3.1 \
patch=2.7.1-4ubuntu2.4 \
build-essential=11.6ubuntu6 \
pkg-config=0.26-1ubuntu4 \
unzip=6.0-9ubuntu1.5 \
sqlite3=3.8.2-1ubuntu2.1 \
python=2.7.5-5ubuntu3 \
zip=3.0-8


RUN wget https://raw.githubusercontent.com/ocaml/opam/1.3.1/shell/opam_installer.sh -O - | sh -s /usr/local/bin
RUN ln -s /usr/local/bin/opam /usr/bin/opam && /usr/local/bin/opam init -y
RUN opam repo add pplacer-deps http://matsen.github.io/pplacer-opam-repository &&  opam update pplacer-deps && eval `opam config env`
RUN opam install -y depext.1.0.5 && opam depext -y \
csv.1.6 \
ounit.2.0.8 \
xmlm.1.2.0 \
mcl.12-068oasis4 \
batteries.2.8.0 \
ocaml-gsl.0.6.3 \
sqlite3.4.1.3 \
camlzip.1.05 \
&& opam install -y \
csv.1.6 \
ounit.2.0.8 \
xmlm.1.2.0 \
mcl.12-068oasis4 \
batteries.2.8.0 \
ocaml-gsl.0.6.3 \
sqlite3.4.1.3 \
camlzip.1.05 

RUN mkdir /pplacer && mkdir /pplacer/src && mkdir /data
WORKDIR /pplacer/src
COPY ./ /pplacer/src/

RUN eval $(opam config env) && make
RUN cp /pplacer/src/bin/* /usr/local/bin
WORKDIR /pplacer/src/bin/ 
RUN zip /pplacer.zip *
WORKDIR /pplacer/src/
RUN zip /pplacer.zip ./scripts/*
WORKDIR /pplacer/src/scripts
RUN python setup.py install

WORKDIR /data
RUN rm -r /pplacer/src/
