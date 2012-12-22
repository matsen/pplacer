#!/bin/sh
set -e

PREFIX=`pwd`/godi
PATH=$PREFIX/bin:$PREFIX/sbin:$PATH

# Make sure the GNU Scientific Library is installed before continuing.
gsl_config=`which gsl-config`
if [ ${#gsl_config} -gt 10  ]; then
  prefix=`gsl-config --prefix | grep "^/"`
  if [ -z $prefix ]; then
    echo "Unable to determine GNU scientific library installation prefix, exiting..."
    exit 1
  fi
  else
    echo ""
    echo "gsl-config (GNU scientific library binary) not found in your path. Is GSL installed?"
    exit 1
fi

CDN=http://c715892.r92.cf2.rackcdn.com

# install ocaml via GODI.
wget -O- $CDN/godi-rocketboost-20110811.tar.gz | tar xzf -
cd godi-rocketboost-20110811
./bootstrap --prefix=$PREFIX --section 3.12 --batch --no-stage2
echo "GODI_BASEPKG_PCRE=yes" >> $PREFIX/etc/godi.conf
./bootstrap_stage2

# build godi-available packages.
godi_perform -build godi-gsl
godi_perform -build godi-sqlite3
cd ..

# build batteries, camomile, and ocaml-csv.
wget $CDN/odb.ml
ocaml odb.ml \
    --have-perms \
    --configure-flags-global "--datadir $PREFIX/share" \
    oUnit batteries csv xmlm zip mcl

# build pplacer.
wget --no-check-certificate http://github.com/matsen/pplacer/tarball/master \
    -O- | tar xzf -
cd matsen-pplacer-*/
make
