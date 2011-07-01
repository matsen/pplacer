#!/bin/sh

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

#install ocaml via GODI
wget http://download.camlcity.org/download/godi-rocketboost-20091222.tar.gz
tar xzf godi-rocketboost-20091222.tar.gz
cd godi-rocketboost-20091222
./bootstrap --prefix=$PREFIX
echo "GODI_BASEPKG_PCRE=yes" >> $PREFIX/etc/godi.conf
./bootstrap_stage2

#build godi packages
godi_perform -build godi-ocamlgsl
godi_perform -build godi-xml-light
godi_perform -build godi-ocaml-csv
godi_perform -build godi-ounit
godi_perform -build godi-sqlite3
cd ..

#build pplacer
wget -O matsen-pplacer.tar.gz http://github.com/matsen/pplacer/tarball/master
tar -xzf matsen-pplacer.tar.gz
cd matsen-pplacer-*/
make


