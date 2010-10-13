#!/bin/bash

KERNEL=`uname -r`
OS=`uname -s`
DIRNAME=pplacer_v1.0_$OS.$KERNEL

rm -f $DIRNAME.tar.gz
mkdir $DIRNAME

for i in `find _build -regex .*native | sed 's/_build\///; s/.native//'`
do 
  cp _build/$i.native $DIRNAME/$i; 
done

tar -cf $DIRNAME.tar $DIRNAME
gzip $DIRNAME.tar
rm -rf $DIRNAME
