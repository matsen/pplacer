#!/bin/bash

KERNEL=`uname -r`
OS=`uname -s`
DIRNAME=pplacer_v1.0_$OS.$KERNEL

rm -f $DIRNAME.tar.gz
cp -r bin $DIRNAME

tar -cf $DIRNAME.tar $DIRNAME
gzip $DIRNAME.tar
rm -rf $DIRNAME
