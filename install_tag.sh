#!/bin/bash

TAG=`git describe`

sed -e "s/VERSION/$TAG/" pplacer_src/version_pre.ml > pplacer_src/version.ml

cat pplacer_src/version.ml
