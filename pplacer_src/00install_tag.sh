#!/bin/bash

TAG=`git describe`

sed -e "s/VERSION/$TAG/" version_pre.ml > version.ml

cat version.ml
