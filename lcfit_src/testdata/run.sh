#!/bin/sh

set -e
set -u
set -x

MICRO_DEMO=../../../microbiome-demo
PPLACER=../../bin/pplacer

$PPLACER -c $MICRO_DEMO/vaginal_16s.refpkg $MICRO_DEMO/src/p4z1r36.fasta -p

Rscript plot_timing.R
Rscript plot_marg.R
Rscript plot_fits.R
