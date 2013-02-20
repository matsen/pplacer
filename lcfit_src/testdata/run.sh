#!/bin/sh

set -e
set -u
set -x

MICRO_DEMO=../../../microbiome-demo
PPLACER=../../bin/pplacer

export LCFIT_TIMING=marg_like_timing.csv
export LCFIT_FIT=marg_like_fit.csv
export LCFIT_COMP=marg_like_comp.csv

$PPLACER -c $MICRO_DEMO/vaginal_16s.refpkg $MICRO_DEMO/src/p4z1r36.fasta -p

Rscript plot_timing.R
Rscript plot_marg.R
Rscript plot_fits.R
