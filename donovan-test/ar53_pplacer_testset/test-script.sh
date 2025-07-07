#!/bin/bash

PPLACER_EXE="pplacer"  # default

# Parse options
for arg in "$@"; do
  case $arg in
    --pplacer=*)
      PPLACER_EXE="${arg#*=}"
      shift
      ;;
    *)
      # Skip unknown options or handle them
      ;;
  esac
done

# Add verbose
set -o verbose
set -e
trap 'echo -e "\033[0;31m[ERROR] Script failed at line $LINENO\033[0m"' ERR

# output json
OUTPUT_JSON=ar53_test_genomes.json
# "golden" ground truth output (generated from: pplacer v1.1.alpha19-0-g807f6f3)
GOLDEN_JSON=ar53_test_genomes.GOLDEN.json

# number of cpus
#N_CPU=32
N_CPU=2

# pplacer info
echo ${PPLACER_EXE}
which ${PPLACER_EXE}
${PPLACER_EXE} --version

# generate test json
${PPLACER_EXE} -m WAG -j ${N_CPU} -c gtdb_r226_ar53.refpkg -o ${OUTPUT_JSON} ar53-test_genomes.fasta.gz

# compare results
diff -w ${OUTPUT_JSON} ${GOLDEN_JSON}

