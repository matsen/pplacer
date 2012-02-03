:tocdepth: 3

.. _scripts:

=======
scripts
=======


The `pplacer` package comes with a few scripts to perform common tasks on
reference packages and placements:

Installing
==========

All scripts can be used by specifying the full path.  For convenience, a Python
``setup.py`` file is provided, which will install them globally.  To install,
run::

   $ python setup.py install

from the ``scripts/`` subdirectory, prefixed with ``sudo`` if the python
installation directory is not writable.

``refpkg_align.py``
===================

``refpkg_align.py`` works with reference package alignments and alignment
profiles, providing methods to align sequences to a reference package
alignment, and extract an alignment from a reference package.

``refpkg_align.py`` depends on `BioPython <http://www.biopython.org/>`_, as
well as the external tools used for alignment: HMMER3_, Infernal_, and PyNAST_.


.. _HMMER3: http://hmmer.janelia.org
.. _Infernal: http://infernal.janelia.org
.. _PyNAST: http://pynast.sourceforge.net
List of subcommands
-------------------

``align``
*********

``refpkg_align.py align`` aligns sequences to a reference package alignment for
use with pplacer.  For reference packages built with Infernal_, ``cmalign`` is
used for alignment. For packages built using HMMER3_, alignment is performed
with ``hmmalign``. Reference packages lacking a ``profile`` entry are aligned
using PyNAST_.  By default, an alignment :ref:`mask` is applied if it exists.

The output format varies: Stockholm for Infernal- and HMMER-based reference
packages, FASTA for all others.

For Infernal-based reference packages, MPI may be used.

::

    usage: refpkg_align.py align [options] refpkg seqfile outfile

Options
^^^^^^^

::

      -h, --help            show this help message and exit
      --align-opts OPTS     Alignment options, such as "--mapali $aln_sto". '$'
                            characters will need to be escaped if using template
                            variables. Available template variables are $aln_sto,
                            $profile. Defaults are as follows for the different
                            profiles: (PyNAST: "-l 150 -f /dev/null -g /dev/null")
                            (INFERNAL: "-1 --hbanded --sub --dna")
      --alignment-method {PyNAST,HMMER3,INFERNAL}
                            Profile version to use. [default: Guess. PyNAST is
                            used if a valid CM or HMM is not found in the
                            reference package.]
      --no-mask             Do not trim the alignment to unmasked columns.
                            [default: apply mask if it exists]
      --debug               Enable debug output
      --verbose             Enable verbose output

      MPI Options

      --use-mpi             Use MPI [infernal only]
      --mpi-arguments MPI_ARGUMENTS
                            Arguments to pass to mpirun
      --mpi-run MPI_RUN     Name of mpirun executable

``extract``
***********

``extract`` extracts a reference alignment from a reference package, apply a
:ref:`mask` if it exists by default.

::

    usage: refpkg_align.py extract [options] refpkg output_file

Options
^^^^^^^

::

    positional arguments:
      refpkg                Reference package directory
      output_file           Destination

    optional arguments:
      -h, --help            show this help message and exit
      --output-format OUTPUT_FORMAT
                            output format [default: stockholm]
      --no-mask             Do not apply mask to alignment [default: apply mask
                            if it exists]


.. _mask:

mask
----

*Warning:* masking is experimental and we may change our mind about how it gets
implemented.

Alignment masks may be specified through an entry named "mask" in the
``CONTENTS.json`` file of a reference package pointing to a file with a
comma-delimited set of 0-based indices in an alignment to **keep** after
masking.

For example, a mask specification of:

    ``0,1,2,3,4,5,6,28,29``

Would discard all columns in an alignment except for 0-7, 28, and 29.

``sort_placefile.py``
=====================

``sort_placefile.py`` takes a placefile and sorts and formats its contents for
then performing a visual diff of placefiles. Output defaults to being emitted
to stdout.

::

    usage: sort_placefile.py [-h] [-o FILE] infile

..

``update_refpkg.py``
====================

``update_refpkg.py`` updates a reference package from the 1.0 format to the 1.1
format. It takes the ``CONTENTS.json`` file in the reference package as its
parameter and updates it in place, after making a backup copy.

::

    usage: update_refpkg.py [-h] CONTENTS.json

..

``check_placements.py``
=======================

``check_placements.py`` checks a placefile for potential issues, including:

 * Any ``like_weight_ratio`` being equal to 0.
 * The sum of the ``like_weight_ratios`` not being equal to 1.
 * Any ``post_prob`` being equal to 0.
 * The sum of the ``post_probs`` being equal to 0.
 * The sum of the ``post_probs`` not being equal to 1.

::

    usage: check_placements.py example.jplace

..

.. _deduplicate-sequences:

``deduplicate_sequences.py``
============================

``deduplicate_sequences.py`` deduplicates a sequence file and produces a dedup
file suitable for use with ``guppy redup -m``. See the
:ref:`redup <guppy_redup>` documentation for details.
