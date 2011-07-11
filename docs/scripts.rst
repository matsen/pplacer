:tocdepth: 3

.. _scripts:

=======
scripts
=======

The `pplacer` package comes with a few scripts to perform common tasks on
reference packages and placements:

``refpkg_align.py``
===================

``refpkg_align.py`` performs operations using an alignment within a reference
package.  The script can align an input sequence file to the reference
alignment using ``cmalign`` or ``hmmalign``, search for sequences in a file that
match an alignment using ``hmmsearch``, or simply extract the alignment from the
reference package in the format of your choosing.

``refpkg_align.py`` depends on both `BioPython <http://www.biopython.org/>`_
and `Taxtastic <http://github.com/fhcrc/taxtastic>`_.

List of subcommands
-------------------

``align``
*********

``align`` aligns input sequences to a reference package alignment. For packages
built with Infernal, ``cmalign`` is used. For packages built using HMMER3,
``hmmalign`` is used. By default, a :ref:`mask` is applied if it exists.

The output is stored in stockholm format.

::

    usage: refpkg_align.py align [options] refpkg seqfile outfile

Options
^^^^^^^

::

    positional arguments:
      refpkg                Reference package directory
      seqfile               Input file, in FASTA format.
      outfile               Output file

    optional arguments:
      -h, --help            show this help message and exit
      --align-opts OPTS     Alignment options, such as "--mapali $aln_sto". '$'
                            characters will need to be escaped if using template
                            variables. Available template variables are $aln_sto,
                            $profile. Defaults are as follows for the different
                            profiles: (hmmer3: "--mapali $aln_sto") (infernal1:
                            "-1 --hbanded --sub --dna")
      --profile-version {infernal1mpi,hmmer3,infernal1}
                            Profile version to use. [default: hmmer3]
      --no-mask             Do not trim the alignment to unmasked columns.
                            [default: apply mask if it exists]
      --debug               Enable debug output
      --verbose             Enable verbose output


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


``search-align``
****************

``search-align`` searches input sequences for matches to reference alignment,
aligning any high-scoring matches to reference alignment. By default, a
:ref:`mask` is applied if it exists.

The output is stored in stockholm format.

.. note::
    ``search-align`` is only available for reference packages created with HMMER3.

::

    usage: refpkg_align.py search-align [options] refpkg seqfile outfile

Options
^^^^^^^

::

    positional arguments:
      refpkg                Reference package directory
      seqfile               Input file, in FASTA format.
      outfile               Output file

    optional arguments:
      -h, --help            show this help message and exit
      --search-opts OPTS    search options, such as "--notextw --noali -E 1e-2"
                            Defaults are as follows for the different profiles:
                            (hmmer3: "--notextw --noali")
      --align-opts OPTS     Alignment options, such as "--mapali $aln_sto". '$'
                            characters will need to be escaped if using template
                            variables. Available template variables are $aln_sto,
                            $profile. Defaults are as follows for the different
                            profiles: (hmmer3: "--mapali $aln_sto") (infernal1:
                            "-1 --hbanded --sub --dna")
      --profile-version {infernal1mpi,hmmer3,infernal1}
                            Profile version to use. [default: hmmer3]
      --no-mask             Do not trim the alignment to unmasked columns.
                            [default: apply mask if it exists]
      --debug               Enable debug output
      --verbose             Enable verbose output



.. _mask:

mask
----

Alignment masks may be specified through an entry named "mask" in the
``CONTENTS.json`` file of a reference package pointing to a file with a
comma-delimited set of 0-based indices in an alignment to **keep** after
masking.

For example, a mask specification of:

    ``0,1,2,3,4,5,6,28,29``

Would discard all columns in an alignment except for 0-7, 28, and 29.
