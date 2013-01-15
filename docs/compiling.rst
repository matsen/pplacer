Compiling pplacer from source
=============================

.. important::

    We provide binaries on the `main pplacer site`_ and encourage their use.
    However, for those who want to be on the bleeding edge of development, here are
    instructions and scripts for setting up a compilation environment.

Compiling ``pplacer`` requires a number of libraries and tools to be installed:

* `git <http://www.git-scm.com/>`_
* `GNU Scientific Library (GSL)`_
* `libsqlite3 <http://www.sqlite.org>`_
* make
* `OCaml 3.12 <http://www.ocaml.org>`_
* patch
* m4
* gawk

On Debian/Ubuntu, these can be installed with:

.. code-block:: bash

    apt-get install -y \
      camlp4-extra \
      gawk \
      git-core \
      libgsl0-dev \
      libsqlite3-dev \
      m4 \
      make \
      ocaml \
      patch


OPAM
^^^^

``pplacer`` uses `OPAM`_ for installing OCaml dependencies.
Binaries are provided for 64-bit Linux machines. For other architectures, see
the `OPAM installation instructions
<http://opam.ocamlpro.com/doc/Advanced_Install.html>`_.

Once OPAM is installed, it needs to be configured:

.. code-block:: bash

    opam init
    opam repo add pplacer-deps https://github.com/matsen/pplacer-opam-repository.git
    opam update pplacer-deps
    eval `opam config env`

Building ``pplacer``
^^^^^^^^^^^^^^^^^^^^

Once the dependencies above are installed, navigate to the checkout of
``pplacer``, and use OPAM to install the OCaml modules used by ``pplacer``:

.. code-block:: bash

    cd /path/to/pplacer
    # Install required OCaml packages
    cat opam-requirements.txt | xargs opam install -y

Finally, build ``pplacer``, ``guppy`` and ``rppr`` with::

    make

Now the binaries should be in the ``bin/`` directory. Put them in your
path and you are ready to go!

.. _GNU Scientific Library (GSL): http://www.gnu.org/s/gsl/
.. _main pplacer site: http://matsen.fhcrc.org/pplacer/
.. _OPAM: http://opam.ocamlpro.com
