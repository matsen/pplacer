:tocdepth: 1

Compiling ``pplacer`` from source
=================================

.. important::

    We provide binaries on the `main pplacer site`_ and encourage their use.
    However, for those who want to be on the bleeding edge of development, here are
    instructions and scripts for setting up a compilation environment.

Compiling ``pplacer`` requires a number of libraries and tools to be installed:

* make
* `OCaml 3.12 or 4.01.0 <http://www.ocaml.org>`_
* patch
* m4
* gawk
* `GNU Scientific Library (GSL)`_ (on Linux, the static library ``libgsl.a`` is required)
* `libsqlite3 <http://www.sqlite.org>`_  (on Linux, the static library ``libsqlite3.a`` is required)
* zlib (on Linux, the static library ``libz.a`` is required)

On Debian/Ubuntu 12.04, these can be installed with:

.. code-block:: bash

    apt-get install -y \
      camlp4-extra \
      gawk \
      libgsl0-dev \
      libsqlite3-dev \
      libz-dev \
      m4 \
      make \
      ocaml \
      patch

OPAM
^^^^

``pplacer`` uses the `OPAM`_ package manager for installing OCaml dependencies.
See the the `official instructions
<http://opam.ocaml.org/doc/Quick_Install.html>`_ for installation details.

Once OPAM is installed, it needs to be configured:

.. code-block:: bash

    opam init
    opam repo add pplacer-deps http://matsen.github.com/pplacer-opam-repository
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
.. _OPAM: http://opam.ocaml.org
