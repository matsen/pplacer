:tocdepth: 1

Compiling ``pplacer`` from source
=================================

.. important::

    We provide binaries on the `main pplacer site`_ and encourage their use.
    However, for those who want to be on the bleeding edge of development, here are
    instructions and scripts for setting up a compilation environment.

Compiling ``pplacer`` requires a number of libraries and tools to be installed:

* make
* OPAM (:ref:`see below <OPAM>`)
* `OCaml 3.12.1 or 4.01.0 <http://www.ocaml.org>`_ (installed via your package manager, compiling from source, or :ref:`OPAM`)
* patch
* m4
* gawk
* `GNU Scientific Library (GSL)`_ (on Linux, the static library ``libgsl.a`` is required)
* `libsqlite3 <http://www.sqlite.org>`_  (on Linux, the static library ``libsqlite3.a`` is required)
* zlib (on Linux, the static library ``libz.a`` is required)

On Debian/Ubuntu 12.04, everything but OPAM can be installed with:

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

.. _OPAM:

OPAM
^^^^

``pplacer`` uses the `OPAM package manager`_ for installing OCaml dependencies.
OPAM can also manage different versions of the OCaml compiler - ``pplacer`` is tested against version 3.12.1.

See the the `official instructions
<http://opam.ocaml.org/doc/Quick_Install.html>`_ for installation details.

Once OPAM is installed, it needs to be configured:

.. code-block:: bash

    # Install version 3.12.1 of the OCaml compiler version if it's not active
    command -v ocamlc && ( ocamlc -version | grep -q 3.12.1 ) || opam switch install 3.12.1

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
.. _OPAM package manager: http://opam.ocaml.org
