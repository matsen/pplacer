====
rppr
====

:Authors: Erick Matsen and Aaron Gallagher
:Title: rppr
:Version: 1.1
:License: GPL v3
:Date: September 2011

|rppr| is a binary to help prepare reference packages and select sequences.
It's pronounced "ripper" and is short for Reference Package PReparer.


Usage
=====

Command line interface
----------------------
The general way to invoke |rppr| is ``rppr COMMAND [options] placefile[s]`` where COMMAND is one of the |rppr| commands.
For example::

  rppr info -c some.refpkg

These programs are listed with more detail below, and can always be found using ``rppr --cmds`` .

|rppr| can also be invoked as ``rppr --quiet COMMAND [...]``,  which prevents
the specified command from writing to stdout unless explicitly requested.

List of subcommands
===================

The following table provides links to more in-depth documentation for each
rppr subcommand:

.. command-table

.. toctree::
   :glob:
   :hidden:

   rppr_*


.. |rppr| replace:: ``rppr``
.. |guppy| replace:: ``guppy``
.. |pplacer| replace:: ``pplacer``
.. |KR| replace:: Kantorovich-Rubinstein

.. vim:set ai fo+=n fo-=l ft=rst:

