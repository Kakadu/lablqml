********
Overview
********

Introduction
============

asdfasfasdf

-  1

-  2

-  3

The first section of this document defines some terms used in the rest
of this manual. The second section specifies the dune metadata
format and the third one describes how to use the ``dune`` command.

Terminology
===========

-  **package**: a package is a set of libraries, executables, ... that
   are built and installed as one by opam

-  **project**: a project is a source tree, maybe containing one or more
   packages

-  **root**: the root is the directory from where dune can build
   things. Dune knows how to build targets that are descendants of
   the root. Anything outside of the tree starting from the root is
   considered part of the **installed world**. How the root is
   determined is explained in :ref:`finding-root`.

