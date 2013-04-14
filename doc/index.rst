cldomain
========

.. contents::

About
-----

CLDomain is an extension for the Sphinx documentation generation tool
that allow sphinx to generate documentation for Common Lisp libraries.

Hyperspec is a cross referencing extension that supports linking to
the hyperspec.

Download
--------

Since there is currently no released version, the source is available
at `github`_.

.. _github: https://github.com/russell/sphinxcontrib-cldomain

Requirements
------------

* Sphinx
* cl-launch
* quicklisp

Installation
------------

After installing add cldomain and hyperspec to your enabled
extensions. ::

   extensions.extend(['sphinxcontrib.cldomain',
                      'sphinxcontrib.hyperspec'])


The path to each package needs to the lisp_packages configuration
option.  In this example the conf.py is in a doc directory and the ASD
file is in the parent directory. ::

   from os.path import join, dirname, realpath
   cl_packages = {"cl-git": join(dirname(realpath(__file__)), "../")}

To set the location of quicklisp in conf.py add a quicklisp variable
with the value set to it's location. ::

   import os
   cl_quicklisp = path.expandvars('$HOME/.quicklisp/')


Documentation
-------------


Package
~~~~~~~

.. rst:directive:: cl:package

   The cl:package directive specifies the package that all the subsequent
   directives will look up when trying to resolve a symbol.::

      .. cl:package:: sphinxcontrib.cldomain.doc

.. cl:package:: sphinxcontrib.cldomain.doc


Function
~~~~~~~~

.. rst:directive:: cl:function

   The cl:function directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:function:: example-function

.. cl:function:: example-function


Class
~~~~~

.. rst:directive:: cl:class

   The cl:function directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:class:: example-class

.. cl:type:: example-class


Generics
~~~~~~~~

.. rst:directive:: cl:generic

   The cl:generic directive will resolve the arguments and
   documentation from the common lisp definition.  It will also
   accumulate a list of the specialises and link to the types that
   this generic specialises on.::

       .. cl:generic:: example-generic

.. cl:generic:: example-generic


Methods
~~~~~~~

.. rst:directive:: cl:method

   The cl:method directive will resolve the arguments and
   documentation from the common lisp definition.::

       .. cl:method:: example-generic

.. cl:method:: example-generic example-class :test
