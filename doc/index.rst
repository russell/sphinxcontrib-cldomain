About
=====

CLDomain is an extension for the Sphinx documentation generation tool
that allow sphinx to generate documentation for Common Lisp libraries.

Hyperspec is a cross referencing extension that supports linking to
the hyperspec.

Installation
============

Requirements
------------

* `Sphinx <http://sphinx-doc.org/>`_ 3.21.7
* `cl-launch <http://cliki.net/cl-launch>`_ 3.21.7
* `asdf <http://common-lisp.net/project/asdf/>`_ 3.1
* `quicklisp <http://www.quicklisp.org/beta/>`_
* `pygments-cl-repl <https://pypi.python.org/pypi/pygments-cl-repl>`_


Download
--------

Releases are hosted on `github`_ or `pypi`_.  The `source`_ is also available.


.. code-block:: sh

    pip install sphinxcontrib-cldomain

.. _github: https://github.com/russell/sphinxcontrib-cldomain/releases
.. _pypi: https://pypi.python.org/pypi/sphinxcontrib-cldomain
.. _source: https://github.com/russell/sphinxcontrib-cldomain

Configuration
-------------

Edit you `conf.py` file and add CLDomain and Hyperspec to your enabled
extensions.

.. code-block:: python

   extensions.extend(['sphinxcontrib.cldomain',
                      'sphinxcontrib.hyperspec'])


System symbols to load
^^^^^^^^^^^^^^^^^^^^^^

The path to each package needs to the lisp_packages configuration
option.  In this example the conf.py is in a doc directory and the ASD
file is in the parent directory.

The valid keys for each of the systems in the systems dict:

**name**
   The name of the system to load.

**path**
   The path to the system.

**packages**
   A list of the packages to extract symbol information from.

.. code-block:: python

   from os.path import join, dirname, realpath
   cl_systems = [{"name": "cl-git",
                  "path": join(dirname(realpath(__file__)), "../")),
                  "packages": ["cl-git"]}]

Quicklisp Location
^^^^^^^^^^^^^^^^^^

To set the location of quicklisp in conf.py add a quicklisp variable
with the value set to it's location.

.. code-block:: python

   from os.path import expandvars
   cl_quicklisp = expandvars('$HOME/quicklisp/')

LISP to use
^^^^^^^^^^^

To configure a specific lisp executable search order use.

.. code-block:: python

   cl_lisps = "sbcl ecl"


Documentation
=============

All directives support the ``nodoc`` option that will prevent them
from pulling the documentation string from Common Lisp.  Argument
lists and specializers will still be printed::

       .. cl:macro:: example-macro
          :nodoc:

          No documentation

.. cl:macro:: example-macro
   :nodoc:

   No documentation

Package
-------

.. rst:directive:: .. cl:package:: symbol-name

   The cl:package directive specifies the package that all the subsequent
   directives will look up when trying to resolve a symbol.::

      .. cl:package:: sphinxcontrib.cldomain.doc

.. cl:package:: sphinxcontrib.cldomain.doc


Variable
--------

.. rst:directive::  .. cl:variable:: symbol-name

   The cl:variable directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:variable:: *example-variable*

Example:

.. cl:variable:: *example-variable*

   extra description can be appended to further explain the
   functionality.  This doesn't need to appear in the lisp
   code. Instead it can be added to the rst files and it will be
   appended to the documentation.

Function
--------

.. rst:directive:: .. cl:function:: symbol-name

   The cl:function directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:function:: example-function

Example:

.. cl:function:: example-function


Macro
--------

.. rst:directive:: .. cl:macro:: symbol-name

   The cl:macro directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:macro:: example-macro

Example:

.. cl:macro:: example-macro


Class
-----

.. rst:directive:: .. cl:class:: symbol-name

   The cl:function directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:class:: example-class

Example:

.. cl:type:: example-class


Generics
--------

.. rst:directive:: .. cl:generic:: symbol-name

   The cl:generic directive will resolve the arguments and
   documentation from the common lisp definition.  It will also
   accumulate a list of the specialises and link to the types that
   this generic specialises on.::

       .. cl:generic:: example-generic

Example:

.. cl:generic:: example-generic


Methods
-------

.. rst:directive:: .. cl:method:: symbol-name (specializer)

   The cl:method directive will resolve the arguments and
   documentation from the common lisp dbenigntion::

       .. cl:method:: example-generic example-class :test

   For the time being all specializing arguments that aren't in the
   current package need to be qualified with a package.  E.g
   ``common-lisp:t``

   If you would like to prevent the method from resolving to the
   generics forms documentation string this can be suppressed using
   the ``noinherit`` option like::

       .. cl:method:: example-generic example-class :test
          :noinherit:

Example:

.. cl:method:: example-generic example-class :test

Multiple Packages
-----------------

.. cl:package:: sphinxcontrib.cldomain.doc-alt


.. rst:directive:: .. cl:function:: symbol-name

   The cl:function directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:package:: sphinxcontrib.cldomain.doc-alt

       .. cl:function:: example-function

Example:

.. cl:package:: sphinxcontrib.cldomain.doc-alt

.. cl:function:: example-function


Changelog
=========

.. include:: ../CHANGELOG.rst
