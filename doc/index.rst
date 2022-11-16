About
=====

CLDomain is an extension for the Sphinx documentation generation tool
that allow sphinx to generate documentation for Common Lisp libraries.
Documentation is extracted from the various entity's documentation
strings, loaded from ASDF systems and associated internal packages.

Hyperspec is a cross referencing extension that supports linking to
the hyperspec.

Installation
============

Requirements
------------

* `Sphinx <http://sphinx-doc.org/>`_
* `roswell <https://roswell.github.io/>`_
* `pygments-cl-repl <https://pypi.python.org/pypi/pygments-cl-repl>`_


Download
--------

Releases are available via `pypi`_ or as `git tags`_.  The `source`_ is also available.


.. code-block:: sh

    pip install sphinxcontrib-cldomain

.. _git tags: https://git.sr.ht/~rsl/sphinxcontrib-cldomain/refs
.. _pypi: https://pypi.python.org/pypi/sphinxcontrib-cldomain
.. _source: https://git.sr.ht/~rsl/sphinxcontrib-cldomain

Configuration
-------------

Configuring CLDomain involves two actions: (a) adding the extensions to the
extension list, (b) telling CLDomain the systems and packages to load.

.. _conf_py_block:
.. code-block:: python

  from os.path import join, dirname, realpath, expandvars

  # Extensions: add 'sphinxcontrib.cldomain' and 'sphinxcontrib.hyperspec',
  # just like this example:
  extensions = [
      'sphinx.ext.intersphinx',
      'sphinxcontrib.cldomain',
      'sphinxcontrib.hyperspec'
  ]

  # --- CL domain customizations:
  #
  # cl_systems: The systems and packages from which to extract documentation:
  #
  # name - The name of the system to load.
  # path - The path to the system.
  # packages - A list of the packages to extract symbol information from.
  #
  # Note: This conf.py sits in a subdirectory below ("../"), relative to where
  # the "my-system.asd" system description file lives:
  cl_systems = [{"name": "my-system",
                 "path": join(dirname(realpath(__file__)), "../"),
                 "packages": ["my-package-1", "my-package-2"]}]
  # Ensure that the default highlighting language is CL:
  highlight_language = 'common-lisp'

  # For developer debugging only (and the curious, although, it did kill the cat!)
  # Currently ``True`` or ``False`` to output the JSON collected from cl-launch.
  cl_debug = False

Documenting Your Common Lisp Code
=================================

Sphinx can output HTML, :download:`pdf <build/latex/cldomain.pdf>`,
:download:`info <build/texinfo/cldomain.info>`

To test the ``info`` file you can open it in Emacs using ``C-u C-h i
<filename>``.

Common Lisp docstrings
----------------------

CLDomain collects the documentation strings for the package-exported symbols in
each system enumerated in the ``cl_systems`` configuration variable, which
CLDomain appends to the symbol's signature. You can include additional
documentation after the directive and it will also get included in the
Spinx-generated output. The output template looks like:

    *type*: *signature*

    *symbol-docstring*

    Any additional text described in the RST files.

For an example, follow :ref:`this <variable2>` link or read on.


Don't include the docstring: :nodoc:
------------------------------------


Sometimes, you'd prefer to provide separate (non-docstring) documentation
instead of having CLDomain insert the Lisp docstrings.  That's what the
``:nodoc`` option does.

Note: Argument lists and specializers will still be printed, as in this
example::


      .. cl:macro:: example-macro
        :nodoc:

        No documentation from the ``example-macro`` documentation string.

Code:

.. code-block:: common-lisp

   (defmacro example-macro ((arg1 arg2) &body arg3)
     "The CL Domain will try and convert any uppercase symbols into
   reference for example EXAMPLE-FUNCTION or a hyperspec link LIST.  Any
   unmatched symbols are converted to literals as is ARG1, ARG2 and ARG3.
   Explicit package references will also help resolve symbol sources
   COMMON-LISP:CDR.  Keywords are also detected for example :TEST."
     arg3)


Output:

.. cl:macro:: example-macro
   :nodoc:

   No documentation from the ``example-macro`` documentation string.


Packages
--------

CLDomain, like Common Lisp, needs to know the current package when resolving symbols. The
``:cl:package:`` directive is the CLDomain equivalent of ``(in-package ...)``. You can switch
between packages at any time in the documentation file using this directive.

.. rst:directive:: .. cl:package:: package

   Use ``package`` as the package name when resolving symbols to documentation::

      .. cl:package:: sphinxcontrib.cldomain.doc

   For multi-package documentation in the same Sphinx documentation file::

      .. cl:package:: sphinxcontrib.cldomain.doc

      documentation... documentation... documentation...

      .. cl:package:: org.coolness.my.code

      foo... bar... baz... lemon odor quux!!!


.. cl:package:: sphinxcontrib.cldomain.doc


Variables
---------

.. rst:directive::  .. cl:variable:: symbol-name

   The cl:variable directive will resolve the arguments and documentation
   from the common lisp definition::

       .. cl:variable:: *example-variable*

Code:

.. code-block:: common-lisp

    (defvar *example-variable* "value"
      "This is an example variable.")

Output:

.. cl:variable:: *example-variable*

.. _variable2:

You can include additional text, which appears after the docstring (unless you
use the ``:nodoc:`` option)::

      .. cl:variable:: *example-variable-2*

         This variable requires more explanitory text after its docstring. Because,
         more text means more clarity and further explains the intent of the original
         software developer.

Code:

.. code-block:: common-lisp

  (defvar *example-variable-2* "another value"
    "This example has additional text.")

Output:

.. cl:variable:: *example-variable-2*

   This variable requires more explanitory text after its docstring. Because,
   more text means more clarity and further explains the intent of the original
   software developer.

Functions
---------

.. rst:directive:: .. cl:function:: symbol-name

   Outputs the function's signature (arguments)::

       .. cl:function:: example-function

.. _hyperspec_example:

Code:

.. code-block:: common-lisp

  (defun example-function (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
    "The CL Domain will try and convert any uppercase symbols into
  reference for example EXAMPLE-FUNCTION, EXAMPLE-GENERIC or a hyperspec
  link LIST.  Any unmatched symbols are converted to literals as is
  ARG1, ARG2 and ARG3.  Explicit package references will also help
  resolve symbol sources COMMON-LISP:CAR.  Keywords are also detected
  for example :KEYWORD."
    (list arg1 arg2 arg3))

Output:

.. cl:function:: example-function


Macros
------

.. rst:directive:: .. cl:macro:: symbol-name

   Emit the macro's signature and documentation::

       .. cl:macro:: example-macro


Code:

.. code-block:: common-lisp

   (defmacro example-macro ((arg1 arg2) &body arg3)
     "The CL Domain will try and convert any uppercase symbols into
   reference for example EXAMPLE-FUNCTION or a hyperspec link LIST.  Any
   unmatched symbols are converted to literals as is ARG1, ARG2 and ARG3.
   Explicit package references will also help resolve symbol sources
   COMMON-LISP:CDR.  Keywords are also detected for example :TEST."
     arg3)

Output:

.. cl:macro:: example-macro


Types (aka CLOS Classes)
------------------------

.. rst:directive:: .. cl:type:: symbol-name

   The ``:cl:type:`` directive emits Common Lisp Object System (CLOS) class documentation::

       .. cl:type:: example-class

   The ``:noinitargs:`` option can be specified to exclude the class' list of ``:initarg``
   initialzers that are ordinarily included in the class' signature::

       .. cl:type:: example-class
          :noinitargs:


   Note: There is no mechanism or directive to document individual slots at the moment.

Code:

.. code-block:: common-lisp

  (defclass example-class ()
    ((slot1 :initarg :slot1 :accessor slot1
            :initform "default"
            :documentation "the first slot.")
     (slot2 :initarg :slot2 :accessor slot2
            :documentation "the second slot."))
    (:documentation "An example class."))

Output:

.. cl:type:: example-class


Generics and Methods
--------------------

.. rst:directive:: .. cl:generic:: symbol-name

   The ``:cl:generic:`` directive emits the documentation for a generic function and
   its specializers::

       .. cl:generic:: example-generic

Code:

.. code-block:: common-lisp

  (defgeneric example-generic (arg1 arg2 &optional arg3)
    (:documentation "A test generic function."))

Output:

.. cl:generic:: example-generic


.. rst:directive:: .. cl:method:: symbol-name (specializer)

   The ``:cl:method`` emits the documentation for generic method specializers::

       .. cl:method:: example-generic example-class :test

   For the time being, all specializing arguments that aren't in the current
   package must be qualified with a package, e.g., ``common-lisp:t``

Code:

.. code-block:: common-lisp

  (defmethod example-generic ((arg1 example-class) (arg2 (eql :test)) &optional arg3)
    "This is the first specialized version of example-generic."
    (list arg1 arg2 arg3))

Output:

.. cl:method:: example-generic example-class :test


Disable inheriting documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: The output for a specializing method will include its parent
generic function's documentation string if there is no documentation
for the method, i.e., specializing methods will inherit their parent
generic's docstring. The ``:noinherit:`` option suppresses this
behavior and will result in no docstring::

       .. cl:method:: example-generic example-class :test1
          :noinherit:

.. cl:method:: example-generic example-class :test1
   :noinherit:


Disable listing specializers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: all methods will also list other specializing methods by default
this behaviour can be disabled by passing the ``::nospecializers::``
option::

       .. cl:method:: example-generic example-class :test
          :nospecializers:

.. cl:method:: example-generic example-class :test
   :nospecializers:


Linking back to the generic
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: A link to the generic function can be added by specifying
the ``::linkgeneric::` option::

       .. cl:method:: example-generic example-class :test
          :linkgeneric:

.. cl:method:: example-generic example-class :test
   :linkgeneric:


Cross-references
----------------

You can cross reference Lisp entities using the following CLDomain Sphinx
roles, which results in a hyperlinked reference to the matching identifier, if
found:

.. rst:role:: cl:function

   References a function, as in ``:cl:function:`example-function``` (link:
   :cl:function:`example-function`).

.. rst:role:: cl:generic

   References a generic function, as in ``:cl:generic:`example-generic```
   (link: :cl:generic:`example-generic`).

.. rst:role:: cl:macro

   References a macro, as in ``:cl:macro:`example-macro``` (link:
   :cl:macro:`example-macro`).

.. rst:role:: cl:variable

   References a variable, as in ``:cl:variable:`*example-variable*``` (link:
   :cl:variable:`*example-variable*`).

.. rst:role:: cl:type

   References a type/CLOS class, as in ``:cl:type:`example-class``` (link:
   :cl:type:`example-class`).

.. FIXME rst:role:: cl:method
.. FIXME
.. FIXME   References a generic-specializing method, as in
.. FIXME``:cl:method:`example-generic``` (link: :cl:method:`example-generic
.. FIXME example-class :test`).


.. rst:role:: cl:symbol

   References a symbol, such as ``:cl:symbol:example-function`` (link: :cl:symbol:`example-function`).

Hyperspec References
--------------------

Generating a reference is very easy (and you've probably noticed already if you've read the Common Lisp
code snippets used to generate the examples). To generate a Hyperspec reference:

1. THE COMMON LISP SYMBOL NAME IS IN ALL CAPS, LIKE LIST OR FORMAT. (No, the
   documentation isn't shouting at you. It's the normal Lisp convention for
   symbols.

2. Prefix the symbol name with ``COMMON-LISP:``, e.g., ``COMMON-LISP:CAR``

The :ref:`cl:function: example<hyperspec_example>` has an example of Hyperspec-ing in its example code.


Changelog
=========

.. include:: ../CHANGELOG.rst
