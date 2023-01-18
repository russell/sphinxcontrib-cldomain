0.17.1 - 2023-01-18
-------------------
Minor Fixes
~~~~~~~~~~~

* remove clear_doc method

0.17.0 - 2023-01-18
-------------------
Features
~~~~~~~~

* refactor object backend
* cleanup generic/method linking
* update the generic linking so it's less obtrusive
* setf expander support

Minor Fixes
~~~~~~~~~~~

* cleanup specializer handling

Documentation
~~~~~~~~~~~~~

* changelog had the wrong title headings

Build Tooling
~~~~~~~~~~~~~

* add example envrc

0.16.2 - 2023-01-08
-------------------

Minor Fixes
~~~~~~~~~~~

* add missing roswell file

0.16.1 - 2023-01-08
-------------------

Minor Fixes
~~~~~~~~~~~

* add back files missing from dist

0.16.0 - 2023-01-08
-------------------

Features
~~~~~~~~

* rename type to class

Minor Fixes
~~~~~~~~~~~

* fix method arguments in PDF output closes `#7
  <https://github.com/russell/sphinxcontrib-cldomain/issues/7>`_
* fix dictionary changed size during iteration
* rename type to class, in reality we are documenting classes, not
  types.
* bump pants to 2.14.0
* remove list_unused_symbols
* disable more warnings

Documentation
~~~~~~~~~~~~~

* add PDF and Info examples to documentation
* update changelog
* update bugtracker and documentation url
* fix sphinx url
* fix reference to pdf

Tests
~~~~~

* add tests for types, clos classes
* hookup lisp tests

Build Tooling
~~~~~~~~~~~~~

* migrate from pants to pyproject for building

Cleanups
~~~~~~~~

* modernise system definition

0.15.3 - 2022-07-24
-------------------
* assign *TRACE-OUTPUT* and *DEBUG-IO* to *ERROR-OUTPUT*

0.15.2 - 2022-07-24
-------------------
* fix don't decode bytes before writing them

0.15.1 - 2022-07-24
-------------------
* fix decode bytes before writing them

0.15 - 2022-07-23
-----------------
* stop qualifying lambda list symbols with a package
* fix display of method specializer links #16
* fix labelling of link back to generic

0.14 - 2022-07-10
-----------------
* convert to unix-opts, because i couldn't get clon to work
* strip packages from symbols if it's the current package, so
  CL-GIT::BODY would become BODY.
* add whitespace between method arguments so method ``(full-name
  (objectreference))`` will print as method ``(full-name (object
  reference))``
* symbols that a appear at the start of newlines are now correctly
  rendered, this might break CLISP, but will work in SBCL.  The bug
  was introduced by trying to support CLISP, but i think valid
  rendering trumps multiplatform support for now.

0.13 - 2015-09-06
-----------------
* updated com.dvlsoft.clon to net.didierverna.clon.

0.12 - 2015-02-24
-----------------
* fixed argument generation bug.

0.11 - 2014-12-30
-----------------
* support loading symbol information from multiple packages.

0.10 - 2014-06-12
-----------------
* added back parentheses to parameter lists.
* added type information to parameter list of methods.
* added links to other methods from a method docstring.
* fixed bug with macro documentation strings.
* added better keyword detection in documentation strings.
* fixed bug where symbols at the end of documentation
  strings were ignored.

0.9 - 2014-02-10
----------------
* fixed problem with version number generation.

0.8 - 2014-02-10
----------------
* fixed bug with lisps argument.
* removed dependency on swank.
* remove specializers symbols package if it's the current
  package.

0.7 - 2013-06-12
----------------
* started to make internals more modular.
* print specialisation for methods.
* add links to method specializers.
* added methods to index.

0.6 - 2013-04-22
----------------
* added more documentation.
* added better error handling when json fails to parse.
* methods can now pull documentation from their generic.

0.5 - 2013-04-20
----------------
* inherit environment when calling subprocesses.
* better handling of symbols in doc strings.

0.4 - 2013-04-19
----------------
* fixed some packaging bugs.
* made the data model more tolerant to missing symbols.
* fixed symbol resolving bug.
* added output of unused symbols.

0.3 - 2013-04-16
----------------
* cleaned up specializer output.
* fixed bug when rendering specializers that have the form :KEYWORD
  SYMBOL.
* updated documentation.
* split out package code from lisp program.

0.2 - 2013-04-14
----------------

* link between generics and specializers.
* ignore symbols in documentation if they are in the arg list.
* better Quicklisp support.
* handling of symbols that boarder on punctuation.

0.1 - UNRELEASED
----------------

* initial prototype
