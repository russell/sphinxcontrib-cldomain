Release 0.15.4 UNRELEASED
-------------------------
* fix method arguments in PDF output closes `#7
  <https://github.com/russell/sphinxcontrib-cldomain/issues/7>`_
* fix dictionary filter bug
* add PDF and Info examples to documentation

Release 0.15.3 2022-07-24
-------------------------
* assign *TRACE-OUTPUT* and *DEBUG-IO* to *ERROR-OUTPUT*

Release 0.15.2 2022-07-24
-------------------------
* fix don't decode bytes before writing them

Release 0.15.1 2022-07-24
-------------------------
* fix decode bytes before writing them

Release 0.15 2022-07-23
-----------------------
* stop qualifying lambda list symbols with a package
* fix display of method specializer links #16
* fix labelling of link back to generic

Release 0.14 2022-07-10
-----------------------
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

Release 0.13 2015-09-06
-----------------------
* updated com.dvlsoft.clon to net.didierverna.clon.

Release 0.12 2015-02-24
-----------------------
* fixed argument generation bug.

Release 0.11 2014-12-30
-----------------------
* support loading symbol information from multiple packages.

Release 0.10 2014-06-12
-----------------------
* added back parentheses to parameter lists.
* added type information to parameter list of methods.
* added links to other methods from a method docstring.
* fixed bug with macro documentation strings.
* added better keyword detection in documentation strings.
* fixed bug where symbols at the end of documentation
  strings were ignored.

Release 0.9 2014-02-10
----------------------
* fixed problem with version number generation.

Release 0.8 2014-02-10
----------------------
* fixed bug with lisps argument.
* removed dependency on swank.
* remove specializers symbols package if it's the current
  package.

Release 0.7 2013-06-12
----------------------
* started to make internals more modular.
* print specialisation for methods.
* add links to method specializers.
* added methods to index.

Release 0.6 2013-04-22
----------------------
* added more documentation.
* added better error handling when json fails to parse.
* methods can now pull documentation from their generic.

Release 0.5 2013-04-20
----------------------
* inherit environment when calling subprocesses.
* better handling of symbols in doc strings.

Release 0.4 2013-04-19
----------------------
* fixed some packaging bugs.
* made the data model more tolerant to missing symbols.
* fixed symbol resolving bug.
* added output of unused symbols.

Release 0.3 2013-04-16
-----------------------
* cleaned up specializer output.
* fixed bug when rendering specializers that have the form :KEYWORD
  SYMBOL.
* updated documentation.
* split out package code from lisp program.

Release 0.2 2013-04-14
-----------------------

* link between generics and specializers.
* ignore symbols in documentation if they are in the arg list.
* better Quicklisp support.
* handling of symbols that boarder on punctuation.

Release 0.1 UNRELEASED
----------------------

* initial prototype
