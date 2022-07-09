Release 0.14 UNRELEASED
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

Release 0.13 06-09-2015
-----------------------
* updated com.dvlsoft.clon to net.didierverna.clon.

Release 0.12 24-02-2015
-----------------------
* fixed argument generation bug.

Release 0.11 30-12-2014
-----------------------
* support loading symbol information from multiple packages.

Release 0.10 12-06-2014
-----------------------
* added back parentheses to parameter lists.
* added type information to parameter list of methods.
* added links to other methods from a method docstring.
* fixed bug with macro documentation strings.
* added better keyword detection in documentation strings.
* fixed bug where symbols at the end of documentation
  strings were ignored.

Release 0.9 10-02-2014
----------------------
* fixed problem with version number generation.

Release 0.8 10-02-2014
----------------------
* fixed bug with lisps argument.
* removed dependency on swank.
* remove specializers symbols package if it's the current
  package.

Release 0.7 12-06-2013
----------------------
* started to make internals more modular.
* print specialisation for methods.
* add links to method specializers.
* added methods to index.

Release 0.6 22-04-2013
----------------------
* added more documentation.
* added better error handling when json fails to parse.
* methods can now pull documentation from their generic.

Release 0.5 20-04-2013
----------------------
* inherit environment when calling subprocesses.
* better handling of symbols in doc strings.

Release 0.4 19-04-2013
----------------------
* fixed some packaging bugs.
* made the data model more tolerant to missing symbols.
* fixed symbol resolving bug.
* added output of unused symbols.

Release 0.3 16-04-2013
-----------------------
* cleaned up specializer output.
* fixed bug when rendering specializers that have the form :KEYWORD
  SYMBOL.
* updated documentation.
* split out package code from lisp program.

Release 0.2 14-04-2013
-----------------------

* link between generics and specializers.
* ignore symbols in documentation if they are in the arg list.
* better Quicklisp support.
* handling of symbols that boarder on punctuation.

Release 0.1 UNRELEASED
----------------------

* initial prototype
