CLDomain
========

CLDomain is a Common Lisp domain for `Sphinx Documentation Generator`_.
Sphinx is a mulit-language tool this project extends it's
functionality to cover Common Lisp. The aim is to support
documentation with the same ease that a Python project.

Currently the project only supports inspection based documentation of
symbols, future versions will allow more manual creation of
documentation sections.

CLDomain is licensed under the `GPLv3`_. Please report any bugs in the
GitHub `Bug Tracker`_.

An example of the generated documentation can be found in the
`cl-git`_ project.

The current sample of a function definition looks like ::

   .. cl:function:: ensure-git-repository-exist
   
      :param path: the path to the git repository.
      :param bare: if truthful, then create a bare repository.
   
      .. code-block:: common-lisp
   
         CL-GIT> (ensure-git-repository-exist #p"/tmp/test-repo/")
         #P"/tmp/test-repo/"

and the `rendered example`_ shows the documentation string has been
introspected and so has the function definition.

TODO
----

* use cl-launch to generate a SHELL script to execute in a more
  portable way.
* Add support for classes and slots.
* Link to texinfo generated doc.
* document this project and provide an example.
* when parsing text from doc strings, package names should be added to
  un-scoped symbols.
* source view, link function to original source code.  (Could use
  linkcode, this can link to code in an external repository.  Viewcode
  generates static html versions so will require more extensions to support CL)
* remove parens from function call display.  To match the SBCL and
  TEXINFO generated documentation.

.. _rendered example: http://cl-git.russellsim.org/#CL-GIT:ENSURE-GIT-REPOSITORY-EXIST
.. _cl-git: http://cl-git.russellsim.org/
.. _Sphinx Documentation Generator: http://sphinx.pocoo.org/
.. _Bug Tracker: https://github.com/russell/sphinxcontrib-cldomain
.. _GPLv3: https://www.gnu.org/licenses/gpl-3.0-standalone.html

