;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;; cldomain is a Common Lisp domain for the Sphinx documentation tool.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defsystem :sphinxcontrib.cldomain
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:unix-opts :cl-json :alexandria :closer-mop
               #+sbcl :sb-introspect)
  :defsystem-depends-on (:asdf)
  :entry-point "sphinxcontrib.cldomain:main"
  :description "A documentation tool."
  :components ((:static-file "sphinxcontrib.cldomain.asd")
               (:file "package")
               (:file "cldomain" :depends-on ("package"))))


(defmethod perform ((op asdf:test-op) (system (eql (find-system :sphinxcontrib.cldomain))))
  (asdf:oos 'asdf:load-op :sphinxcontrib.cldomain-test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cldomain))


(defsystem :sphinxcontrib.cldomain/test
  :depends-on (:swank :sphinxcontrib.cldomain :fiveam :uiop)
  :defsystem-depends-on (:asdf)
  :description "A tests for the sphinxcontrib.cldomain documentation tool."
  :components ((:file "test")))
