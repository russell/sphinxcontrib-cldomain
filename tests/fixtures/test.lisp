;; cldomain is a Common Lisp domain for the Sphinx documentation tool.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :sphinxcontrib.cldomain.doc-test
  (:use #:common-lisp)
  (:export #:example-function
           #:example-function1
           #:example-class
           #:example-macro
           #:example-macro-ignored-doc
           #:*example-variable*
           #:*example-variable-2*
           #:example-generic))

(in-package :sphinxcontrib.cldomain.doc-test)

(defvar *example-variable* "value"
  "This is an example variable.")

(defvar *example-variable-2* "another value"
  "This example has additional text.")

(defun example-function (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
  "Example function CL Domain
Reference EXAMPLE-FUNCTION
Package references COMMON-LISP:CDR
Hyperspec LIST
Arguments ARG1, ARG2 and ARG3
Keyword :TEST"
  (list arg1 arg2 arg3))

(define-setf-expander example-function (arg1 arg2 &environment env)
  "Example setf with documentation."
  (declare (ignore arg1 arg2 env)))

(defun example-function1 (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
  "An example function"
  (list arg1 arg2 arg3))

(defun (setf example-function1) (arg1 arg2 value)
  "Example setf with documentation."
  (declare (ignore arg1 arg2 value)))

(defclass example-class ()
  ((slot1 :initarg :slot1 :accessor slot1
          :initform "default"
          :documentation "the first slot.")
   (slot2 :initarg :slot2 :accessor slot2
          :documentation "the second slot."))
  (:documentation "An example class."))


(defgeneric example-generic (arg1 arg2 &optional arg3)
  (:documentation "Example generic CL Domain
Reference EXAMPLE-FUNCTION
Package references COMMON-LISP:CDR
Hyperspec LIST
Arguments ARG1, ARG2 and ARG3
Keyword :TEST"))


(defmethod example-generic ((arg1 example-class) (arg2 (eql :test)) &optional arg3)
  "This is the first specialized version of example-generic."
  (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 (eql :test1)) &optional arg3)
  (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 (eql :test2)) &optional arg3)
  "The third test method."
  (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 t) &optional arg3)
  "The third test method."
  (list arg1 arg2 arg3))

(defgeneric (setf example-generic) (new-value arg1 arg2)
  (:documentation "This is the documentation for some setf magic"))

(defmethod (setf example-generic) (new-value (arg1 example-class) (arg2 (eql :test)))
  "first setf"
  (list arg1 arg2 arg3))

(defmethod (setf example-generic) ((new-value example-class) (arg1 example-class) (arg2 t))
  "first setf"
  (list arg1 arg2 arg3))


(defmacro example-macro ((arg1 arg2) &body arg3)
  "Example macro CL Domain
Reference EXAMPLE-FUNCTION
Package references COMMON-LISP:CDR
Hyperspec LIST
Arguments ARG1, ARG2 and ARG3
Keyword :TEST"
  arg3)

(defmacro example-macro-ignored-doc ((arg1 arg2) &body arg3)
  "This should be ignored"
  arg3)


(defpackage :sphinxcontrib.cldomain.doc-test-alt
  (:use #:common-lisp)
  (:export #:example-function))

(in-package :sphinxcontrib.cldomain.doc-test-alt)

(defun example-function (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
  "Symbols can be documented from multiple packages.  References to symbols be resolved in the context of the current package for example EXAMPLE-FUNCTION will resolve to the SPHINXCONTRIB.CLDOMAIN.DOC-ALT symbol."
  (list arg1 arg2 arg3))
