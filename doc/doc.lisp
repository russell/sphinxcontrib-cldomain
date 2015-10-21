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

(defpackage :sphinxcontrib.cldomain.doc
  (:use #:common-lisp)
  (:export #:example-function
           #:example-class
           #:example-macro
           #:*example-variable*
	   #:*example-variable-2*
           #:example-generic))

(in-package :sphinxcontrib.cldomain.doc)

(defvar *example-variable* "value"
  "This is an example variable.")

(defvar *example-variable-2* "another value"
  "This example has additional text.")

(defun example-function (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
  "The CL Domain will try and convert any uppercase symbols into
reference for example EXAMPLE-FUNCTION, EXAMPLE-GENERIC or a hyperspec
link LIST.  Any unmatched symbols are converted to literals as is
ARG1, ARG2 and ARG3.  Explicit package references will also help
resolve symbol sources COMMON-LISP:CAR.  Keywords are also detected
for example :KEYWORD."
  (list arg1 arg2 arg3))


(defclass example-class ()
  ((slot1 :initarg :slot1 :accessor slot1
          :initform "default"
          :documentation "the first slot.")
   (slot2 :initarg :slot2 :accessor slot2
          :documentation "the second slot."))
  (:documentation "An example class."))


(defgeneric example-generic (arg1 arg2 &optional arg3)
  (:documentation "A test generic function."))


(defmethod example-generic ((arg1 example-class) (arg2 (eql :test)) &optional arg3)
    "This is the first specialized version of example-generic."
    (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 (eql :test1)) &optional arg3)
  "The second test method."
  (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 (eql :test2)) &optional arg3)
  "The third test method."
  (list arg1 arg2 arg3))

(defmethod example-generic ((arg1 example-class) (arg2 t) &optional arg3)
  "The third test method."
  (list arg1 arg2 arg3))


(defmacro example-macro ((arg1 arg2) &body arg3)
  "The CL Domain will try and convert any uppercase symbols into
reference for example EXAMPLE-FUNCTION or a hyperspec link LIST.  Any
unmatched symbols are converted to literals as is ARG1, ARG2 and ARG3.
Explicit package references will also help resolve symbol sources
COMMON-LISP:CDR.  Keywords are also detected for example :TEST."
  arg3)


(defpackage :sphinxcontrib.cldomain.doc-alt
  (:use #:common-lisp)
  (:export #:example-function))

(in-package :sphinxcontrib.cldomain.doc-alt)

(defun example-function (arg1 arg2 &optional (arg3 #'sort) &key (kw *example-variable*))
  "Symbols can be documented from multiple packages.  References to symbols be resolved in the context of the current package for example EXAMPLE-FUNCTION will resolve to the SPHINXCONTRIB.CLDOMAIN.DOC-ALT symbol."
  (list arg1 arg2 arg3))
