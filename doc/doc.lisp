;; cldomain is a Common Lisp domain for the Sphinx documentation tool.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>

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
           #:example-function1
           #:example-class
           #:ex-generic))

(in-package :sphinxcontrib.cldomain.doc)


(defun example-function (arg1 arg2 arg3)
  "An example function, returns a list containing ARG1 ARG2 and ARG3."
  (list arg1 arg2 arg3))


(defun example-function1 (arg1 arg2 &optional (arg3 #'sort))
  "An example function similar to EXAMPLE-FUNCTION but with an
  optional argument that contains a default with a default."
  (list arg1 arg2 arg3))


(defclass example-class ()
  ((slot1 :initarg :slot1 :accessor slot1
          :initform "default"
          :documentation "the first slot.")
   (slot2 :initarg :slot2 :accessor slot2
          :documentation "the second slot."))
  (:documentation "An example class."))


(defgeneric ex-generic (arg1 arg2 &optional arg3)
  (:documentation "A test generic function."))


(defmethod ex-generic ((arg1 example-class) (arg2 (eql :test)) &optional arg3)
  (list arg1 arg2 arg3))
