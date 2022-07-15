;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
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

(defpackage :sphinxcontrib.cldomain/test
  (:use #:closer-common-lisp #:fiveam #:sphinxcontrib.cldomain))

(in-package :sphinxcontrib.cldomain/test)

(def-suite :cldomain)

(in-suite :cldomain)

(test encode-symbol
  (is (equal (mapcar #'encode-symbol '(car "COMMON-LISP:LIST"))
             '("COMMON-LISP:CAR" "COMMON-LISP:LIST"))))

(test find-best-symbol
  (let ((*current-package* *package*))
    (is (equal '(list ".")
               (multiple-value-bind (symbol literal rest)
                   (find-best-symbol '("COMMON-LISP:LIST."
                                       "COMMON-LISP:LIST"))
                   (declare   (ignore literal))
                 (list symbol rest))))))

(test find-best-symbol-one
  (let ((*current-package* *package*))
    (is (equal 'list
               (find-best-symbol '("COMMON-LISP:LIST"))))))

(test intern*
  (is (equal '(list nil)
             (mapcar #'intern* '("COMMON-LISP:LIST" "LIST")))))

(test scope-symbols-in-text
  (let ((*current-package* *package*))
    (is (equal "example text :cl:symbol:`~COMMON-LISP:LIST` ``CAR`` ignore MORE text."
               (scope-symbols-in-text
                "example text LIST CAR ignore MORE text." '(car))))))

(test scope-symbols-in-text-with-newlines
  (let ((*current-package* *package*))
    (is (equal "example text :cl:symbol:`~COMMON-LISP:LIST` ``CAR`` ``:KEY``
``:ANOTHER`` ignore MORE text."
               (scope-symbols-in-text
                "example text LIST CAR :KEY
:ANOTHER ignore MORE text." '(car))))))



(defun run-tests ()
  (let ((result-list (fiveam:run :cldomain)))
    (explain! result-list)
    (if (remove-if
         (lambda (test) (equal (symbol-name (class-name (class-of test)))
                               "TEST-PASSED"))
         result-list)
        (uiop:quit 1)
        (uiop:quit 0))))
