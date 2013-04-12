;; cldomain is a Common Lisp domain for the Sphinx documentation tool.
;; Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>

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

(defpackage :sphinxcontrib.cldomain-test
  (:use #:common-lisp #:fiveam #:sphinxcontrib.cldomain))

(in-package :sphinxcontrib.cldomain-test)

(def-suite :cldomain)

(in-suite :cldomain)

(test encode-symbol
  (is (equal '("COMMON-LISP:CAR" "COMMON-LISP:LIST")
             (mapcar #'encode-symbol '(car "COMMON-LISP:LIST")))))

(test resolve-symbol
  (let ((*current-package* *package*))
    (is (equal '(":cl:symbol:`~COMMON-LISP:CAR`"
                 ":cl:symbol:`~COMMON-LISP:LIST`"
                 "COMMON-LISP:LIST."
                 ":KEY-TEST.")
               (mapcar #'resolve-symbol '("CAR" "COMMON-LISP:LIST"
                                          "COMMON-LISP:LIST."
                                          ":KEY-TEST."))))))

(test find-best-symbol
  (let ((*current-package* *package*))
    (is (equal '(":cl:symbol:`~COMMON-LISP:LIST`" ".")
               (multiple-value-bind (symbol rest)
                   (find-best-symbol '("COMMON-LISP:LIST."
                                       "COMMON-LISP:LIST"))
                 (list symbol rest))))))

(test find-best-symbol-one
  (let ((*current-package* *package*))
    (is (equal ":cl:symbol:`~COMMON-LISP:LIST`"
               (find-best-symbol '("COMMON-LISP:LIST"))))))

(test intern*
  (is (equal '(list nil)
             (mapcar #'intern* '("COMMON-LISP:LIST" "LIST")))))
