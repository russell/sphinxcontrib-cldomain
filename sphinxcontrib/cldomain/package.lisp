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


(in-package :cl-user)

(defpackage :sphinxcontrib.cldomain
  (:use #:closer-common-lisp #:json
        #:closer-mop #:alexandria)
  (:export #:main
           #:encode-symbol
           #:resolve-symbol
           #:*current-package*
           #:intern*
           #:scope-symbols-in-text
           #:find-best-symbol
           #:print-packages))
