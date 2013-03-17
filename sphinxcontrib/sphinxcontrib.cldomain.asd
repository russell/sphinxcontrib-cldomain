;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cldomain a Common Lisp domain for sphinx.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
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
  :depends-on (:swank :getopt :cl-json :alexandria :closer-mop)
  :defsystem-depends-on (:asdf)
  :description "A documentation tool."
  :components ((:static-file "sphinxcontrib.cldomain.asd")
               (:file "cldomain")))
