;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cldomain a Common Lisp domain for sphinx.
;; Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>
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


(require 'asdf)

(let ((*standard-output* *error-output*))
  (asdf:oos 'asdf:load-op 'getopt))

(defun find-quicklisp ()
  "try and find quicklisp"
  (car
   (handler-bind
       ((file-error
         (lambda (c)
           (declare (ignore c))
           (invoke-restart 'skip-path))))
     (loop :for path
        :in (mapcar (lambda (path)
                      (merge-pathnames (make-pathname :directory (list :relative path))
                                       (user-homedir-pathname)))
                    (list "quicklisp" ".quicklisp"))
        :when (restart-case
                  (truename path)
                (skip-path () nil))
        :collect path))))

#-quicklisp
(let ((quicklisp-init (merge-pathnames (make-pathname :name "setup"
                                                      :type "lisp")
                                       (find-quicklisp))))
  (if (probe-file quicklisp-init)
    (load quicklisp-init)
    (error "Can't Find Quicklisp")))
