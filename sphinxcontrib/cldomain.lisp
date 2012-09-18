#!/usr/bin/sbcl --script
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

(require 'asdf)

(defpackage :cldomain
  (:use :common-lisp))
(in-package #:cldomain)

(let ((*standard-output* *error-output*))
  (asdf:oos 'asdf:load-op 'getopt))

(defun load-quicklisp ()
  ;; use the local quicklisp if it is there
  #-quicklisp
  (let* ((quicklisp-path
           (car (remove-if #'null
                           (mapcar #'probe-file
                                   (list (merge-pathnames "quicklisp/setup.lisp"
                                                          (user-homedir-pathname))
                                         (merge-pathnames ".quicklisp/setup.lisp"
                                                          (user-homedir-pathname)))))))
         (quicklisp-init quicklisp-path))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))


(load-quicklisp)
(let ((*standard-output* *error-output*))
  (eval '(ql:quickload 'swank))
  (eval '(ql:quickload 'cl-json)))
(defun argv ()
  (or
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun print-message (message)
  (format t "~A~%" message))

(defun print-usage ()
  (format t "./cldomain.lisp --package <package name> --path <package path> ~%"))

(defun main ()
  (multiple-value-bind (unused-args args invalid-args)
      (getopt:getopt (argv) '(("package" :required)("path" :required)))
    (cond
      (invalid-args
       (print-message "Invalid arguments")
       (print-usage))
      ((< 1 (length unused-args))
       (print-message "Unused args")
       (print-usage))
      ((= 2 (length args))
       (push (pathname (cdr (assoc "path" args :test #'equalp))) asdf:*central-registry*)
       (let ((my-package (string-upcase (cdr (assoc "package" args :test #'equalp)))))
         (let ((*standard-output* *error-output*))
           #+quicklisp
           (ql:quickload my-package :prompt nil)
           #-quicklisp
           (asdf:oos 'asdf:load-op my-package))
         (let* ((swank::*buffer-package* (find-package (intern (string-upcase "CL-USER"))))
                (swank::*buffer-readtable* (copy-readtable)))
           (json:encode-json-alist
            (let ((package-symbols nil))
              (dolist (sym (swank:apropos-list-for-emacs "" t nil my-package) package-symbols)
                (let* ((sym-name (cadr sym))
                       (sym-type (caddr sym))
                       (sym-docstring (cadddr sym)))
                  (when (and (> (length sym-name) (length my-package))
                             (string= (subseq sym-name 0 (length my-package))
                                      (string-upcase my-package)))
                    (labels ((symbols-to-local (symbols)
                               (cond
                                 ((listp symbols)
                                  (mapcar #'symbols-to-local symbols))
                                 ((and (symbolp symbols) (eq (symbol-package symbols) (find-package 'KEYWORD)))
                                  symbols)
                                 ((symbolp symbols)
                                  (intern (symbol-name symbols)))
                                 (t symbols))))
                      (let* ((sym-args (case sym-type
                                         (:FUNCTION
                                          (format nil "~S" (mapcar #'symbols-to-local
                                                                   (swank::%arglist (intern (subseq sym-name (1+ (length my-package))) my-package)))))
                                         (otherwise ""))))
                        (push (list (subseq sym-name (1+ (length my-package))) sym-type sym-args (if (eq :NOT-DOCUMENTED sym-docstring) "" sym-docstring)
                                            ) package-symbols)))))))))))
      (t
       (print-message "missing args")
       (print-usage)))))

(main)
