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

(defun symbols-to-local (symbols)
  (cond
    ((listp symbols)
     (mapcar #'symbols-to-local symbols))
    ((and (symbolp symbols) (eq (symbol-package symbols) (find-package 'KEYWORD)))
     symbols)
    ((symbolp symbols)
     (intern (symbol-name symbols)))
    (t symbols)))

(defun func-or-macro-args (sym-name sym-package lambda-list-func)
  (format nil "~S"
          (mapcar #'symbols-to-local
                  (funcall
                   lambda-list-func
                   (intern (subseq sym-name (1+ (length sym-package)))
                           sym-package)))))

(defun convert-classes-to-names (sym)
  (cond
    ((equal "EQL-SPECIALIZER" (symbol-name (class-name (class-of sym))))
     (slot-value sym 'SB-PCL::%TYPE))
    (t
     (swank-mop:class-name sym))))

(defun specializers (gf)
  (loop :for syms
        :in (mapcar #'swank-mop:method-specializers
                    (sb-mop:generic-function-methods gf))
        :collect (mapcar #'convert-classes-to-names syms)))

(defun symbol-args (symbol package type)
  "look up symbol args and convert them to a string."
  (case type
    (:function
     (func-or-macro-args symbol package #'swank::arglist))
    (:macro
     (func-or-macro-args symbol package #'swank::arglist))
    (:generic-function
     (func-or-macro-args symbol package #'swank::arglist))
    (otherwise "")))

(defun symbols-to-json (package)
  (let ((my-package (string-upcase package)))
    (let ((*standard-output* *error-output*))
      #+quicklisp
      (ql:quickload my-package :prompt nil)
      #-quicklisp
      (asdf:oos 'asdf:load-op my-package))
    (let* ((swank::*buffer-package* (find-package (intern (string-upcase "CL-USER"))))
           (swank::*buffer-readtable* (copy-readtable)))
      (let ((package-symbols nil))
        (json:with-object (*standard-output*)
          (dolist (sym (swank:apropos-list-for-emacs "" t nil my-package) package-symbols)
            (let* ((sym-name (cadr sym))
                   (sym-type (caddr sym))
                   (sym-docstring (cadddr sym)))
              (when (and (> (length sym-name) (length my-package))
                         (string= (subseq sym-name 0 (length my-package))
                                  (string-upcase my-package)))
                (json:as-object-member ((subseq sym-name (1+ (length my-package))) *standard-output*)
                  (json:with-object (*standard-output*)
                    (json:encode-object-member 'type sym-type)
                    (json:encode-object-member 'arguments
                                               (symbol-args sym-name my-package sym-type))
                    (when (eq sym-type :generic-function)
                      (json:encode-object-member 'specializers
                                                 (specializers (symbol-function (intern (subseq sym-name (1+ (length my-package))) my-package)))))
                    (json:encode-object-member 'docstring
                                               (if (eq :not-documented sym-docstring) "" sym-docstring))))))))))))

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
       (push (truename (pathname (cdr (assoc "path" args :test #'equalp))))
             asdf:*central-registry*)
       (symbols-to-json (cdr (assoc "package" args :test #'equalp))))
      (t
       (print-message "missing args")
       (print-usage)))))

(main)
