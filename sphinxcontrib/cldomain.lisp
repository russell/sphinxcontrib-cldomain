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
  (asdf:oos 'asdf:load-op 'getopt)
)

(defun load-quicklisp ()
  ;; use the local quicklisp if it is there
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
  )


(load-quicklisp)


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
       (let ((my-package (intern (string-upcase (cdr (assoc "package" args :test #'equalp))))))
         (let ((*standard-output* *error-output*))
           #+quicklisp
           (ql:quickload my-package :prompt nil)
           #-quicklisp
           (asdf:oos 'asdf:load-op my-package))
         (json-documentation my-package)))

      (t
       (print-message "missing args")
       (print-usage)))))


(require 'sb-introspect)

(defun inspect-docstring (sym)
  (documentation sym (cond ((boundp sym)
			    'variable)
			   ((fboundp sym)
			    'function)
			   ((compiler-macro-function sym)
			    'macro))))


(defun inspect-arglist (sym)
  (cond ((boundp sym)
	 nil)
	((fboundp sym)
	 (sb-introspect:function-lambda-list sym))
	((compiler-macro-function sym)
	 (sb-introspect:function-lambda-list sym))))


(defparameter +json-lisp-escaped-chars+
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))


(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
       do (write-char ch stream)
     else
       do (let ((special '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
            (destructuring-bind (esc . (width . radix)) special
              (format stream "\\~C~V,V,'0R" esc radix width code)))))


(defun json-documentation (package-name)
  (let ((package (find-package package-name)))
    (princ "{")
    (let ((first-loop t))
      (do-external-symbols (sym package)
        (if first-loop (setq first-loop nil) (princ ", "))
        (format t "\"~A\": [\"" sym)
        (if (inspect-arglist sym)
            (write-json-chars (prin1-to-string (inspect-arglist sym)) *standard-output*))
        (princ "\", \"")
        (write-json-chars
         (or (inspect-docstring sym) "")
         *standard-output*)
        (princ "\"]")))
    (format t "}~%")))


(main)
