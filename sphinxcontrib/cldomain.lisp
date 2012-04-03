#!/usr/bin/sbcl --script

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


(defun main ()
  (multiple-value-bind (unused-args args invalid-args)
      (getopt:getopt (argv) '(("package" :required)("path" :required)))
    (cond
      (invalid-args
       (print "Invalid arguments"))
      ((< 1 (length unused-args))
       (print "Unused args"))
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
       (princ "missing args")))))


(defun inspect-docstring (sym)
  (documentation sym (cond ((boundp sym)
			    'variable)
			   ((fboundp sym)
			    'function)
			   ((compiler-macro-function sym)
			    'macro))))


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
        (format t "\"~A\": \"" sym)
        (write-json-chars
         (or (inspect-docstring sym) "")
         *standard-output*)
        (princ "\"")))
    (format t "}~%")))


(main)
