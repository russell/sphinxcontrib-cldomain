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

(defpackage :sphinxcontrib.cldomain
  (:use #:closer-common-lisp #:getopt #:json #:closer-mop #:alexandria)
  (:export #:main))

(in-package :sphinxcontrib.cldomain)

(defun argv ()
  (or
   #+CL-LAUNCH cl-launch:*arguments*
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun print-message (message)
  (format t "~A~%" message))

(defun print-usage ()
  (format t "./cldomain.lisp --package <package name> --path <package path> ~%"))

(defun intern* (symbol)
  "Take a symbol in the form of a string e.g. \"CL-GIT:GIT-AUTHOR\" or
  \"CL-GIT::INDEX\" and return the interned symbol."
  (destructuring-bind (package name)
      (remove-if (lambda (e) (equal e ""))
       (loop :for i = 0 :then (1+ j)
             :as j = (position #\: symbol :start i)
             :when (subseq symbol i j)
               :collect (subseq symbol i j)
             :while j))
    (intern name package)))

(defun encode-when-object-member (type value)
  (when value (encode-object-member type value)))

(defun encode-class (symbol)
  "encode a class as a string including the package."
  (encode-symbol (class-name symbol)))

(defun encode-symbol (symbol)
  "encode the symbol as a string, including the package."
  (concatenate
   'string
   (package-name (symbol-package symbol))
   (if (multiple-value-bind
             (sym status)
           (find-symbol (symbol-name symbol) (package-name (symbol-package symbol)))
         (declare (ignore sym))
         (member status '(:inherited :external)))
       ":" "::")
   (symbol-name symbol)))

(defun encode-specializer (atom)
  "encode a single specializer lambda list"
  (cond ((eq (type-of atom) 'eql-specializer)
         (concatenate 'string "(EQ " (encode-symbol (eql-specializer-object atom)) ")"))
        ((classp atom)
         (encode-class atom))
        (t (encode-symbol atom))))

(defun encode-specializers (gf)
  (loop :for syms
        :in (mapcar #'swank-mop:method-specializers
                    (sb-mop:generic-function-methods gf))
        :collect (format nil "~a" (mapcar #'encode-specializer syms))))

(defun encode-methods (generic-function)
  (let ((methods (closer-mop:generic-function-methods generic-function)))
    (with-object ()
      (dolist (method methods)
        (let ((specializer (closer-mop:method-specializers method)))
          (encode-object-member
           (mapcar #'encode-specializer specializer)
           (documentation method t)))))))

(defun encode-object-documentation (sym type)
  "Encode documentation for a symbol as a JSON
object member."
  (encode-object-member
   type
   (documentation sym (cond
                        ((eq type 'generic-function) 'function)
                                        ; TODO i'm not sure this will
                                        ; be portable, shouldn't this
                                        ; be 'compiler-macro
                        ((eq type 'macro) 'function)
                        (t type)))))

(defun class-args (class)
  (loop :for slot :in (class-direct-slots (find-class class))
        :collect (car (slot-definition-initargs slot))))

(defun encode-object-class (sym)
  (with-array ()
    (dolist (slot (class-direct-slots (find-class sym)))
      (as-array-member ()
        (with-object ()
          (encode-object-member 'name (slot-definition-name slot))
          (encode-object-member 'initarg (write-to-string (car (slot-definition-initargs slot))))
          (encode-object-member 'readers (write-to-string (car (slot-definition-readers slot))))
          (encode-object-member 'writers (write-to-string (car (slot-definition-writers slot))))
          (encode-object-member 'type (write-to-string (class-name (class-of slot))))
          (encode-object-member 'documentation (documentation slot t)))))))

(defun symbols-to-json (package)
  (let ((my-package (string-upcase package)))
    (let* ((swank::*buffer-package* (find-package 'CL-USER))
           (swank::*buffer-readtable* (copy-readtable)))
      (let ((package-symbols nil)
            (*json-output* *standard-output*))
        (with-object ()
          (dolist (sym (swank:apropos-list-for-emacs "" t nil my-package) package-symbols)
            (destructuring-bind  (&key designator macro function generic-function setf variable type)
                sym
              (multiple-value-bind (symbol internal) (intern* designator)
                (when (eq (symbol-package symbol) (find-package (intern my-package)))
                  (as-object-member ((symbol-name symbol))
                    (with-object ()
                      (dolist (sym `((macro ,macro)
                                     (function ,function)
                                     (generic-function ,generic-function)
                                     (setf ,setf)
                                     (variable ,variable)
                                     (type ,type)))
                        (when (cadr sym)
                          (if (eq (cadr sym) :not-documented)
                              (encode-object-member (car sym) "")
                              (encode-object-documentation symbol (car sym)))))
                      (when (or function macro generic-function)
                        (encode-object-member 'arguments (format nil "~S" (swank::arglist symbol))))
                      (when type
                        (as-object-member ('slots) (encode-object-class symbol)))
                      (when internal
                        (encode-object-member 'internal t))
                      (when generic-function
                      ;; when generic function
                        (as-object-member ("methods")
                          (encode-methods (symbol-function symbol)))))))))))))))

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
       (let ((package-path (truename (pathname (cdr (assoc "path" args :test #'equalp)))))
             (my-package (cdr (assoc "package" args :test #'equalp))))
         (push package-path asdf:*central-registry*)
         (let ((*standard-output* *error-output*))
           #+quicklisp
           (ql:quickload my-package :prompt nil)
           #-quicklisp
           (asdf:oos 'asdf:load-op my-package))
         (symbols-to-json my-package)))
      (t
       (print-message "missing args")
       (print-usage)))))
