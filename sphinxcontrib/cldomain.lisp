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

(defvar *current-package* nil)

(defun argv ()
  (or
   #+ASDF (uiop/image:command-line-arguments)
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

(defun encode-symbol (symbol &optional (ftm-string "~s"))
  "encode the symbol as a string, including the package."
  (format nil ftm-string symbol))

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
           (scope-symbols-in-text
            (or (documentation method t) ""))))))))


(defun resolve-symbol (string &optional (package *current-package*))
  "try and resolve an uppercase symbol to it's home with a package."
  (let ((sym (find-symbol string package)))
    (when (eql (char string 0) #\:)
      (setf sym (read-from-string string)))
    (if sym (encode-symbol sym ":cl:symbol:`~~~S`")
        string)))

(defun scope-symbols-in-text (text &optional ignore-symbols)
  (with-input-from-string (stream text)
    (with-output-to-string (out)
      (let ((possible-symbol nil)
            (probably-example nil))
        (do ((char (read-char stream nil)
                   (read-char stream nil)))
            ((null char))
          (cond
            ;; if probably an example and peek eol, then switch back.
            ((and probably-example
                  (peek-char nil stream nil)
                  (eql (peek-char nil stream nil) #\Newline)
                  (setf probably-example nil))
             (write-char char out))
            ;; new line, then indentation, probably an example.
            ((and (eql char #\Newline)
                  (peek-char nil stream nil)
                  (eql (peek-char nil stream nil) #\Space)
                  (setf probably-example t))
             (write-char char out))
            ;; already started building a symbol
            ((and possible-symbol (upper-case-p char))
             (push char possible-symbol))
            ;; already building symbol, and hit a lower case letter.
            ((and possible-symbol (lower-case-p char))
             (write-string (coerce (reverse possible-symbol) 'string) out)
             (write-char char out)
             (setf possible-symbol nil))
            ;; building a symbol and found some white space
            ((and possible-symbol (not (upper-case-p char)))
             (write-string (resolve-symbol (coerce (reverse possible-symbol) 'string))
                           out)
             (unread-char char stream)
             (setf possible-symbol nil))
            ;; if the current char is a white space, then check if the
            ;; next is an uppercase, or a :
            ((and (find char '(#\Newline #\Space #\Tab) :test #'eql)
                  (peek-char nil stream nil)
                  (or (upper-case-p (peek-char nil stream nil))
                      (eql (peek-char nil stream nil) #\:)))
             (write-char char out)
             (push (read-char stream nil) possible-symbol))
            ;; else just write the character out
            (t
             (write-char char out))))))))

(defun encode-object-documentation (sym type)
  "Encode documentation for a symbol as a JSON
object member."
  (encode-object-member
   type
   (scope-symbols-in-text
    (or (documentation sym (cond
                           ((eq type 'generic-function) 'function)
                                        ; TODO i'm not sure this will
                                        ; be portable, shouldn't this
                                        ; be 'compiler-macro
                           ((eq type 'macro) 'function)
                           (t type)))
        ""))))

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
          (encode-object-member 'documentation (scope-symbols-in-text (or (documentation slot t) ""))))))))

(defun symbols-to-json (&optional (package *current-package*) )
  (let* ((swank::*buffer-package* (find-package 'CL-USER))
         (swank::*buffer-readtable* (copy-readtable)))
    (let ((package-symbols nil)
          (*json-output* *standard-output*))
      (with-object ()
        (dolist (sym (swank:apropos-list-for-emacs "" t nil (package-name package)) package-symbols)
          (destructuring-bind  (&key designator macro function generic-function setf variable type)
              sym
            (multiple-value-bind (symbol internal) (intern* designator)
              (when (eq (symbol-package symbol) package)
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
                        (encode-methods (symbol-function symbol))))))))))))))

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
         (let ((*current-package* (find-package (intern (string-upcase my-package)))))
           (symbols-to-json))))
      (t
       (print-message "missing args")
       (print-usage)))))
