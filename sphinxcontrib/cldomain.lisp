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
  (:export #:main
           #:encode-symbol
           #:resolve-symbol
           #:*current-package*
           #:intern*
           #:scope-symbols-in-text
           #:find-best-symbol))

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

(defun split-symbol-string (string)
    (remove-if (lambda (e) (equal e ""))
       (loop :for i = 0 :then (1+ j)
             :as j = (position #\: string :start i)
             :when (subseq string i j)
               :collect (subseq string i j)
             :while j)))

(defun intern* (symbol)
  "Take a symbol in the form of a string e.g. \"CL-GIT:GIT-AUTHOR\" or
  \"CL-GIT::INDEX\" and return the interned symbol."
  (let ((symbol-parts (split-symbol-string symbol)))
    (when (= (length symbol-parts) 2)
        (destructuring-bind (package name)
            symbol-parts
          (intern name package)))))

(defun find-symbol* (string &optional package)
  (let ((symbol-parts (split-symbol-string string)))
    (cond
      ((= (length symbol-parts) 1)
      (if package
           (find-symbol string (find-package package))
           (find-symbol string)))
      ((= (length symbol-parts) 2)
       (destructuring-bind (package-part name-part)
           symbol-parts
         (when (find-package package-part)
           (find-symbol name-part
                        (find-package
                         package-part))))))))

(defun simplify-arglist (arglist)
  "return the first element of each list item this is to remove the
default values from the arglist."
  (mapcar (lambda (e)
            (cond
              ((listp e)
               (car e))
              (t e)))
          arglist))

(defun encode-when-object-member (type value)
  (when value (encode-object-member type value)))

(defun encode-class (symbol)
  "Encode a class as a string including the package."
  (encode-symbol (class-name symbol)))

(defun encode-symbol (symbol &optional (ftm-string "~a"))
  "Encode the symbol as a string, including the package.  If the value
is a string then just return the string."
  (let ((sym-string
          (if (stringp symbol)
              symbol
              (concatenate
               'string
               (package-name (symbol-package symbol))
               (if (multiple-value-bind
                         (sym status)
                       (find-symbol (symbol-name symbol)
                                    (package-name
                                     (symbol-package symbol)))
                     (declare (ignore sym))
                     (member status '(:inherited :external)))
                   ":" "::")
               (symbol-name symbol)))))
    (format nil ftm-string sym-string)))

(defun encode-xref (symbol)
  (encode-symbol symbol ":cl:symbol:`~~~a`"))

(defun encode-specializer (atom)
  "encode a single specializer lambda list"
  (cond ((eq (type-of atom) 'eql-specializer)
         (concatenate 'string
                      "(EQ " (encode-symbol
                              (eql-specializer-object atom)) ")"))
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

(defun find-best-symbol (symbols &optional ignore-symbols)
  "try and find the best symbol, return the most appropriate and the
remaining string.  symbols is a list of strings that contains
possible symbol names."
  (dolist (symbol-string symbols)
    (let ((symbol (find-symbol* symbol-string)))
      (when (and (not (null symbol)) (symbolp symbol))
        (when (member symbol ignore-symbols)
          (return))
        (return-from find-best-symbol
          (values symbol (if (equal symbol-string (car symbols))
                           ""  ; first symbol, so no remainder
                           (subseq (car symbols) (length symbol-string))))))))
  (values nil (car symbols)))

(defun scope-symbols-in-text (text &optional ignore-symbols)
  (flet ((whitespace-p (char)
           (find char '(#\Newline #\Space #\Tab) :test #'eql)))
    (with-input-from-string (stream text)
      (with-output-to-string (out)
        (let ((possible-symbol nil)
              (possible-symbols nil)
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
              ((and possible-symbol
                    (or (upper-case-p char)
                        (and (not (whitespace-p char))
                             (not (lower-case-p char)))))
               (push char possible-symbol)
               ;; already building a symbol, something looks fishy one
               ;; char ahead, so save each possible symbol.
               (when (and possible-symbol
                          (peek-char nil stream nil) ; check there is something there
                          (not (upper-case-p (peek-char nil stream nil))))
                 (push (coerce (reverse possible-symbol) 'string)
                       possible-symbols)))
              ;; already building symbol, and hit a lower case letter.
              ((and possible-symbol (lower-case-p char))
               (write-string (coerce (reverse possible-symbol) 'string) out)
               (write-char char out)
               (setf possible-symbol nil)
               (setf possible-symbols nil))
              ;; building a symbol and found some white space
              ((and possible-symbol (not (upper-case-p char)))
               (push (coerce (reverse possible-symbol) 'string)
                     possible-symbols)
               (multiple-value-bind (symbol rest)
                   (find-best-symbol possible-symbols ignore-symbols)
                 (when symbol
                   (write-string (encode-xref symbol) out))
                 (write-string rest out))
               (unread-char char stream)
               (setf possible-symbol nil)
               (setf possible-symbols nil))
              ;; if the current char is a white space, then check if the
              ;; next is an uppercase, or a :
              ((and (whitespace-p char)
                    (peek-char nil stream nil)
                    (or (upper-case-p (peek-char nil stream nil))
                        (eql (peek-char nil stream nil) #\:)))
               (write-char char out)
               (push (read-char stream nil) possible-symbol))
              ;; else just write the character out
              (t
               (write-char char out))))
          (when possible-symbol
            (write-string (coerce (reverse possible-symbol) 'string) out)))))))

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
        "")
    (when (functionp sym)
      (simplify-arglist
       (swank::arglist sym))))))

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
