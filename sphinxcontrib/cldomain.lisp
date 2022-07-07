;; cldomain is a Common Lisp domain for the Sphinx documentation tool.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>

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

(in-package :sphinxcontrib.cldomain)

(defvar *current-package* nil)

(defun print-message (message)
  (format t "~A~%" message))

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

(defun find-symbol* (string &optional (package *current-package*))
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

(defun encode-literal (symbol)
  (if (equal (symbol-package symbol) (find-package 'keyword))
      (format nil "``~s``" symbol)
      (format nil "``~a``" symbol)))

(defun encode-specializer (atom)
  "encode a single specializer lambda list"
  (cond ((eq (type-of atom) 'eql-specializer)
         (concatenate 'string
                      "(EQ " (encode-symbol
                              (eql-specializer-object atom)) ")"))
        ((classp atom)
         (encode-class atom))
        (t (encode-symbol atom))))

(defun encode-methods (generic-function)
  (let ((methods (closer-mop:generic-function-methods generic-function)))
    (with-object ()
      (dolist (method methods)
        (let ((specializer (closer-mop:method-specializers method))
              (lambda-list (closer-mop:method-lambda-list method)))
          (encode-object-member
           (mapcar #'encode-specializer specializer)
           (scope-symbols-in-text
            (or (documentation method t) "")
            (simplify-arglist lambda-list))))))))

(defun intern-keyword (keyword)
  (let ((keyword (if (eql (char keyword 0) #\:)
                     (subseq keyword 1) keyword)))
    (intern (string-upcase keyword) 'keyword)))

(defun find-best-symbol (symbols &optional ignore-symbols)
  "try and find the best symbol, return the most appropriate and the
remaining string.  symbols is a list of strings that contains
possible symbol names."
  (dolist (symbol-string symbols)
    (let ((symbol (find-symbol* symbol-string))
          (rest-string
            (if (equal symbol-string (car symbols))
                ""  ; first symbol, so no remainder
                (subseq (car symbols) (length symbol-string)))))
      (cond
        ;; Exception for keywords with full stops at the end.  It's
        ;; probably punctuation.
        ((and (eql (char symbol-string 0) #\:)
              (member (char symbol-string (1- (length symbol-string)))
                      (list #\. #\,)))
         (return-from find-best-symbol
           (values nil
                   (intern-keyword (subseq symbol-string
                                         1
                                         (1- (length symbol-string))))
                   (subseq symbol-string (1- (length symbol-string))))))
        ;; Return keyword
        ((eql (char symbol-string 0) #\:)
         (return-from find-best-symbol
           (values nil
                   (intern-keyword (subseq symbol-string 1))
                   rest-string)))
        ((and (not (null symbol)) (symbolp symbol))
         (if (member symbol ignore-symbols)
             (return-from find-best-symbol
               (values nil symbol rest-string))
             (return-from find-best-symbol
               (values symbol nil rest-string)))))))
  (values nil nil (car symbols)))

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
                    (eql char #\Newline)
                    (peek-char nil stream nil)
                    (not (eql (peek-char nil stream nil) #\Space)))
               (write-char char out)
               (setf probably-example nil))
              ;; continue writing an example if we are in one. (avoid
              ;; encoding symbols)
              (probably-example
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
               (multiple-value-bind (symbol literal rest)
                   (find-best-symbol possible-symbols ignore-symbols)
                 (when symbol
                   (write-string (encode-xref symbol) out))
                 (when literal
                   (write-string (encode-literal literal) out))
                 (write-string rest out))
               ;; TODO (RS) Pushing newlines back onto the input stream -- this can cause
               ;; some CL's to spew.
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
          ;; Write out remaining symbols
          (when possible-symbol
            (push (coerce (reverse possible-symbol) 'string)
                  possible-symbols)
            (multiple-value-bind (symbol literal rest)
                   (find-best-symbol possible-symbols ignore-symbols)
                 (when symbol
                   (write-string (encode-xref symbol) out))
                 (when literal
                   (write-string (encode-literal literal) out))
                 (write-string rest out))))))))

(defun arglist (symbol)
  #+sbcl  (sb-introspect:function-lambda-list symbol)
  #+clisp (sys::arglist symbol)
  #+ccl (ccl:arglist symbol)
  #+ecl   (ext:function-lambda-list symbol)
  #-(or sbcl clisp ccl ecl) (error "arglist not available for this Lisp."))

(defun encode-symbol-status (symbol package)
  (multiple-value-bind
        (sym internal)
      (find-symbol (symbol-name symbol) package)
    (declare (ignore sym))
    (case internal
      (:internal
       (encode-object-member 'external nil))
      (:external
       (encode-object-member 'external t))
      (:inherited
       (encode-object-member 'external nil)))))

(defun encode-function-documentation* (symbol type doc)
  (encode-object-member
   type
   (scope-symbols-in-text
    doc
    (simplify-arglist (arglist symbol))))
  (encode-object-member
   'arguments
   (format nil "~S" (arglist symbol))))

(defun encode-variable-documentation* (symbol type)
  (encode-object-member
   type
   (scope-symbols-in-text (or (documentation symbol type) ""))))

(defgeneric encode-object-documentation (symbol type)
  (:documentation
   "Encode documentation for a symbol as a JSON object member."))

(defmethod encode-function-documentation (symbol (type (eql 'function)))
  (encode-function-documentation*
   symbol type (or (documentation symbol type) "")))

;; CLISP-ism (might be other CL's as well): compiled functions are still
;; functions:
(defmethod encode-function-documentation (symbol (type (eql 'compiled-function)))
  (encode-function-documentation*
   symbol 'function (or (documentation symbol 'function) "")))

(defmethod encode-function-documentation (symbol (type (eql 'macro)))
  (encode-function-documentation*
   symbol type (or (documentation symbol 'function) "")))

(defmethod encode-function-documentation (symbol (type (eql 'generic-function)))
  (encode-function-documentation*
   symbol type (or (documentation symbol 'function) ""))
  (as-object-member ('methods)
    (encode-methods (symbol-function symbol))))

(defmethod encode-function-documentation (symbol (type (eql 'cl:standard-generic-function)))
  (encode-function-documentation symbol 'generic-function))

(defmethod encode-value-documentation (symbol (type (eql 'variable)))
  (encode-variable-documentation* symbol type))

(defmethod encode-value-documentation (symbol (type (eql 'setf)))
  (encode-function-documentation* symbol type (or (documentation symbol 'setf) "")))

(defmethod encode-value-documentation (symbol (type (eql 'type)))
  (encode-variable-documentation* symbol type)
  (as-object-member ('slots)
    (encode-object-class symbol)))

(defun symbol-function-type (symbol)
  (cond
    ((macro-function symbol)
     'macro)
    ((fboundp symbol)
     (type-of (fdefinition symbol)))))

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

(defun class-p (symbol)
  "Return T if the symbol is a class."
  #+sbcl (eql (sb-int:info :type :kind symbol) :instance)
  #-sbcl (find-class symbol nil))

(defun variable-p (symbol)
  "Return T if the symbol is a bound variable."
  (and #+sbcl (sb-int:info :variable :kind symbol)
              (boundp symbol)))

(defun symbols-to-json (&optional (package *current-package*))
  (do-external-symbols (symbol package)
    (as-object-member ((encode-symbol symbol))
      (with-object ()
        (encode-symbol-status symbol package)
        (when (symbol-function-type symbol)
          ;; (format *error-output* "symbol ~S type ~S~%"symbol (symbol-function-type symbol))
          (encode-function-documentation symbol (symbol-function-type symbol)))
        (when (class-p symbol)
          (encode-value-documentation symbol 'type))
        (when (variable-p symbol)
          (encode-value-documentation symbol 'variable))))))

(defun print-packages (&rest packages)
  (let ((*json-output* *standard-output*))
    (cl-json:with-object ()
      (dolist (package packages)
        (let ((*current-package*
                (find-package (intern (string-upcase package)))))
          (symbols-to-json))))))
