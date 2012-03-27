(asdf:oos 'asdf:load-op 'cl-git)

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
    (do-external-symbols (sym package)
      (princ "\"")
      (princ sym)
      (princ "\": \"")
      (write-json-chars
       (if (inspect-docstring sym)
	   (inspect-docstring sym)
	   "") *standard-output*)
      (princ "\", "))
    (princ "}")))


