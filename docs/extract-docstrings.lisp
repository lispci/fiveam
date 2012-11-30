(quicklisp:quickload :iterate)
(quicklisp:quickload :alexandria)

(defpackage :it.bese.fiveam.documentation
  (:use :common-lisp :iterate :alexandria))

(in-package :it.bese.fiveam.documentation)

(quicklisp:quickload :cl-fad)
(quicklisp:quickload :cl-ppcre)
(quicklisp:quickload :closer-mop)

(quicklisp:quickload :fiveam)

(defvar *slime-root* #P"/Users/mb/m/.emacs/slime/")

(load (path:catfile *slime-root* "swank.asd"))
(asdf:load-system :swank)

(ensure-directories-exist "./docstrings/")

(defun symbol-name-to-pathname (symbol type)
  (let ((name (if (symbolp symbol)
                  (symbol-name symbol)
                  (string symbol))))
    (setf name (cl-ppcre:regex-replace-all "\\*" name "-STAR-")
          name (cl-ppcre:regex-replace-all "\\+" name "-PLUS-")
          name (cl-ppcre:regex-replace-all "\\~" name "-TILDE-")
          name (cl-ppcre:regex-replace-all "\\!" name "-EPOINT-")
          name (cl-ppcre:regex-replace-all "\\!" name "-QMARK-"))
    (concatenate 'string
                 (ecase type (function "OP") (type "TYPE") (arglist "ARGLIST") (variable "VAR"))
                 "_"
                 name)))

(defun output-docstring (name type)
  (let ((docstring (documentation name type)))
    (when docstring
      (with-output-to-file (d (path:catfile "./docstrings/" (format nil "~A.txt" (symbol-name-to-pathname name type))) :if-exists :supersede)
        (write-string docstring d)))))

(iter
 (with *package* = (find-package :fiveam))
 (for i in-package (find-package :fiveam) external-only t)

 (output-docstring i 'function)
 (when (documentation i 'function)
   (with-output-to-file (d (path:catfile "./docstrings/" (format nil "~A.txt" (symbol-name-to-pathname i 'arglist))))
     (write-string (string-downcase (format nil "~A~{ __~A__~}~%~%" i (swank-backend:arglist i)))
                   d)))
  (output-docstring i 'variable))

(output-docstring '5am::test-suite 'type)
(output-docstring '5am::testable-object 'type)
(output-docstring '5am::test-case 'type)
