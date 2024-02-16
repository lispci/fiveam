;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

#+ccl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:define-definition-type :fiveam-test nil)
  (ccl:define-definition-type :fiveam-suite nil)
  (ccl:define-definition-type :fiveam-fixture nil))

(defmacro record-source-file (name type)
  #+allegro `(excl:record-source-file ',name :type ',type)
  #+ccl `(ccl:record-source-file ',name ',type)
  #-(or allegro ccl)
  ())
