;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :fiveam
  :author "Edward Marco Baringer <mb@bese.it>"
  :version (:read-file-form "version.sexp")
  :description "A simple regression testing framework"
  :license "BSD"
  :depends-on (:alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "check" :depends-on ("package" "utils"))
               (:file "fixture" :depends-on ("package"))
               (:file "classes" :depends-on ("package"))
               (:file "random" :depends-on ("package" "check"))
               (:file "test" :depends-on ("package" "fixture" "classes"))
               (:file "explain" :depends-on ("package" "utils" "check" "classes" "random"))
               (:file "suite" :depends-on ("package" "test" "classes"))
               (:file "run" :depends-on ("package" "check" "classes" "test" "explain" "suite"))))

(defsystem :fiveam/test
  :author "Edward Marco Baringer <mb@bese.it>"
  :description "FiveAM's own test suite"
  :license "BSD"
  :depends-on (:fiveam)
  :pathname "t/"
  :components ((:file "suite")
               (:file "tests" :depends-on ("suite"))))

(defmethod perform ((o test-op) (c (eql (find-system :fiveam))))
  (load-system :fiveam/test :force '(:fiveam/test))
  (uiop:symbol-call :5am :run! :it.bese.fiveam))

;;;;@include "src/package.lisp"

;;;;@include "t/example.lisp"
