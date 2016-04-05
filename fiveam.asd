;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :fiveam
  :author "Edward Marco Baringer <mb@bese.it>"
  :version (:read-file-form "version.sexp")
  :description "A simple regression testing framework"
  :license "BSD"
  :depends-on (:alexandria :net.didierverna.asdf-flv)
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
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :fiveam))))
  (load-system :fiveam/test :force '(:fiveam/test))
  (symbol-call :5am :run! :it.bese.fiveam))

;;;;@include "src/package.lisp"

;;;;@include "t/example.lisp"
