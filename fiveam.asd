;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :fiveam
  :author "Edward Marco Baringer <mb@bese.it>"
  :depends-on (:alexandria)
  :pathname "src/"
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "check" :depends-on ("packages" "utils"))
               (:file "fixture" :depends-on ("packages"))
               (:file "classes" :depends-on ("packages"))
               (:file "random" :depends-on ("packages" "check"))
               (:file "test" :depends-on ("packages" "fixture" "classes"))
               (:file "explain" :depends-on ("packages" "utils" "check" "classes" "random"))
               (:file "suite" :depends-on ("packages" "test" "classes"))
               (:file "run" :depends-on ("packages" "check" "classes" "test" "explain" "suite")))
  :in-order-to ((test-op (load-op :fiveam-test)))
  :perform (test-op :after (op c)
             (funcall (intern (string '#:run!) :it.bese.fiveam)
                      :it.bese.fiveam)))

(defsystem :fiveam-test
  :author "Edward Marco Baringer <mb@bese.it>"
  :depends-on (:fiveam)
  :pathname "t/"
  :components ((:file "suite")
               (:file "tests" :depends-on ("suite"))))

;;;;@include "src/packages.lisp"

;;;;@include "t/example.lisp"
