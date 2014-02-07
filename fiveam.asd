;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :fiveam
  :author "Edward Marco Baringer <mb@bese.it>"
  :version #.(with-open-file (f (merge-pathnames "version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :description "A simple regression testing framework"
  :license "BSD"
  :depends-on (:alexandria :trivial-backtrace)
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
               (:file "run" :depends-on ("package" "check" "classes" "test" "explain" "suite")))
  :in-order-to ((test-op (load-op :fiveam-test)))
  :perform (test-op :after (op c)
             (funcall (intern (string '#:run!) :it.bese.fiveam)
                      :it.bese.fiveam)))

(defsystem :fiveam-test
  :author "Edward Marco Baringer <mb@bese.it>"
  :description "FiveAM's own test suite"
  :license "BSD"
  :depends-on (:fiveam)
  :pathname "t/"
  :components ((:file "suite")
               (:file "tests" :depends-on ("suite"))))

;;;;@include "src/package.lisp"

;;;;@include "t/example.lisp"
