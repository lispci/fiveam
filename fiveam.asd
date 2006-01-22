;; -*- lisp -*-

(defpackage :it.bese.FiveAM.system
  (:use :common-lisp
        :asdf))

(in-package :it.bese.FiveAM.system)

(defsystem :FiveAM
    :author "Edward Marco Baringer <mb@bese.it>"
    :properties ((:test-suite-name . :it.bese.fiveam))
    :components ((:static-file "fiveam.asd")
                 (:module :src
                  :components ((:file "check" :depends-on ("packages"))
			       (:file "classes" :depends-on ("packages"))
			       (:file "explain" :depends-on ("classes" "packages" "check"))
			       (:file "fixture" :depends-on ("packages"))
			       (:file "packages")
			       (:file "run" :depends-on ("packages" "classes" "test" "suite" "check"))
			       (:file "suite" :depends-on ("packages" "test" "classes"))
                               (:file "random" :depends-on ("packages" "check"))
			       (:file "test" :depends-on ("packages" "classes"))))
		 (:module :t
		  :components ((:file "suite")
			       (:file "tests" :depends-on ("suite")))
		  :depends-on (:src)))
    :depends-on (:arnesi))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :FiveAM))))
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :it.bese.FiveAM))

;;;;@include "src/packages.lisp"

;;;;@include "t/example.lisp"
