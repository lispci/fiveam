;;;; -*- lisp -*-

(in-package :it.bese.FiveAM)

(in-suite :it.bese.FiveAM)

(def-suite test-suite :description "Suite for tests which should fail.")

(defmacro with-test-results ((results test-name) &body body)
  `(let ((,results (with-*test-dribble* nil (run ',test-name))))
     ,@body))

;;;; Test the checks

(test (is1 :suite test-suite)
  (is (plusp 1))
  (is (< 0 1))
  (is (not (plusp -1)))
  (is (not (< 1 0)))
  (is-true t)
  (is-false nil))

(test (is2 :suite test-suite)
  (is (plusp 0))
  (is (< 0 -1))
  (is (not (plusp 1)))
  (is (not (< 0 1)))
  (is-true nil)
  (is-false t))

(test is
  (with-test-results (results is1)
    (is (= 6 (length results)))
    (is (every #'test-passed-p results)))
  (with-test-results (results is2)
    (is (= 6 (length results)))
    (is (every #'test-failure-p results))))

(test signals/finishes
  (signals error
    (error "an error"))
  (finishes
   (signals error
    (error "an error"))))

(test pass
  (pass))

(test (fail1 :suite test-suite)
  (fail "This is supposed to fail"))

(test fail
  (with-test-results (results fail1)
    (is (= 1 (length results)))
    (is (test-failure-p (first results)))))

;;;; non top level checks

(test foo-bar
  (let ((state 0))
    (is (= 0 state))
    (is (= 1 (incf state)))))

;;;; Test dependencies

(test (ok :suite test-suite)
  (pass))

(test (not-ok :suite test-suite)
  (fail "This is supposed to fail."))

(test (and1 :depends-on (and ok not-ok) :suite test-suite)
  (fail))

(test (and2 :depends-on (and ok) :suite test-suite)
  (pass))

(test dep-and 
  (with-test-results (results and1)
    (is (= 3 (length results)))
    ;; we should have one skippedw one failed and one passed
    (is (some #'test-passed-p results))
    (is (some #'test-skipped-p results))
    (is (some #'test-failure-p results)))
  (with-test-results (results and2)
    (is (= 2 (length results)))
    (is (every #'test-passed-p results))))

(test (or1 :depends-on (or ok not-ok) :suite test-suite)
  (pass))

(test (or2 :depends-on (or not-ok ok) :suite test-suite)
  (pass))

(test dep-or
  (with-test-results (results or1)
    (is (= 2 (length results)))
    (is (every #'test-passed-p results)))
  (with-test-results (results or2)
    (is (= 3 (length results)))
    (is (= 2 (length (remove-if-not #'test-passed-p results))))))

(test (not1 :depends-on (not not-ok) :suite test-suite)
  (pass))

(test (not2 :depends-on (not ok) :suite test-suite)
  (fail))

(test not
  (with-test-results (results not1)
    (is (= 2 (length results)))
    (is (some #'test-passed-p results))
    (is (some #'test-failure-p results)))
  (with-test-results (results not2)
    (is (= 2 (length results)))
    (is (some #'test-passed-p results))
    (is (some #'test-skipped-p results))))

(test (nested-logic :depends-on (and ok (not not-ok) (not not-ok))
                    :suite test-suite)
  (pass))

(test dep-nested
  (with-test-results (results nested-logic)
    (is (= 3 (length results)))
    (is (= 2 (length (remove-if-not #'test-passed-p results))))
    (is (= 1 (length (remove-if-not #'test-failure-p results))))))

(test (circular-0 :depends-on (and circular-1 circular-2 or1) 
                  :suite test-suite)
  (fail "we depend on a circular dependency, we should not be tested."))

(test (circular-1 :depends-on (and circular-2)
                  :suite test-suite)
  (fail "we have a circular depednency, we should not be tested."))

(test (circular-2 :depends-on (and circular-1)
                  :suite test-suite)
  (fail "we have a circular depednency, we should not be tested."))

(test circular
  (signals circular-dependency
    (run 'circular-0))
  (signals circular-dependency
    (run 'circular-1))
  (signals circular-dependency
    (run 'circular-2)))
