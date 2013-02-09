(defpackage :fiveam.rt
  (:use :fiveam :common-lisp)
  (:export #:deftest
           #:*test*
           #:do-test
           #:*do-tests-when-defined*
           #:get-test
           #:rem-test
           #:rem-all-tests
           #:do-tests
           #:pending-tests
           #:continue-testing))

(in-package :fiveam.rt)

(fiveam:def-suite :rt
  :description "Suite holding all tests defined via the RT compatability layer.")

(defvar *test* nil)

(defvar *tests* '())

(defvar *do-tests-when-defined* nil)

(defmacro deftest (name form &rest values)
  `(progn
     (push (list ',name ',form (list ,@values)) *tests*)
     (def-test ,name ()
       (is-true (every #'equal (multiple-value-list ,name) (list ,@values))))
     (when *do-tests-when-defined*
       (do-test ',name))
     (setf *test* ',name)))

(defun do-test (&optional (name *test*))
  (setf *test* name)
  (let ((results (run name)))
    (if (every #'fiveam::test-passed-p result)
        name
        (progn
          (explain (make-instance 'fiveam::detailed-text-explainer) results)
          nil))))

(defun get-test (&optional (name *test*))
  (find name *tests* :test #'eql :key #'first))

(defun rem-test (&optional (name *test*))
  (if (get-test name)
      (progn
        (setf *tests* (delete name *tests* :test #'eql :key #'first))
        (remhash name (fiveam::tests (get-test :rt)))
        name)
      nil))

(defun rem-all-tests ()
  (clrhash (fiveam::tests (get-test :rt)))
  (setf *tests* '()))

(defun do-tests (&optional (out *standard-output*))
  (loop
    for test in *tests*
    nconc (run (get-test (first test))) into results
    finally (explain (make-instance 'fiveam::detailed-text-explainer) results)
    finally (return (every #'fiveam::test-passed-p results))))
