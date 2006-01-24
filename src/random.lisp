;; -*- lisp -*-

(in-package :it.bese.FiveAM)

;;;; ** Random (QuickCheck-ish) testing

;;;; FiveAM provides the ability to automatically generate a
;;;; collection of random input data for a specific test and run a
;;;; test multiple times.

;;;; Specification testing is done through the FOR-ALL macro. This
;;;; macro will bind variables to random data and run a test body a
;;;; certain number of times. Should the test body ever signal a
;;;; failure we stop running and report what values of the variables
;;;; caused the code to fail.

(defparameter *num-trials* 100
  "Number of times we attempt to run the body of the FOR-ALL test.")

(defparameter *max-trials* 10000
  "Number of total times we attempt to run the body of the
  FOR-ALL test including when the body is skipped due to failed
  guard conditions.

Since we have guard conditions we may get into infinite loops
where the test code is never run due to the guards never
returning true. This second run limit prevents that.")

(defmacro for-all (bindings &body body)
  `(perform-random-testing
    (list ,@(mapcar #'second bindings))
    (lambda ,(mapcar #'first bindings)
      (if (and ,@(delete-if #'null (mapcar #'third bindings)))
          (progn ,@body)
          (throw 'run-once
            (list :guard-conditions-failed))))))

(defun perform-random-testing (generators body)
  (loop
     with random-state = *random-state*
     with total-counter = *max-trials*
     with counter = *num-trials*
     with run-at-least-once = nil
     until (or (zerop total-counter)
               (zerop counter))
     do (let ((result (perform-random-testing/run-once generators body)))
          (ecase (first result)
            (:pass
             (decf counter)
             (decf total-counter)
             (setf run-at-least-once t))
            (:no-tests
             (add-result 'for-all-test-no-tests
                         :reason "No tests"
                         :random-state random-state)
             (return-from perform-random-testing nil))
            (:guard-conditions-failed
             (decf total-counter))
            (:fail
             (add-result 'for-all-test-failed
                         :reason "Found failing test data"
                         :random-state random-state
                         :failure-values (second result)
                         :result-list (third result))
             (return-from perform-random-testing nil))))
     finally (if run-at-least-once
                 (add-result 'for-all-test-passed)
                 (add-result 'for-all-test-never-run
                             :reason "Guard conditions never passed"))))

(defun perform-random-testing/run-once (generators body)
  (catch 'run-once
    (bind-run-state ((result-list '()))
      (let ((values (mapcar #'funcall generators)))
        (apply body values)
        (cond
          ((null result-list)
           (throw 'run-once (list :no-tests)))
          ((every #'test-passed-p result-list)
           (throw 'run-once (list :pass)))
          ((notevery #'test-passed-p result-list)
           (throw 'run-once (list :fail values result-list))))))))

(defclass for-all-test-result ()
  ((random-state :initarg :random-state)))

(defclass for-all-test-passed (test-passed for-all-test-result)
  ())

(defclass for-all-test-failed (test-failure for-all-test-result)
  ((failure-values :initarg :failure-values)
   (result-list :initarg :result-list)))

(defgeneric for-all-test-failed-p (object)
  (:method ((object for-all-test-failed)) t)
  (:method ((object t)) nil))

(defmethod reason ((result for-all-test-failed))
  (format nil "Falsafiable with ~S" (slot-value result 'failure-values)))

(defclass for-all-test-no-tests (test-failure for-all-test-result)
  ())

(defclass for-all-test-never-run (test-failure for-all-test-result)
  ())

;;;; *** Generators

;;;; Since this is random testing we need some way of creating random
;;;; data to feed to our code. Generators are regular functions which
;;;; create this random data.

;;;; We provide a set of built-in generators.

(defmacro defgenerator (name arguments &body body)
  `(defun ,name ,arguments
     (lambda () ,@body)))

(defgenerator gen-integer (&key (max (1+ most-positive-fixnum))
                                (min (1- most-negative-fixnum)))
  (+ min (random (1+ (- max min)))))

(defgenerator gen-character (&key (code (gen-integer :min 0 :max (1- char-code-limit)))
                                  (alphanumericp nil))
  (if alphanumericp
      (code-char (funcall code))
      (loop
         for char = (code-char (funcall code))
         until (alphanumericp char)
         finally (return char))))

(defgenerator gen-string (&key (length (gen-integer :min 0 :max 80))
                               (elements (gen-character))
                               (element-type 'character))
  (loop
     with length = (funcall length)
     with string = (make-string length :element-type element-type)
     for index below length
     do (setf (aref string index) (funcall elements))
     finally (return string)))

(defgenerator gen-list (&key (length (gen-integer :min 0 :max 10))
                             (elements (gen-integer :min -10 :max 10)))
  (loop
     repeat (funcall length)
     collect (funcall elements)))

;;;; The trivial always-produce-the-same-thing generator is done using
;;;; cl:constantly.
