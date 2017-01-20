;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :it.bese.fiveam)

;;; We want to make something like the following possible:

;; (test foo
;;   (let ((thread (bt:make-thread (lambda () (is (= 7 6))))))
;;     (is (= 1 5))
;;     (bt:join-thread thread)))

;;; Result management

(defvar *result-lock*)

(defvar *main-thread*)

(defmethod call-environment-binding :around
    ((name (eql 'run-state)) (bindings list) (style t) (thunk t))
  (let* ((*result-lock* (bt:make-lock "Result lock"))
         (*main-thread* nil)
         (bt:*default-wrappers*
          (list*
           (lambda (next)
             ;; Capture in parent thread.
             (let ((main-thread      (bt:current-thread)) ; TODO make a list of special variables
                   (debug-on-failure *debug-on-failure*)
                   (debug-on-error   *debug-on-error*)
                   (result-lock      *result-lock*))
               (lambda ()
                 ;; Establish in child thread.
                 (let ((*main-thread*      main-thread)
                       (*debug-on-failure* debug-on-failure)
                       (*debug-on-error*   debug-on-error)
                       (*result-lock*      result-lock))
                   (handler-bind
                       ((check-failure
                         (lambda (e)
                           (declare (ignore e))
                           (unless *debug-on-failure*
                             (invoke-restart
                              (find-restart 'ignore-failure)))))
                        (error
                         (lambda (e)
                           (unless (or *debug-on-error*
                                       (typep e 'check-failure))
                             ;; TODO make a function; also in run-test-lambda
                             (locally (declare (special current-test))
                               (add-result 'unexpected-test-failure
                                           :test-expr nil
                                           :test-case current-test
                                           :reason (format nil "Unexpected Error: ~S~%~A." e e)
                                           :condition e))
                             (abort)))))
                     (funcall next))))))
           bt:*default-wrappers*)))
    (call-next-method)))

(defmethod add-result :around ((result-type t) &rest make-instance-args)
  (bt:with-lock-held (*result-lock*)
    (if *main-thread*
        (bt:interrupt-thread
         *main-thread*
         (lambda () (apply #'add-result result-type make-instance-args)))
        (call-next-method))))
