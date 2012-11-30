;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :it.bese.fiveam)

;;;; * Tests

;;;; While executing checks and collecting the results is the core job
;;;; of a testing framework it is also important to be able to
;;;; organize checks into groups, fiveam provides two mechanisms for
;;;; organizing checks: tests and test suites. A test is a named
;;;; collection of checks which can be run and a test suite is a named
;;;; collection of tests and test suites.

(defvar *test*
  (make-hash-table :test 'eql)
  "Lookup table mapping test (and test suite) names to objects.")

(defun get-test (key &key default error)
  "Finds the test named KEY. If KEY is a testable-object (a test case
or a test suite) then we just return KEY, otherwise we look for a test
named KEY in the *TEST* hash table."
  (if (testable-object-p key)
      key
      (multiple-value-bind (value foundp)
          (gethash key *test*)
        (if foundp
            value
            (if error
                (error "Unable to find test named ~S." key)
                default)))))

(defun (setf get-test) (value key)
  (setf (gethash key *test*) value))

(defun rem-test (key)
  (remhash key *test*))

(defun test-names ()
  (loop for test being the hash-keys of *test*
        collect test))

(defmacro test (name &body body)
  "Deprecated. See DEF-TEST."
  (simple-style-warning "~A is OBSOLETE! Use ~A instead." 'test 'def-test)
  (destructuring-bind (name &rest args)
      (ensure-list name)
    `(def-test ,name (,@args) ,@body)))

(defmacro def-test (name (&key (suite nil suite-p)
                               fixture
                               (compile-at :run-time)
                               depends-on
                               profile)
                    &body body)
  "Create a test named NAME.

NAME (a symbol)::
  The name of the test.

SUITE (a test name)::
  The suite to put the test under. It defaults to *SUITE* (which
  itself defaults to the default global suite).

FIXTURE::
  The name of the fixture to use for this test. See `WITH-FIXTURE` for
  details on fixtures.

COMPILE-AT (a keyword)::
  When the body of this test should be compiled. By default, or when
  `:compile-at` is `:run-time`, test bodies are only compiled before
  they are run. Set this to to `:definition-time` to force
  compilation, and errors/warnings, to be done at compile time.

DEPENDS-ON::
  A list, or a symbol, which specifies the relationship between this
  test and other tests. These conditions, `AND`, `OR` and `NOT` can be
  combined to produce complex dependencies (whethere this is something
  you should actually be doing is a question for another day).

  `(and &rest TEST-NAMES)`:::
    This test is run only if all of the tests in TEST-NAMES have
    passed, otherwise a single test-skipped result is generated.

  `(or &rest TEST-NAMES)`:::
    If any of TEST-NAMES has passed this test is run, otherwise a
    test-skipped result is generated.

  `(NOT TEST-NAME`:::
    This is test is run only if TEST-NAME failed.

  __a-symbol__:::
    Shorthand for `(AND a-symbol)`

PROFILE::
  When non-`NIL` profiling information will be collected as well."
  (check-type compile-at (member :run-time :definition-time))
  (multiple-value-bind (forms decls docstring)
      (parse-body body :documentation t :whole name)
    (let* ((description (or docstring ""))
           (body-forms (append decls forms))
           (suite-form (if suite-p
                           (if suite
                               `(get-test ',suite)
                               nil)
                           '*suite*))
           (effective-body (let* ((test-fixture fixture)
                                  (suite-fixture (if suite-p
                                                     (if suite
                                                         (fixture (get-test suite :error t))
                                                         nil)
                                                     (if *suite*
                                                         (fixture *suite*)
                                                         nil)))
                                  (effective-fixture (or test-fixture suite-fixture)))
                             (if effective-fixture
                                 (destructuring-bind (name &rest args)
                                     (ensure-list effective-fixture)
                                   `((with-fixture ,name ,args ,@body-forms)))
                                 body-forms))))
      `(progn
         (register-test :name ',name
                        :description ,description
                        :body ',effective-body
                        :suite ,suite-form
                        :depends-on ',depends-on
                        :compile-at ,compile-at
                        :profile ,profile)
         (when *run-test-when-defined*
           (run! ',name))
         ',name))))

(defun register-test (&key name description body suite depends-on compile-at profile)
  (remove-from-suites name)
  (let ((lambda-name
          (format-symbol t "%~A-~A" '#:test name))
        (inner-lambda-name
          (format-symbol t "%~A-~A" '#:inner-test name)))
    (setf (get-test name)
          (make-instance 'test-case
                         :name name
                         :runtime-package (find-package (package-name *package*))
                         :test-lambda
                         (eval
                          `(named-lambda ,lambda-name ()
                             ,@(ecase compile-at
                                 (:run-time `((funcall
                                               (let ((*package* (find-package ',(package-name *package*))))
                                                 (compile ',inner-lambda-name
                                                          '(lambda () ,@body))))))
                                 (:definition-time body))))
                         :description description
                         :depends-on depends-on
                         :collect-profiling-info profile))
    (when suite
      (setf (gethash name (tests (get-test suite :error t))) name))))

(defvar *run-test-when-defined* nil
  "When non-NIL tests are run as soon as they are defined.")

;; Copyright (c) 2002-2003, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
