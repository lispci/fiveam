;; -*- lisp -*-

(in-package :it.bese.FiveAM)

;;;; * Tests

;;;; While executing checks and collecting the results is the core job
;;;; of a testing framework it is also important to be able to
;;;; organize checks into groups, FiveAM provides two mechanisms for
;;;; organizing checks: tests and test suites. A test is a named
;;;; collection of checks which can be run and a test suite is a named
;;;; collection of tests and test suites.

(deflookup-table test
  :at-redefinition nil
  :documentation "Lookup table mapping test (and test suite)
  names to objects.")

(defun test-names ()
  (loop for test being the hash-keys of *test*
        collect test))

(defmacro test (name &body body)
  "Create a test named NAME. If NAME is a list it must be of the
form:

  (name &key depends-on suite fixture compile-at profile)

NAME is the symbol which names the test.

DEPENDS-ON is a list of the form:

 (AND . test-names) - This test is run only if all of the tests
 in TEST-NAMES have passed, otherwise a single test-skipped
 result is generated.

 (OR . test-names) - If any of TEST-NAMES has passed this test is
 run, otherwise a test-skipped result is generated.

 (NOT test-name) - This is test is run only if TEST-NAME failed.

AND, OR and NOT can be combined to produce complex dependencies.

If DEPENDS-ON is a symbol it is interpreted as `(AND
,depends-on), this is accomadate the common case of one test
depending on another.

FIXTURE specifies a fixtrue to wrap the body in.

If PROFILE is T profiling information will be collected as well."
  (let* ((tmp (gensym))
         (suite-arg (getf (cdr (ensure-list name)) :suite tmp))
         (suite-form (cond
                       ((eq tmp suite-arg) '*suite*)
                       (t                  `(get-test ',suite-arg)))))
    (when (consp name)
      (remf (cdr name) :suite))
    (destructuring-bind (name &key depends-on (compile-at :run-time) fixture profile)
        (ensure-list name)
      (declare (type (member :run-time :definition-time) compile-at))
      (let ((description (if (stringp (car body))
                             (pop body)
                             ""))
            (effective-body (if fixture
                                (destructuring-bind (name &rest args)
                                    (ensure-list fixture)
                                  `((with-fixture ,name ,args ,@body)))
                                body)))
        `(progn
           (setf (get-test ',name) (make-instance 'test-case
                                                  :name ',name
                                                  :runtime-package
                                                  #-ecl ,*package*
                                                  #+ecl (find-package ,(package-name *package*))
                                                  :test-lambda
                                                  (lambda ()
                                                    ,@ (ecase compile-at
                                                         (:run-time `((funcall
                                                                       (let ((*package* (find-package ',(package-name *package*))))
                                                                         (compile nil '(lambda ()
                                                                                        ,@effective-body))))))
                                                         (:definition-time effective-body)))
                                                  :description ,description
                                                  :depends-on ',depends-on
                                                  :collect-profiling-info ,profile))
           (setf (gethash ',name (tests ,suite-form)) ',name)
           (when *run-test-when-defined*
             (run! ',name))
           ',name)))))

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
