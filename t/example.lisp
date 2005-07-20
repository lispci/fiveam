;;;; -*- lisp -*-

(asdf:oos 'asdf:load-op :FiveAM)

(defpackage :it.bese.FiveAM.example
  (:use :common-lisp
	:it.bese.FiveAM))

(in-package :it.bese.FiveAM.example)

;;; First we need some functions to test.

(defun add-2 (n)
  (+ n 2))

(defun add-4 (n) 
  (+ n 4))

;;; Now we need to create a test which makes sure that add-2 and add-4
;;; work as specified.

;; we create a test named ADD-2 and supply a short description.
(test add-2
 "Test the ADD-2 function" ;; a short description
 ;; the checks
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2))))

;; we can already run add-2. This will return the list of test
;; results, it should be a list of two test-passed objects.

(run 'add-2) 

;; since we'd like to have some kind of readbale output we'll explain
;; the results

(explain *)

;; or we could do both at once:

(run! 'add-2)

;;; So now we've defined and run a single test. Since we plan on
;;; having more than one test and we'd like to run them together let's
;;; create a simple test suite.

(def-suite example-suite :description "The example test suite.")

;; we could explictly specify that every test we create is in the the
;; example-suite suite, but it's easier to just change the default
;; suite:

(in-suite example-suite)

;; now we'll create a new test for the add-4 function.

(test add-4
  (is (= 0 (add-4 -4))))

;; now let's run the test

(run! 'add-4)

;; we can get the same effect by running the suite:

(run! 'example-suite)

;; since we'd like both add-2 and add-4 to be in the same suite, let's
;; redefine add-2 to be in this suite:

(test add-2 "Test the ADD-2 function"
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2))))

;; now we can run the suite and we'll see that both add-2 and add-4
;; have been run (we know this since we no get 4 checks as opposed to
;; 2 as before.

(run! 'example-suite)

;; Just for fun let's see what happens when a test fails. Again we'll
;; redefine add-2, but add in a third, failing, check:

(test add-2 "Test the ADD-2 function"
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2)))
 (is (= 0 (add-2 0))))
