;;;; -*- lisp -*-

(in-package :it.bese.fiveam)

(unless (get-test :it.bese)
  (def-suite :it.bese))

(def-suite :it.bese.fiveam :in :it.bese)
