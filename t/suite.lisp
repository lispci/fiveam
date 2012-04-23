;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :it.bese.fiveam)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :it.bese)
    (def-suite :it.bese)))

(def-suite :it.bese.fiveam :in :it.bese)
