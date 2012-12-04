;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :it.bese.FiveAM)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :it.bese)
    (def-suite :it.bese)))

(def-suite :it.bese.FiveAM :in :it.bese)
