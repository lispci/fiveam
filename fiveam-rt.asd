;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :fiveam-it
  :author "Edward Marco Baringer <mb@bese.it>"

  :depends-on (:fiveam)
  :pathname "src/"
  :components ((:file "rt")))

;;;;@include "src/package.lisp"

;;;;@include "t/example.lisp"
