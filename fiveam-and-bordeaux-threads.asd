;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :fiveam-and-bordeaux-threads
  :author "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version (:read-file-form "version.sexp")
  :depends-on (:fiveam :alexandria :bordeaux-threads)
  :pathname "src/"
  :components ((:file "threads")))
