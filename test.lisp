(defpackage testing
    (:use common-lisp))

(in-package :testing)

#+clisp
(if (ext:getenv "GITHUB_ACTION")
  (require :asdf "/usr/local/share/common-lisp/asdf/asdf")
  (require :asdf))

#-clisp
(require :asdf)

(declaim (optimize (speed 3) (space 3) (safety 3)))

(asdf:load-system "asdf")
(asdf:initialize-source-registry '(:source-registry (:tree :here) :inherit-configuration))

;;; try to find quicklisp
(if (uiop:find-package* '#:ql nil)
    (format t "~&Quicklisp pre-loaded into image.~%")
    (let ((ql-filename (uiop:getenv "QUICKLISP_SETUP"))
          loaded)
      (if ql-filename
          (if (probe-file ql-filename)
              (let ((result (load ql-filename :if-does-not-exist nil)))
                (when result
                  (format t "~&Have loaded quicklisp setup file ~a.~%" ql-filename)
                  (setf loaded t)))
              (format t "Quicklisp not installed where expected: ~a~%" ql-filename)))
      (unless loaded
        (let* ((fallback-name "/root/quicklisp/setup.lisp")
               (result (load fallback-name :if-does-not-exist nil)))
          (when result
            (format t "~&Have loaded quicklisp setup file from /root.~%")
            (setf loaded t))))
      (unless loaded
        (format t "~&Unable to find quicklisp.~%")
        (uiop:quit 1 t))))

(ql:quickload "alexandria")
(ql:quickload "trivial-backtrace")
(ql:quickload "net.didierverna.asdf-flv")

(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (uiop:quit return))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (flet ((quit (c desc)
           (format *error-output* "~&Encountered ~a during test.~%~a~%" desc c)
           (cond
            ;; decline to handle the error.
            ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
             (format t "~&Interactive mode (DEBUG_ASDF_TEST) -- Invoke debugger.~%")
             (invoke-debugger c))
            (t
             (finish-output *standard-output*)
             (finish-output *trace-output*)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (uiop:print-condition-backtrace c)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (finish-output *error-output*)
             (leave-lisp "~&Script failed~%" 1)))))
    (handler-bind
        ((error (lambda (c)
                  (quit c  "ERROR")))
         (storage-condition
          (lambda (c) (quit c "STORAGE-CONDITION")))
         (serious-condition (lambda (c)
                              (quit c "Other SERIOUS-CONDIITON"))))
      (funcall thunk)
      (format t "~&Script succeeded~%")
      t)))

(quit-on-error
 (format t "~&;;; Testing standard FiveAM.~%")
 (asdf:test-system "fiveam"))

(uiop:quit 0)
