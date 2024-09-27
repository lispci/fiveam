(in-package :common-lisp-user)

#+clisp
(if (ext:getenv "GITHUB_ACTION")
  (require :asdf "/usr/local/share/common-lisp/asdf/asdf")
  (require :asdf))

#-clisp
(require :asdf)

(declaim (optimize (speed 3) (space 3) (safety 3)))

(asdf:load-system "asdf")

(asdf:initialize-source-registry '(:source-registry (:tree :here) :inherit-configuration))

;;; try to find Quicklisp -- this is a mess because it isn't consistently installed in the
;;; same location.
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

(setf asdf::*compile-file-failure-behaviour* :error)
;;(setf asdf::*compile-file-warnings-behaviour* :warn)

(defvar *build-warning* nil)
(defvar *build-error* nil)

(catch 'build-failed
 (handler-bind ((warning #'(lambda (x)
                             ;; this is necessary because on SBCL
                             ;; there's an EXTERNAL handler for some
                             ;; uninteresting warnings.
                             (signal x)
                             (push x *build-warning*)))
                (error #'(lambda (x)
                           (setf *build-error* x)
                           (throw 'build-failed t))))
   (asdf:load-system "fiveam" :force t)))

(cond
  (*build-error*
   (uiop:die 1 "FiveAM build failed with an error: ~a.~%" *build-error*))
  (*build-warning*
   (uiop:die 2 "FiveAM build failed with warnings:~%~{~t~a~%~}" *build-warning*))
  (t
   (format t "FiveAM build successful.~%")
   (uiop:quit 0)))
