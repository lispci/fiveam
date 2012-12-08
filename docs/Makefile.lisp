(in-package :smake-user)

(defvar *asciidoc-root* #P"/usr/local/etc/asciidoc/")

(program "asciidoc")

(defun static-file (name &optional source destination)

  (cond
    ((null source)
     (setf source (source-pathname name)))
    ((stringp source)
     (setf source (source-pathname source))))

  (cond
    ((null destination)
     (setf destination (build-pathname name)))
    ((stringp destination)
     (setf destination (build-pathname destination))))

  (target* `(static-file ,name) ()
    (when (file-newer-p source destination)
      (path:cp source destination :overwrite t))))

(static-file "asciidoc.css" (path:catfile *asciidoc-root* "stylesheets/" "asciidoc.css"))

(static-file "asciidoc.js" (path:catfile *asciidoc-root* "javascripts/" "asciidoc.js"))

(static-file "fiveam.css")

(target (static-directory "asciidoc/images") ()
  (ensure-directories-exist (build-pathname "images/icons/callouts/"))
  (dolist (src (directory (path:catfile *asciidoc-root* "images/" "icons/" "callouts/" "*.png")))
    (let ((dst (build-pathname (path:catfile "images/icons/callouts/" (path:basename src)))))
      (when (file-newer-p src dst)
        (path:cp src dst)))))

(defun asciidoc.html (source &optional requires)
  (target* `(asciidoc ,source) (:requires (append requires
                                                  '((program "asciidoc")
                                                    (static-file "asciidoc.js")
                                                    (static-file "asciidoc.css")
                                                    (static-file "fiveam.css")
                                                    (static-directory "asciidoc/images"))))
    (when (file-newer-p (source-pathname source) (build-pathname source :type "html"))
      (unless (path:-e (build-pathname source))
        (sys `(ln -s ,(source-pathname source) ,(build-pathname source))))
      (sys `(asciidoc -o ,(build-pathname source :type "html") ,(build-pathname source))))))

(target "docstrings" ()
  (unless (path:-d (build-pathname "docstrings/"))
    (sys `(ccl64 --load ../extract-docstrings.lisp))
    (sys `(rm -f ,(build-pathname "manual.html") ,(build-pathname "tutorial.html")))))

(asciidoc.html "manual.txt" '("docstrings"))
(asciidoc.html "tutorial.txt" '((asciidoc "manual.txt")))

(target "documentation" (:requires '((asciidoc "manual.txt")
                                     (asciidoc "tutorial.txt"))))

(target "all" (:requires '("documentation")))

