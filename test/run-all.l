;;; -*- mode: lisp; package: user -*-
;;;
;;; test/run-all.l
;;;
#| Usage
(load "path/to/run-all.l")
|#

(in-package :user)

(require "lisp-unit")

(when (buffer-stream-p *standard-output*)
  (dolist (filename (directory (directory-namestring *load-pathname*)
                               :wild "*.lt" :recursive t :absolute t))
    (let* ((module-name  (let ((name (pathname-name filename)))
                           (if (string-match "^[0-9]+_" name)
                             (subseq name (match-end 0))
                             name)))
           (package-name (format nil "test.ansify.~A" module-name)))
      (format t "~&~%* Running tests for ~A~%" module-name)
      (message "loading test file: ~A..." filename)
      (load filename)
      (message "loading test file: ~A...done." filename)
      (let ((pkg (find-package package-name)))
        (unless pkg
          (error "Test package not found for ~S" module-name))
        (message "running tests for ~A..." module-name)
        (let ((*trace-output* *standard-output*))
          (lisp-unit:run-tests-from package-name))
        (message "running tests for ~A...done." module-name)))))

;;; test/run-all.l ends here.
