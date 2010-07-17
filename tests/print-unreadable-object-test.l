;;; -*- mode: lisp; package: user -*-
;;;

(defpackage :cl.test.print-unreadable-object)
(in-package :cl.test.print-unreadable-object)

(require "cl")
(cl::install)

(require "lisp-unit")
(use-package :lisp-unit)

(remove-all-tests)

(define-test print-unreadable-object

  (assert-prints "#<integer 3>"
      (print-unreadable-object (3 nil :type t)
        (princ 3)))

  (assert-prints "#<integer 2>"
      (print-unreadable-object (2 nil :type t)
        (princ 2)))

  (assert-equal "#<integer 2>"
      (with-output-to-string (out)
        (print-unreadable-object (2 out :type t)
          (princ 2 out))))

  (assert-equal "#<foobarbaz>"
      (with-output-to-string (out)
        (print-unreadable-object (t out)
          (princ "foobarbaz" out))))

  (assert-equal "#<>"
      (with-output-to-string (out)
        (print-unreadable-object (:X out))))

  (assert-equal "#<symbol>"
      (with-output-to-string (out)
        (print-unreadable-object (:X out :type t))))

  (assert-equal "#<>"
      (with-output-to-string (out)
        (print-unreadable-object (3 out :type nil))))

  (let ((table (make-hash-table)))
    (assert-equal
        (format nil "#<hash-table ~D>" (si:address-of table))
        (with-output-to-string (out)
          (print-unreadable-object (table out :type t :identity t))))

    (assert-equal
        (format nil "#<something ~D>" (si:address-of table))
        (with-output-to-string (out)
          (print-unreadable-object (table out :type nil :identity t)
            (princ "something" out))))

    (assert-equal
        (format nil "#<hash-table something ~D>" (si:address-of table))
        (with-output-to-string (out)
          (print-unreadable-object (table out :type t :identity t)
            (princ "something" out))))

    (assert-equal
        (format nil "#<something ~D>" (si:address-of table))
        (with-output-to-string (out)
          (print-unreadable-object (table out :type nil :identity t)
            (princ "something" out))))
    )


  )


