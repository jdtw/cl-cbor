;;;; conditions.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(define-condition unexpected-break (parse-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unexpected break code"))))
