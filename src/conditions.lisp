;;;; conditions.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(define-condition unexpected-break (parse-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unexpected break code"))))

(define-condition dict-key-not-a-string (simple-warning)
  ((dict-key :initarg :dict-key
             :reader dict-key))
  (:report (lambda (condition stream)
             (format stream "The dictionary key ~A is not a STRING"
                     (dict-key condition)))))
