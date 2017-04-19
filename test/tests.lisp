;;;; tests.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor-test
  (:use #:cl
        #:prove))

(in-package #:cl-cbor-test)

(plan 2)

(is '(0) (cbor:int->bytes 0))
(is '(1) (cbor:int->bytes 1))

(finalize)
