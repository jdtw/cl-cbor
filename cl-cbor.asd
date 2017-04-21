;;;; cl-cbor.asd

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(asdf:defsystem #:cl-cbor
  :description "Common lisp cbor encoder/decoder"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:trivial-utf-8
               #:alexandria
               #:ieee-floats)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "initial-byte")
               (:file "misc")
               (:file "encode"))
  :in-order-to ((test-op (test-op cl-cbor-test))))
