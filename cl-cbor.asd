;;;; cl-cbor.asd

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(asdf:defsystem #:cl-cbor
  :description "Common lisp cbor encoder/decoder"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:alexandria
               #:ieee-floats
               #:nibbles
               #:babel
               #:babel-streams)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "cbor")
               (:file "misc")
               (:file "conditions")
               (:file "encode")
               (:file "jump")
               (:file "decode"))
  :in-order-to ((test-op (test-op cl-cbor-test))))
