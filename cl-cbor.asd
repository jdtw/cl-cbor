;;;; cl-cbor.asd

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(asdf:defsystem #:cl-cbor
  :description "Common lisp cbor encoder/decoder"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:trivial-utf-8)
  :serial t
  :components ((:file "package")
               (:file "int")
               (:file "cl-cbor")))
