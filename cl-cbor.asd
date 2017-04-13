;;;; cl-cbor.asd

(asdf:defsystem #:cl-cbor
  :description "Common lisp cbor encoder/decoder"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cl-cbor")))
