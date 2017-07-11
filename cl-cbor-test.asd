;;;; cl-cbor-test.asd

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(asdf:defsystem #:cl-cbor-test
  :description "Tests for cl-cbor, a cbor encoder/decoder"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:cl-cbor
               #:prove
               #:alexandria
               #:local-time)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :serial t
  :components ((:test-file "tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
