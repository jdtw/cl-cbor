;;;; package.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor
  (:nicknames #:cbor)
  (:use #:cl
        #:ieee-floats)
  (:import-from #:flexi-streams
                #:string-to-octets
                #:octets-to-string
                #:make-in-memory-input-stream
                #:make-in-memory-output-stream
                #:get-output-stream-sequence)
  (:import-from #:alexandria
                #:mappend
                #:with-gensyms)
  (:export #:int->bytes
           #:bytes->int
           #:*encode-symbols-as-strings*
           #:*encode-nil-as*
           #:encode
           #:with-output
           #:with-output-to-sequence
           #:with-array
           #:encode-array-element
           #:with-dict
           #:encode-key-value
           #:with-utf8
           #:encode-utf8
           #:with-bytes
           #:encode-bytes))
