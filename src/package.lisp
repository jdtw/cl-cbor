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
                #:with-gensyms)
  (:import-from #:nibbles
                #:write-ub16/be
                #:write-ub32/be
                #:write-ub64/be
                #:read-ub16/be
                #:read-ub32/be
                #:read-ub64/be)
  (:export #:*encode-symbols-as-strings*
           #:*encode-nil-as*
           #:encode
           #:encode-to-sequence
           #:with-output
           #:with-output-to-sequence
           #:with-array
           #:encode-array-element
           #:with-dict
           #:encode-key-value
           #:with-utf8
           #:encode-utf8
           #:with-bytes
           #:encode-bytes
           #:decode
           #:decode-sequence
           #:*decode-hash-table-initargs*))
