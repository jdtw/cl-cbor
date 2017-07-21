;;;; package.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor
  (:nicknames #:cbor)
  (:use #:cl
        #:ieee-floats)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:nibbles
                #:write-ub16/be
                #:write-ub32/be
                #:write-ub64/be
                #:read-ub16/be
                #:read-ub32/be
                #:read-ub64/be)
  (:import-from #:babel
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:babel-streams
                #:make-in-memory-input-stream
                #:make-in-memory-output-stream
                #:get-output-stream-sequence
                #:with-input-from-sequence
                #:with-output-to-sequence)
  (:import-from #:bit-smasher
                #:int->octets
                #:octets->int)
  (:import-from #:local-time
                #:timestamp
                #:format-rfc3339-timestring
                #:parse-timestring
                #:unix-to-timestamp)
  (:export #:*encode-symbols-as-strings*
           #:*encode-nil-as*
           #:encode
           #:encode-to-sequence
           #:with-output-to-sequence
           #:with-input-from-sequence
           #:with-array
           #:encode-array-element
           #:with-dict
           #:encode-key-value
           #:with-utf8
           #:encode-utf8
           #:with-bytes
           #:encode-bytes
           #:encode-tagged
           #:encode-timestring
           #:encode-epoch

           #:decode
           #:decode-sequence
           #:*decode-hash-table-initargs*

           #:add-tag-decoder
           #:remove-tag-decoder
           #:deftag
           #:*ignore-tags*
           #:tagged-item
           #:item
           #:item-tag
           #:make-tagged-item
           #:+time+
           #:+epoch+
           #:+bignum+
           #:+neg-bignum+
           #:+self-describe-cbor+

           #:pprint-cbor
           #:pprint-cbor-sequence))
