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
                #:make-in-memory-output-stream)
  (:import-from #:alexandria
                #:mappend)
  (:export #:int->bytes
           #:bytes->int
           #:encode
           #:*encode-symbols-as-strings*
           #:*encode-nil-as*))
