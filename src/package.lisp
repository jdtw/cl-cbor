;;;; package.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor
  (:nicknames #:cbor)
  (:use #:cl
        #:ieee-floats)
  (:import-from #:trivial-utf-8
                #:string-to-utf-8-bytes)
  (:import-from #:alexandria
                #:mappend)
  (:export #:int->bytes
           #:bytes->int
           #:encode
           #:*encode-symbols-as-strings*
           #:*encode-nil-as*))
