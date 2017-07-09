;;;; decode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *decode-hash-table-initargs* nil
  "The caller may specify initial args to make-hash-table
for any hash tables found during decoding")
(defun make-decode-dict ()
  (apply #'make-hash-table *decode-hash-table-initargs*))

;; TODO What should happen if the whole stream is not consumed? Multiple values?
(defun decode (stream)
  (jump stream nil))

(defun decode-sequence (sequence)
  (with-input-from-sequence (stream sequence)
    (decode stream)))

;; Integers
(defjump ((#x00 #x1b)) (read-cbor-uint))
(defjump ((#x20 #x3b)) (- -1 (read-cbor-uint)))

;; Byte strings
(defjump ((#x40 #x5b)) (read-cbor-bytes))
(defjump (#x5f)
  (babel-streams:with-output-to-sequence (out)
    (decode-loop do (write-sequence decoded out))))

;; UTF8 strings
(defjump ((#x60 #x7b))
  (octets-to-string (read-cbor-bytes) :encoding :utf-8))
(defjump (#x7f)
  (with-output-to-string (out)
    (decode-loop do (write-string decoded out))))

;; Arrays
(defjump ((#x80 #x9b) #x9f) (decode-loop collect decoded))

;; Hash tables
(defjump ((#xa0 #xbb) #xbf)
  (decode-loop with dict = (make-decode-dict) do
               (unless (typep decoded 'string)
                 (warn 'dict-key-not-a-string :dict-key decoded))
               (setf (gethash decoded dict) (decode))
               finally (return dict)))

(defjump (#xc0) (error "Text-based date/time not implemented"))
(defjump (#xc1) (error "Epoch-based date/time not implemented"))
(defjump (#xc2) (error "Positive bignum not implemented"))
(defjump (#xc3) (error "Negative bignum not implemented"))
(defjump (#xc4) (error "Decimal Fraction not implemented"))
(defjump (#xc5) (error "Bigfloat not implemented"))
(defjump ((#xc6 #xd4)) (error "Tags not implemented"))
(defjump ((#xd5 #xd7)) (error "Expected conversion not implemented"))
(defjump ((#xd8 #xdb)) (error "Tags not implemented"))
(defjump ((#xe0 #xf3)) (error "Simple value not implemented"))
(defjump (#xf4) nil) ;false
(defjump (#xf5) t)   ;true
(defjump (#xf6) nil) ;null
(defjump (#xf7) 'undefined)
(defjump (#xf8) (error "Simple value not implemented"))
(defjump (#xf9) (decode-float16 (read-cbor-uint)))
(defjump (#xfa) (decode-float32 (read-cbor-uint)))
(defjump (#xfb) (decode-float64 (read-cbor-uint)))
(defjump (#xff) (error 'unexpected-break))
