;;;; decode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *decode-hash-table-initargs* nil)

(defparameter *jump*
  (make-array 256 :element-type 'function
                  :initial-element (lambda (stream)
                                     (declare (ignore stream))
                                     (error "Invalid data type"))))
(defun decode (stream)
  (funcall (aref *jump* (read-byte stream)) stream))

(defun decode-sequence (sequence)
  (flexi-streams:with-input-from-sequence (stream sequence)
    (decode stream)))

;; Anaphoric -- caller can use the function ~(read-uint)~ to read
;; a uint from the stream, as defined by the low five bits of the byte,
;; and ~stream~ to refer to the stream.
(defmacro defjump (pos &body body)
  (let ((indexes (if (atom pos) (list pos pos) pos)))
    `(progn
       ,@(loop for i from (first indexes)
               upto (second indexes) collect
               `(setf (aref *jump* ,i)
                      (lambda (stream)
                        (declare (ignorable stream))
                        (labels ((read-uint ()
                                   ,(case (info i)
                                      (24 '(read-byte stream))
                                      (25 '(read-ub16/be stream))
                                      (26 '(read-ub32/be stream))
                                      (27 '(read-ub64/be stream))
                                      (otherwise (info i)))))
                          (declare (ignorable (function read-uint)))
                          ,@body)))))))

(defjump (#x00 #x1b) (read-uint))
(defjump (#x20 #x3b) (- -1 (read-uint)))
(defjump (#x40 #x5b)
  (let ((bytes (make-array (read-uint)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (read-sequence bytes stream)
    bytes))
(defjump #x5f (error "Indefinite byte string not implemented"))
(defjump (#x60 #x7b)
  (let ((bytes (make-array (read-uint)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (read-sequence bytes stream)
    (octets-to-string bytes :external-format :utf8)))
(defjump #x7f (error "Indefinite strings not implemented"))
(defjump #x80 nil)
(defjump (#x81 #x9b)
  (loop for i from 1 upto (read-uint)
        collect (decode stream)))
(defjump #x9f (error "Indefinite array not implemented"))
(defjump #xa0 (apply #'make-hash-table *decode-hash-table-initargs*))
(defjump (#xa1 #xbb)
  (let ((dict (apply #'make-hash-table *decode-hash-table-initargs*)))
    (loop for i from 1 upto (read-uint)
          do (let ((k (decode stream)) (v (decode stream)))
               (setf (gethash k dict) v)))
    dict))
(defjump #x9f (error "Indefinite hash table not implemented"))
(defjump #xc0 (error "Text-based date/time not implemented"))
(defjump #xc1 (error "Epoch-based date/time not implemented"))
(defjump #xc2 (error "Positive bignum not implemented"))
(defjump #xc3 (error "Negative bignum not implemented"))
(defjump #xc4 (error "Decimal Fraction not implemented"))
(defjump #xc5 (error "Bigfloat not implemented"))
(defjump (#xc6 #xd4) (error "Tags not implemented"))
(defjump (#xd5 #xd7) (error "Expected conversion not implemented"))
(defjump (#xd8 #xdb) (error "Tags not implemented"))
(defjump (#xe0 #xf3) (error "Simple value not implemented"))
(defjump #xf4 nil) ;false
(defjump #xf5 t)   ;true
(defjump #xf6 nil) ;null
(defjump #xf7 'undefined)
(defjump #xf8 (error "Simple value not implemented"))
(defjump #xf9 (decode-float16 (read-uint)))
(defjump #xfa (decode-float32 (read-uint)))
(defjump #xfb (decode-float64 (read-uint)))
(defjump #xff (error "Unexpected break code"))
