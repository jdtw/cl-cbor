;;;; decode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *decode-hash-table-initargs* nil)
(defun make-decode-dict ()
  (apply #'make-hash-table *decode-hash-table-initargs*))

(defparameter *jump*
  (make-array 256 :element-type 'function
                  :initial-element (lambda (stream)
                                     (declare (ignore stream))
                                     (error "Invalid data type"))))
(defun decode* (stream)
  (funcall (aref *jump* (read-byte stream)) stream))

(defun decode (stream)
  (decode*
   (flexi-streams:make-flexi-stream
    stream
    :element-type '(unsigned-byte 8))))

(defun decode-sequence (sequence)
  (flexi-streams:with-input-from-sequence (stream sequence)
    (setf stream
          (flexi-streams:make-flexi-stream stream :element-type '(unsigned-byte 8)))
    (decode* stream)))

;; Anaphoric -- caller can use the function ~(read-cbor-uint)~ to read
;; a uint from the stream, as defined by the low five bits of the byte,
;; and ~stream~ to refer to the stream.
(defmacro jump-lambda (byte &body body)
  `(lambda (stream)
     (labels ((read-cbor-uint ()
                ,(case (info byte)
                   (24 '(read-byte stream))
                   (25 '(read-ub16/be stream))
                   (26 '(read-ub32/be stream))
                   (27 '(read-ub64/be stream))
                   (otherwise (info byte))))
              (read-cbor-bytes ()
                (let ((bytes
                        (make-array
                         (read-cbor-uint)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
                  (read-sequence bytes stream)
                  bytes)))
       (declare (ignorable (function read-cbor-bytes)))
       (macrolet ((decode-loop (&rest rest)
                    (with-gensyms(b result)
                      ,(if (= (info byte) 31)
                           ``(let ((,result (loop for ,b = (peek-byte stream)
                                                  until (= ,b #xff)
                                                  ,@rest)))
                               (read-byte stream)
                               ,result)
                           ``(loop repeat (read-cbor-uint)
                                   ,@rest)))))
         ,@body))))

(defmacro defjump (pos &body body)
  (let ((indexes (if (atom pos) (list pos pos) pos)))
    `(progn
       ,@(loop for i from (first indexes)
               upto (second indexes) collect
               `(setf (aref *jump* ,i) (jump-lambda ,i ,@body))))))

;; Integers
(defjump (#x00 #x1b) (read-cbor-uint))
(defjump (#x20 #x3b) (- -1 (read-cbor-uint)))

;; Byte strings
(defjump (#x40 #x5b) (read-cbor-bytes))
(defjump #x5f
  (flexi-streams:with-output-to-sequence (out)
    (decode-loop do (write-sequence (decode* stream) out))))

;; UTF8 strings
(defjump (#x60 #x7b)
  (octets-to-string (read-cbor-bytes) :encoding :utf-8))
(defjump #x7f
  (with-output-to-string (out)
    (decode-loop do (write-string (decode* stream) out))))

;; Arrays
(defjump (#x80 #x9b) (decode-loop collect (decode* stream)))
(defjump #x9f (decode-loop collect (decode* stream)))

;; Hash tables
(defjump (#xa0 #xbb)
  (decode-loop with dict = (make-decode-dict)
               for k = (decode* stream)
               for v = (decode* stream)
               do (setf (gethash k dict) v)
               finally (return dict)))
(defjump #xbf
  (decode-loop with dict = (make-decode-dict)
               for k = (decode* stream)
               for v = (decode* stream)
               do (setf (gethash k dict) v)
               finally (return dict)))

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
(defjump #xf9 (decode-float16 (read-cbor-uint)))
(defjump #xfa (decode-float32 (read-cbor-uint)))
(defjump #xfb (decode-float64 (read-cbor-uint)))
(defjump #xff (error "Unexpected break code"))
