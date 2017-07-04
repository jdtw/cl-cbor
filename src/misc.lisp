;;;; misc.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defun write-uint (n stream)
  (declare ((unsigned-byte 64) n))
  (let ((bits (integer-length n)))
    (funcall (cond
               ((<= bits 8) #'write-byte)
               ((<= bits 16) #'write-ub16/be)
               ((<= bits 32) #'write-ub32/be)
               ((< bits 64) #'write-ub64/be))
             n stream)))

(defun seq-of-bytes-p (bytes)
  (every (lambda (b) (typep b '(unsigned-byte 8)))
         bytes))

;; ieee-floats contains encoders/decoders for 32 and 64 bit
;; floats, but not 16
(make-float-converters encode-float16 decode-float16 5 10 nil)
