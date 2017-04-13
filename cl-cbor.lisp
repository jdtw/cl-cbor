;;;; cl-cbor.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defun initial-byte (hi lo)
  "The initial byte of each data item contains both information about the major
type (the high-order 3 bits) and additional information (the low-order 5 bits)."
  (let ((b 0))
    (declare
     ((unsigned-byte 8) b)
     ((unsigned-byte 3) hi)
     ((unsigned-byte 5) lo))
    (setf (ldb (byte 3 5) b) hi)
    (setf (ldb (byte 5 0) b) lo)
    b))

(defun information (b)
  (declare ((unsigned-byte 8) b))
  (values
   (ldb (byte 3 5) b)
   (ldb (byte 5 0) b)))

(defun uint-information (n &aux (n-bits (integer-length n)))
  (declare ((unsigned-byte 64) n))
  (cond
    ((<= n 23) (values n 0))
    ((<= n-bits  8) (values 24 0))
    ((<= n-bits 16) (values 25 0))
    ((<= n-bits 32) (values 26 (- 4 (ceiling n-bits 8))))
    ((<= n-bits 64) (values 27 (- 8 (ceiling n-bits 8))))))

(defun int->bytes (n &aux (nbytes (ceiling (integer-length n) 8)))
  (declare ((unsigned-byte 64) n))
  (let (bytes)
    (dotimes (i nbytes (values bytes))
      (locally
          (declare (optimize (safety 0)))
        (push (ldb (byte 8 (* i 8)) n) bytes)))))
