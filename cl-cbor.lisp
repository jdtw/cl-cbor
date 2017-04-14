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

(defun len-information (len &aux (bits (integer-length len)))
  (declare ((unsigned-byte 64) len))
  (cond
    ((<= len 23) len)
    ((<= bits 8) 24)
    ((<= bits 16) 25)
    ((<= bits 32) 26)
    ((<= bits 64) 27)))

(defun int-padding (nbits)
  (let ((pad (cond
               ((<= nbits 16) 0)
               ((<= nbits 32) (- 4 (ceiling nbits 8)))
               ((<= nbits 64) (- 8 (ceiling nbits 8))))))
    (if (> pad 0)
        (loop repeat pad collect 0))))

(defun int->bytes (n)
  (declare ((unsigned-byte 64) n))
  (let* (bytes
         (nbits (integer-length n))
         (nbytes (ceiling nbits 8)))
    (dotimes (i nbytes)
      (locally
          (declare (optimize (safety 0)))
        (push (ldb (byte 8 (* i 8)) n) bytes)))
    (nconc (int-padding nbits) bytes)))
