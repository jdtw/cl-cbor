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

(defun len-information (len)
  "The 5-bit additional information is either the integer itself (for additional
information values 0 through 23) or the length of additional data. Additional
information 24 means the value is represented in an additional uint8_t, 25 means
a uint16_t, 26 means a uint32_t, and 27 means a uint64_t."
  (declare ((unsigned-byte 64) len))
  (let ((bits (integer-length len)))
    (cond
      ((<= len 23) len)
      ((<= bits 8) 24)
      ((<= bits 16) 25)
      ((<= bits 32) 26)
      ((< bits 64) 27))))

(defun int-padding (nbytes)
  (let ((pad (cond
               ((<= nbytes 2) 0)
               ((<= nbytes 4) (- 4 nbytes))
               ((<= nbytes 8) (- 8 nbytes)))))
    (if (> pad 0)
        (loop repeat pad collect 0))))

(defun int->bytes (n)
  (declare ((unsigned-byte 64) n))
  (let ((nbytes (ceiling (integer-length n) 8)) bytes)
    (dotimes (i nbytes)
      (locally
          (declare (optimize (safety 0)))
        (push (ldb (byte 8 (* i 8)) n) bytes)))
    (nconc (int-padding nbytes) bytes)))
