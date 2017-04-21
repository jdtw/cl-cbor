;;;; misc.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defun int-padding (nbytes)
  "Returns a list of zeros required to pad to 32 or 64 bits. (Values less than
or equal to 16 bits long dont need padding, because we can express them as
uint8_t or uint16_t.)"
  (declare ((integer 0 8) nbytes))
  (let ((pad (cond
               ((<= nbytes 2) 0)
               ((<= nbytes 4) (- 4 nbytes))
               ((<= nbytes 8) (- 8 nbytes)))))
    (if (> pad 0)
        (loop repeat pad collect 0))))

(defun int->bytes (n)
  (declare ((unsigned-byte 64) n))
  (let ((nbytes (ceiling (integer-length n) 8)) bytes)
    (dotimes (i (if (= nbytes 0) 1 nbytes))
      (locally
          (declare (optimize (safety 0)))
        (push (ldb (byte 8 (* i 8)) n) bytes)))
    (nconc (int-padding nbytes) bytes)))

(defun bytes->int (bytes)
  (let ((n 0) (i 0))
    (declare ((unsigned-byte 64) n)
             ((unsigned-byte 4) i))
    (dolist (b (reverse bytes) n)
      (locally
          (declare (optimize (safety 0)))
        (setf (ldb (byte 8 (* i 8)) n) b)
        (incf i)))))

(defun list-of-bytes-p (bytes)
  (and (typep bytes 'list)
       (every (lambda (b) (typep b '(unsigned-byte 8)))
              bytes)))

;; ieee-floats contains encoders/decoders for 32 and 64 bit
;; floats, but not 16
(make-float-converters encode-float16 decode-float16 5 10 nil)
