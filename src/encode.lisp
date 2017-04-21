;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *encode-symbols-as-strings* t)

(defun encode (thing)
  (etypecase thing
    ((unsigned-byte 64) (encode-uint thing))
    ((signed-byte 64) (encode-int thing))
    ((vector integer) (encode-bytes thing))
    (string (encode-utf8 thing))
    (symbol (if *encode-symbols-as-strings*
                (encode-utf8 (symbol-name thing))
                (error "*encode-symbols-as-strings* is nil")))
    (list (encode-array thing))
    (hash-table (encode-dict thing))
    (float (encode-float thing))))

(defun encode-uint (n &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (cons
   (initial-byte type (addl-info n))
   (if (> n 23) (int->bytes n))))

(defun encode-int (n)
  (declare ((signed-byte 64) n))
  (if (< n 0)
      (encode-uint (- -1 n) :type +neg-int+)
      (encode-uint n)))

(defun encode-bytes (bytes &key (type +bytes+))
  (let ((byte-list (coerce bytes 'list)))
    (if (not (list-of-bytes-p byte-list))
        (error "Requires a sequence of bytes"))
    (nconc (encode-uint (length bytes) :type type)
           byte-list)))

(defun encode-utf8 (string)
  (encode-bytes (string-to-utf-8-bytes string)
                :type +utf8+))

(defun encode-array (array)
  (nconc (encode-uint (length array) :type +array+)
         (mappend #'encode array)))

(defun encode-dict (dict)
  (nconc (encode-uint (hash-table-count dict) :type +dict+)
         (loop for k being the hash-keys of dict
                 using (hash-value v)
               append (append (encode k) (encode v)))))

;; We don't encode half-floats
(defun encode-float (f)
  (multiple-value-bind (encoder addl-info)
      (etypecase f
        (single-float (values #'encode-float32 26))
        (double-float (values #'encode-float64 27)))
    (cons (initial-byte +simple+ addl-info)
          (int->bytes (funcall encoder f)))))
