;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *encode-symbols-as-strings* t
  "'foo and :foo will be encoded as \"FOO\"")
(defparameter *encode-nil-as* 'null
  "Possible values are 'list, 'null, or 'boolean")

(defun encode (thing)
  (etypecase thing
    (null (ecase *encode-nil-as*
            (null (initial-byte +simple+ +false+))
            (boolean (initial-byte +simple+ +null+))
            (list (encode-array thing))))
    ((unsigned-byte 64) (encode-uint thing))
    ((signed-byte 64) (encode-int thing))
    ((vector integer) (encode-bytes thing))
    (string (encode-utf8 thing))
    (list (encode-array thing))
    (hash-table (encode-dict thing))
    (float (encode-float thing))
    ;; We know this is 'true' because nil would have been
    ;; handled in the first case.
    (boolean (initial-byte +simple+ +true+))
    (symbol (if *encode-symbols-as-strings*
                (encode-utf8 (symbol-name thing))
                (error "*encode-symbols-as-strings* is nil")))))

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
  (encode-bytes (string-to-octets string :external-format :utf8)
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
