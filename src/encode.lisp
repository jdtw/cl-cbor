;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *encode-symbols-as-strings* t
  "'foo and :foo will be encoded as \"FOO\"")
(defparameter *encode-nil-as* 'null
  "Possible values are 'list, 'null, or 'boolean")

(defun encode (thing &key as-list)
  (flexi-streams:with-output-to-sequence
      (stream :as-list as-list)
    (encode-to-stream thing stream)))

(defun encode-to-stream (thing stream)
  (let ((encoder
          (etypecase thing
            (null #'encode-null)
            ((unsigned-byte 64) #'encode-uint)
            ((signed-byte 64) #'encode-int)
            ((vector integer) #'encode-bytes)
            (string #'encode-utf8)
            (list #'encode-array)
            (hash-table #'encode-dict)
            (float #'encode-float)
            (boolean #'encode-bool)
            (symbol #'encode-symbol))))
    (funcall encoder thing stream)))

(defun encode-null (null stream)
  (declare (null null))
  (ecase *encode-nil-as*
    (null (write-byte (initial-byte +simple+ +null+) stream))
    (boolean (write-byte (initial-byte +simple+ +false+) stream))
    (list (encode-array null stream))))

(defun encode-bool (b stream)
  (declare (boolean b))
  (write-byte (initial-byte +simple+ (if b +true+ +false+))
              stream))

(defun encode-symbol (sym stream)
  (declare (symbol sym))
  (if *encode-symbols-as-strings*
      (encode-utf8 (symbol-name sym) stream)
      (error "*encode-symbols-as-strings* is nil")))

(defun encode-uint (n stream &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (write-byte (initial-byte type (addl-info n)) stream)
  (when (> n 23) (write-sequence (int->bytes n) stream)))

(defun encode-int (n stream)
  (declare ((signed-byte 64) n))
  (if (< n 0)
      (encode-uint (- -1 n) stream :type +neg-int+)
      (encode-uint n stream)))

(defun encode-bytes (bytes stream &key (type +bytes+))
  (unless (seq-of-bytes-p bytes)
    (error "Requires a sequence of bytes"))
  (encode-uint (length bytes) stream :type type)
  (write-sequence bytes stream))

(defun encode-utf8 (string stream)
  (encode-bytes
   (string-to-octets string :external-format :utf8)
   stream
   :type +utf8+))

(defun encode-array (array stream)
  (encode-uint (length array) stream :type +array+)
  (loop for e being the elements of array do
        (encode-to-stream e stream)))

(defun encode-dict (dict stream)
  (encode-uint (hash-table-count dict) stream :type +dict+)
  (loop for k being the hash-keys of dict using (hash-value v)
        do (encode-to-stream k stream) (encode-to-stream v stream)))

;; We don't encode half-floats
(defun encode-float (f stream)
  (multiple-value-bind (encoder addl-info)
      (etypecase f
        (single-float (values #'encode-float32 26))
        (double-float (values #'encode-float64 27)))
    (write-byte (initial-byte +simple+ addl-info) stream)
    (write-sequence (int->bytes (funcall encoder f)) stream)))

;;; Streaming encoding

(defparameter *cbor-output* nil)

(defmacro with-output ((stream) &body body)
  `(let ((*cbor-output* ,stream))
     ,@body))

(defmacro with-output-to-sequence ((&key as-list) &body body)
  `(let ((*cbor-output* (make-in-memory-output-stream)))
     ,@body
     (get-output-stream-sequence *cbor-output* :as-list ,as-list)))

(defmacro def-indefinite-encoder (name type)
  (alexandria:with-gensyms (body)
    `(defmacro ,name (() &body ,body)
       `(progn
          (write-initial-byte ,,type +indefinite+ *cbor-output*)
          ,@,body
          (write-byte +break+ *cbor-output*)))))

(def-indefinite-encoder with-array +array+)
(defun encode-array-element (e)
  (encode-to-stream e *cbor-output*))

(def-indefinite-encoder with-dict +dict+)
(defun encode-key-value (k v)
  (encode-to-stream k *cbor-output*)
  (encode-to-stream v *cbor-output*))

(def-indefinite-encoder with-utf8 +utf8+)
(defun encode-utf8-chunk (string)
  (encode-utf8 string *cbor-output*))

(def-indefinite-encoder with-bytes +bytes+)
(defun encode-byte-chunk (bytes)
  (encode-bytes bytes *cbor-output*))
