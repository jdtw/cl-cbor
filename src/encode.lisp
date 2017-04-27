;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

;;; Lisp data encoding

(defparameter *encode-symbols-as-strings* t
  "'foo and :foo will be encoded as \"FOO\"")
(defparameter *encode-nil-as* 'null
  "Possible values are 'list, 'null, or 'boolean")

(defun encode (thing &key as-list)
  (flexi-streams:with-output-to-sequence
      (stream :as-list as-list)
    (encode-to-stream thing stream)))

(defgeneric encode-to-stream (thing stream)
  (:documentation "Encodes a Common Lisp type to cbor"))

(defmethod encode-to-stream ((thing null) stream)
  (declare (ignore thing))
  (ecase *encode-nil-as*
    (null (write-initial-byte +simple+ +null+ stream))
    (boolean (write-initial-byte +simple+ +false+ stream))
    (list (write-initial-byte +array+ 0 stream))))

(defmethod encode-to-stream ((thing integer) stream)
  (unless (<= (integer-length thing) 64)
    (error "Only 64-bit integers are supported"))
  (if (< thing 0)
      (encode-uint (- -1 thing) stream :type +neg-int+)
      (encode-uint thing stream)))

(defmethod encode-to-stream ((thing vector) stream)
  (unless (seq-of-bytes-p thing)
    (error "Requires a sequence of bytes"))
  (encode-uint (length thing) stream :type +bytes+)
  (write-sequence thing stream))

(defmethod encode-to-stream ((thing string) stream)
  (encode-uint (length thing) stream :type +utf8+)
  (write-sequence (string-to-octets thing :external-format :utf8)
                  stream))

(defmethod encode-to-stream ((thing list) stream)
  (encode-uint (length thing) stream :type +array+)
  (loop for x in thing do
    (encode-to-stream x stream)))

(defmethod encode-to-stream ((thing hash-table) stream)
  (encode-uint (hash-table-count thing) stream :type +dict+)
  (loop for k being the hash-keys of thing using (hash-value v)
        do (encode-to-stream k stream) (encode-to-stream v stream)))

(defmethod encode-to-stream ((thing float) stream)
  (multiple-value-bind (encoder addl-info)
      (etypecase thing
        (single-float (values #'encode-float32 26))
        (double-float (values #'encode-float64 27)))
    (write-initial-byte +simple+ addl-info stream)
    (write-sequence (int->bytes (funcall encoder thing)) stream)))

(defmethod encode-to-stream ((thing ratio) stream)
  (encode-to-stream (coerce thing 'double-float) stream))

(defmethod encode-to-stream ((thing (eql t)) stream)
  (write-initial-byte +simple+ (if thing +true+ +false+) stream))

(defmethod encode-to-stream ((thing symbol) stream)
  (unless *encode-symbols-as-strings*
    (error "*encode-symbols-as-strings* is nil"))
  (encode-to-stream (symbol-name thing) stream))

(defun encode-uint (n stream &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (write-initial-byte type (addl-info n) stream)
  (when (> n 23) (write-sequence (int->bytes n) stream)))

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
