;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

;;; Lisp data encoding

(defparameter *encode-symbols-as-strings* t
  "'foo and :foo will be encoded as \"FOO\"")
(defparameter *encode-nil-as* 'null
  "Possible values are 'list, 'null, or 'boolean")

(defgeneric encode (thing stream)
  (:documentation "Encodes a Common Lisp type to cbor"))

(defmethod encode ((thing null) stream)
  (declare (ignore thing))
  (ecase *encode-nil-as*
    (null (write-initial-byte +simple+ +null+ stream))
    (boolean (write-initial-byte +simple+ +false+ stream))
    (list (write-initial-byte +array+ 0 stream))))

(defmethod encode ((thing integer) stream)
  (unless (<= (integer-length thing) 64)
    (error "Only 64-bit integers are supported"))
  (if (< thing 0)
      (encode-uint (- -1 thing) stream :type +neg-int+)
      (encode-uint thing stream)))

(defmethod encode ((thing vector) stream)
  (unless (seq-of-bytes-p thing)
    (error "Requires a sequence of bytes"))
  (encode-uint (length thing) stream :type +bytes+)
  (write-sequence thing stream))

(defmethod encode ((thing string) stream)
  (let ((octets (string-to-octets thing :external-format :utf8)))
    (encode-uint (length octets) stream :type +utf8+)
    (write-sequence octets stream)))

(defmethod encode ((thing list) stream)
  (encode-uint (length thing) stream :type +array+)
  (loop for x in thing do
    (encode x stream)))

(defmethod encode ((thing hash-table) stream)
  (encode-uint (hash-table-count thing) stream :type +dict+)
  (loop for k being the hash-keys of thing using (hash-value v)
        do (encode k stream) (encode v stream)))

(defmethod encode ((thing float) stream)
  (let ((encoder (etypecase thing
                   (single-float #'encode-float32)
                   (double-float #'encode-float64))))
    (encode-uint (funcall encoder thing) stream :type +simple+)))

(defmethod encode ((thing ratio) stream)
  (encode (coerce thing 'double-float) stream))

(defmethod encode ((thing (eql t)) stream)
  (write-initial-byte +simple+ (if thing +true+ +false+) stream))

(defmethod encode ((thing symbol) stream)
  (unless *encode-symbols-as-strings*
    (error "*encode-symbols-as-strings* is nil"))
  (encode (symbol-name thing) stream))

(defun encode-uint (n stream &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (let ((bits (integer-length n)))
    (multiple-value-bind (addl-info writer)
        (cond
          ((<= n 23) (values n (lambda (n s) (declare (ignore n s)))))
          ((<= bits 8) (values +ub8+ #'write-byte))
          ((<= bits 16) (values +ub16+ #'write-ub16/be))
          ((<= bits 32) (values +ub32+ #'write-ub32/be))
          ((< bits 64) (values +ub64+ #'write-ub64/be)))
      (write-initial-byte type addl-info stream)
      (funcall writer n stream))))

(defun encode-to-sequence (thing &key as-list)
  (flexi-streams:with-output-to-sequence
      (stream :as-list as-list)
    (encode thing stream)))

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
  (with-gensyms (body)
    `(defmacro ,name (&body ,body)
       `(progn
          (write-initial-byte ,,type +indefinite+ *cbor-output*)
          ,@,body
          (write-byte +break+ *cbor-output*)))))

(def-indefinite-encoder with-array +array+)
(defun encode-array-element (e)
  (encode e *cbor-output*))

(def-indefinite-encoder with-dict +dict+)
(defun encode-key-value (k v)
  (encode k *cbor-output*)
  (encode v *cbor-output*))

(def-indefinite-encoder with-utf8 +utf8+)
(defun encode-utf8 (string)
  (encode (coerce string 'string) *cbor-output*))

(def-indefinite-encoder with-bytes +bytes+)
(defun encode-bytes (bytes)
  (encode (coerce bytes 'vector) *cbor-output*))
