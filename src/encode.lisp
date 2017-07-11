;;;; encode.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

;;; Lisp data encoding

(defparameter *encode-symbols-as-strings* nil
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
  (if (<= (integer-length thing) 64)
      (if (< thing 0)
          (encode-uint (- -1 thing) stream :type +neg-int+)
          (encode-uint thing stream))
      (encode-bignum thing stream)))

(defmethod encode ((thing vector) stream)
  (unless (seq-of-bytes-p thing)
    (error "Requires a sequence of bytes"))
  (encode-uint (length thing) stream :type +bytes+)
  (write-sequence thing stream))

(defmethod encode ((thing string) stream)
  (let ((octets (string-to-octets thing :encoding :utf-8)))
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
  (multiple-value-bind (info encoder writer)
      (etypecase thing
        (single-float (values +ub32+ #'encode-float32 #'write-ub32/be))
        (double-float (values +ub64+ #'encode-float64 #'write-ub64/be)))
    (write-initial-byte +simple+ info stream)
    (funcall writer (funcall encoder thing) stream)))

(defmethod encode ((thing ratio) stream)
  (encode (coerce thing 'double-float) stream))

(defmethod encode ((thing (eql t)) stream)
  (write-initial-byte +simple+ (if thing +true+ +false+) stream))

(defmethod encode ((thing symbol) stream)
  (unless *encode-symbols-as-strings*
    (error "*encode-symbols-as-strings* is nil"))
  (encode (symbol-name thing) stream))

(defun encode-tagged (tag thing stream)
  (encode (make-tagged-item tag thing) stream))

(defmethod encode ((thing tagged-item) stream)
  (encode-uint (item-tag thing) stream :type +tags+)
  (encode (item thing) stream))

(defmethod encode-timestring (string stream)
  (check-type string string)
  (encode-tagged +time+ string stream))

(defmethod encode ((thing timestamp) stream)
  (encode-timestring (format-rfc3339-timestring nil thing) stream))

(defmethod encode-epoch (epoch stream)
  (check-type epoch (or float
                        (unsigned-byte 64)
                        (unsigned-byte 64)))
  (encode-tagged +epoch+ epoch stream))

(defun encode-uint (n stream &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (let ((bits (integer-length n)))
    (multiple-value-bind (info writer)
        (cond
          ((<= n 23) (values n (lambda (n s) (declare (ignore n s)))))
          ((<= bits 8) (values +ub8+ #'write-byte))
          ((<= bits 16) (values +ub16+ #'write-ub16/be))
          ((<= bits 32) (values +ub32+ #'write-ub32/be))
          ((<= bits 64) (values +ub64+ #'write-ub64/be)))
      (write-initial-byte type info stream)
      (funcall writer n stream))))

(defun encode-bignum (n stream)
  (apply #'encode-tagged
         (if (< n 0)
             (list +neg-bignum+ (- -1 n))
             (list +bignum+ n))
         stream))

(defun encode-to-sequence (thing &key (return-as 'vector))
  (with-output-to-sequence (stream
                            :return-as return-as
                            :element-type '(unsigned-byte 8))
    (encode thing stream)))

;;; Streaming encoding

(defmacro def-encoder (name type stream (encoder capture &body encode))
  `(defmacro ,name ((,stream) &body body)
     `(flet ((,',encoder ,',capture ,@',encode))
        (declare (ignorable (function ,',encoder)))
        (write-initial-byte ,,type +indefinite+ ,,stream)
        ,@body
        (write-byte +break+ ,,stream))))

(def-encoder with-array +array+ stream
    (encode-array-element (e)
      (encode e stream)))

(def-encoder with-dict +dict+ stream
    (encode-key-value (k v)
      (unless (typep k 'string)
        (warn 'dict-key-not-a-string :dict-key k))
      (encode k stream)
      (encode v stream)))

(def-encoder with-utf8 +utf8+ stream
    (encode-utf8 (utf8)
      (check-type utf8 string)
      (encode utf8 stream)))

(def-encoder with-bytes +bytes+ stream
    (encode-bytes (bytes)
      (check-type bytes (vector integer))
      (encode bytes stream)))
