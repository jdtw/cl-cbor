;;;; encode.lisp

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
  "The initial byte of each data item contains both information about the major
type (the high-order 3 bits) and additional information (the low-order 5 bits)."
  (declare ((unsigned-byte 8) b))
  (values
   (ldb (byte 3 5) b)
   (ldb (byte 5 0) b)))

(defun addl-info (len)
  (declare ((unsigned-byte 64) len))
  (let ((bits (integer-length len)))
    (cond
      ((<= len 23) len)
      ((<= bits 8) 24)
      ((<= bits 16) 25)
      ((<= bits 32) 26)
      ((< bits 64) 27))))

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

(defconstant +uint+ 0
  "Major type 0: an unsigned integer. The 5-bit additional information is either
the integer itself (for additional information values 0 through 23) or the
length of additional data. Additional information 24 means the value is
represented in an additional uint8_t, 25 means a uint16_t, 26 means a uint32_t,
and 27 means a uint64_t.")

(defun encode-uint (n &key (type +uint+))
  (declare ((unsigned-byte 64) n))
  (cons
   (initial-byte type (addl-info n))
   (if (> n 23) (int->bytes n))))

(defconstant +neg-int+ 1
  "Major type 1: a negative integer. The encoding follows the rules for unsigned
integers (major type 0), except that the value is then -1 minus the encoded
unsigned integer.")

(defun encode-int (n)
  (declare ((signed-byte 64) n))
  (if (< n 0)
      (encode-uint (- -1 n) :type +neg-int+)
      (encode-uint n)))

(defconstant +bytes+ 2
  "Major type 2: a byte string. The string's length in bytes is represented
following the rules for positive integers (major type 0).")

(defun list-of-bytes-p (bytes)
  (and (typep bytes 'list)
       (every (lambda (b) (typep b '(unsigned-byte 8)))
              bytes)))

(defun encode-bytes (bytes &key (type +bytes+))
  (let ((byte-list (coerce bytes 'list)))
    (if (not (list-of-bytes-p byte-list))
        (error "Requires a sequence of bytes"))
    (nconc (encode-uint (length bytes) :type type)
           byte-list)))

(defconstant +utf8+ 3
  "Major type 3: a text string, specifically a string of Unicode characters that
is encoded as UTF-8 [RFC3629]. The format of this type is identical to that of
byte strings (major type 2), that is, as with major type 2, the length gives the
number of bytes.")

(defun encode-utf8 (string)
  (encode-bytes (string-to-utf-8-bytes string)
                :type +utf8+))

(defconstant +array+ 4
  "Major type 4:  an array of data items.  Arrays are also called lists,
sequences, or tuples. The array's length follows the rules for byte
strings (major type 2), except that the length denotes the number of data items,
not the length in bytes that the array takes up.")

(defun encode-array (array)
  (append (encode-uint (length array) :type +array+)
          (mappend #'encode array)))

(defconstant +dict+ 5
  "Major type 5: a map of pairs of data items. Maps are also called tables,
dictionaries, hashes, or objects (in JSON). A map is comprised of pairs of data
items, each pair consisting of a key that is immediately followed by a value.
The map's length follows the rules for byte strings (major type 2), except that
the length denotes the number of pairs, not the length in bytes that the map
takes up.")

(defun encode-dict (dict)
  (append (encode-uint (hash-table-count dict) :type +dict+)
          (loop for k being the hash-keys of dict
                  using (hash-value v)
                append (append (encode k) (encode v)))))

(defconstant +tags+ 6
  "Major type 6:  optional semantic tagging of other major types.")

(defconstant +simple+ 7
  "Major type 7: floating-point numbers and simple data types that need no
content, as well as the \"break\" stop code.")
(defconstant +break+ (initial-byte 7 31))

(declaim (inline encode-float16 decode-float16))
(make-float-converters encode-float16 decode-float16 5 10 nil)

(defun encode-float (f)
  "Encodes a single or double float. We do not encode half floats,
since Common Lisp has no notion of them."
  (multiple-value-bind (encoder addl-info)
      (etypecase f
        (single-float (values #'encode-float32 26))
        (double-float (values #'encode-float64 27)))
    (cons (initial-byte +simple+ addl-info)
          (int->bytes (funcall encoder f)))))
