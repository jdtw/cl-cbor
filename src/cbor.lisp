;;;; cbor.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defconstant +uint+ 0
  "Major type 0: an unsigned integer. The 5-bit additional information is either
the integer itself (for additional information values 0 through 23) or the
length of additional data. Additional information 24 means the value is
represented in an additional uint8_t, 25 means a uint16_t, 26 means a uint32_t,
and 27 means a uint64_t.")
(defconstant +ub8+ 24)
(defconstant +ub16+ 25)
(defconstant +ub32+ 26)
(defconstant +ub64+ 27)

(defconstant +neg-int+ 1
  "Major type 1: a negative integer. The encoding follows the rules for unsigned
integers (major type 0), except that the value is then -1 minus the encoded
unsigned integer.")

(defconstant +bytes+ 2
  "Major type 2: a byte string. The string's length in bytes is represented
following the rules for positive integers (major type 0).")

(defconstant +utf8+ 3
  "Major type 3: a text string, specifically a string of Unicode characters that
is encoded as UTF-8 [RFC3629]. The format of this type is identical to that of
byte strings (major type 2), that is, as with major type 2, the length gives the
number of bytes.")

(defconstant +array+ 4
  "Major type 4:  an array of data items.  Arrays are also called lists,
sequences, or tuples. The array's length follows the rules for byte
strings (major type 2), except that the length denotes the number of data items,
not the length in bytes that the array takes up.")

(defconstant +dict+ 5
  "Major type 5: a map of pairs of data items. Maps are also called tables,
dictionaries, hashes, or objects (in JSON). A map is comprised of pairs of data
items, each pair consisting of a key that is immediately followed by a value.
The map's length follows the rules for byte strings (major type 2), except that
the length denotes the number of pairs, not the length in bytes that the map
takes up.")

(defconstant +tags+ 6
  "Major type 6:  optional semantic tagging of other major types.")

(defconstant +simple+ 7
  "Major type 7: floating-point numbers and simple data types that need no
content, as well as the \"break\" stop code.")

(defconstant +uint+ 0
  "Major type 0: an unsigned integer. The 5-bit additional information is either
the integer itself (for additional information values 0 through 23) or the
length of additional data. Additional information 24 means the value is
represented in an additional uint8_t, 25 means a uint16_t, 26 means a uint32_t,
and 27 means a uint64_t.")

(defconstant +neg-int+ 1
  "Major type 1: a negative integer. The encoding follows the rules for unsigned
integers (major type 0), except that the value is then -1 minus the encoded
unsigned integer.")

(defconstant +bytes+ 2
  "Major type 2: a byte string. The string's length in bytes is represented
following the rules for positive integers (major type 0).")

(defconstant +utf8+ 3
  "Major type 3: a text string, specifically a string of Unicode characters that
is encoded as UTF-8 [RFC3629]. The format of this type is identical to that of
byte strings (major type 2), that is, as with major type 2, the length gives the
number of bytes.")

(defconstant +array+ 4
  "Major type 4:  an array of data items.  Arrays are also called lists,
sequences, or tuples. The array's length follows the rules for byte
strings (major type 2), except that the length denotes the number of data items,
not the length in bytes that the array takes up.")

(defconstant +dict+ 5
  "Major type 5: a map of pairs of data items. Maps are also called tables,
dictionaries, hashes, or objects (in JSON). A map is comprised of pairs of data
items, each pair consisting of a key that is immediately followed by a value.
The map's length follows the rules for byte strings (major type 2), except that
the length denotes the number of pairs, not the length in bytes that the map
takes up.")

(defconstant +tags+ 6
  "Major type 6:  optional semantic tagging of other major types.")

(defconstant +simple+ 7
  "Major type 7: floating-point numbers and simple data types that need no
content, as well as the \"break\" stop code.")

(defconstant +false+ 20)
(defconstant +true+ 21)
(defconstant +null+ 22)
(defconstant +break+ #xFF)

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
(defun write-initial-byte (hi lo stream)
  (write-byte (initial-byte hi lo) stream))

(defun info (b)
  "The initial byte of each data item contains both information about the major
type (the high-order 3 bits) and additional information (the low-order 5 bits)."
  (declare ((unsigned-byte 8) b))
  (ldb (byte 5 0) b))

(defun major (b)
  "The initial byte of each data item contains both information about the major
type (the high-order 3 bits) and additional information (the low-order 5 bits)."
  (declare ((unsigned-byte 8) b))
  (ldb (byte 3 5) b))

(defconstant +indefinite+ 31)
