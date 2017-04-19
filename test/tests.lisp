;;;; tests.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor-test
  (:use #:cl
        #:prove))

(in-package #:cl-cbor-test)

(plan 2)

(subtest "Test int->bytes"
  (is '(0) (cbor:int->bytes 0) "0x0 -> 0x0 ")
  (is '(1) (cbor:int->bytes 1) "0x1 -> 0x1 ")
  (is '(255) (cbor:int->bytes 255) "0xFF -> 0xFF ")
  (is '(1 0) (cbor:int->bytes 256) "0x100 -> 0x1 0x0 ")
  (is '(171 205) (cbor:int->bytes 43981) "0xABCD -> 0xAB 0xCD ")
  (is '(0 1 0 0) (cbor:int->bytes 65536) "0x10000 -> 0x0 0x1 0x0 0x0 ")
  (is '(255 255 255 255) (cbor:int->bytes 4294967295)
      "0xFFFFFFFF -> 0xFF 0xFF 0xFF 0xFF ")
  (is '(0 0 0 1 0 0 0 0) (cbor:int->bytes 4294967296)
      "0x100000000 -> 0x0 0x0 0x0 0x1 0x0 0x0 0x0 0x0 ")
  (is '(0 0 1 0 0 0 0 0) (cbor:int->bytes 1099511627776)
      "0x10000000000 -> 0x0 0x0 0x1 0x0 0x0 0x0 0x0 0x0 ")
  (is '(0 1 0 0 0 0 0 0) (cbor:int->bytes 281474976710656)
      "0x1000000000000 -> 0x0 0x1 0x0 0x0 0x0 0x0 0x0 0x0 ")
  (is '(255 255 255 255 255 255 255 255)
      (cbor:int->bytes 18446744073709551615)
      "0xFFFFFFFFFFFFFFFF -> 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF ")
  (is-error (cbor:int->bytes (expt 2 64)) 'type-error "2^64 is too big")
  (is-error (cbor:int->bytes 'not-an-int) 'type-error "symbol is not an int"))

(subtest "Test bytes->int"
  (is 0 (cbor:bytes->int (cbor:int->bytes 0)) "0x0")
  (is 1 (cbor:bytes->int (cbor:int->bytes 1)) "0x1")
  (is 256 (cbor:bytes->int (cbor:int->bytes 256)) "0x100")
  (is 65536 (cbor:bytes->int (cbor:int->bytes 65536)) "0x10000")
  (is 16777216 (cbor:bytes->int (cbor:int->bytes 16777216)) "0x1000000")
  (is 4294967296 (cbor:bytes->int (cbor:int->bytes 4294967296))
      "0x100000000")
  (is 1099511627776 (cbor:bytes->int (cbor:int->bytes 1099511627776))
      "0x10000000000")
  (is 281474976710656 (cbor:bytes->int (cbor:int->bytes 281474976710656))
      "0x1000000000000")
  (is 72057594037927936
      (cbor:bytes->int (cbor:int->bytes 72057594037927936))
      "0x100000000000000"))

(finalize)
