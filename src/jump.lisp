;;;; jump.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *jump*
  (make-array 256 :element-type 'function
                  :initial-element (lambda (stream)
                                     (declare (ignore stream))
                                     (error "Invalid data type"))))
(defun jump (stream)
  (funcall (aref *jump* (read-byte stream)) stream))

(defmacro read-cbor-uint* (byte stream)
  (case (info byte)
    (24 `(read-byte ,stream))
    (25 `(read-ub16/be ,stream))
    (26 `(read-ub32/be ,stream))
    (27 `(read-ub64/be ,stream))
    (otherwise (info byte))))

(defmacro read-cbor-bytes* (count stream)
  `(let* ((count ,count)
          (bytes (make-array count :element-type '(unsigned-byte 8)))
          (read (read-sequence bytes ,stream)))
     (unless (= read count) (error 'end-of-file))
     bytes))

(defmacro decode-loop* (byte stream &rest rest)
  (cond
    ((= (info byte) 0) 'nil)
    ((= (info byte) 31)
     (with-gensyms (halt)
       `(let ((,halt (gensym)))
          (loop for decoded = (handler-case (jump ,stream)
                                (unexpected-break () ,halt))
                until (eq decoded ,halt)
                ,@rest))))
    (t `(loop repeat (read-cbor-uint* ,byte ,stream)
              for decoded = (jump ,stream)
              ,@rest))))

(defmacro jump-lambda (byte &body body)
  "Each entry in the jump list is a lambda, which can be defined by the
caller with ~defjump~. Several functions are provided to the caller:
- read-cbor-uint :: Reads an unsigned integer from the stream, as defined
      by the CBOR spec.
- read-cbor-bytes :: Uses as a call to read-cbor-uint as the length, and
      then reads that many bytes into an array.
- decode :: Jumps to the next entry in the jump list for decoding, and
      returns the common lisp object that was decoded.
- decode-loop :: Either loops a number of times defined by read-cbor-uint,
      or, for indefinite items, loops until the #xff stop code is reached."
  (with-gensyms (stream)
    `(lambda (,stream)
       (labels ((read-cbor-uint ()
                  (read-cbor-uint* ,byte ,stream))
                (read-cbor-bytes ()
                  (read-cbor-bytes* (read-cbor-uint* ,byte ,stream) ,stream))
                (decode ()
                  (jump ,stream)))
         (declare (ignorable (function read-cbor-uint)
                             (function read-cbor-bytes)
                             (function decode)))
         (macrolet ((decode-loop (&rest rest)
                      `(decode-loop* ,',byte ,',stream ,@,'rest)))
           ,@body)))))

(defun jump-range (spec)
  (loop with bytes = nil
        for b in spec
        do (if (atom b)
               (push b bytes)
               (loop for b from (first b) upto (second b)
                     do (push b bytes)))
        finally (return (reverse bytes))))

(defmacro defjump (pos &body body)
  `(progn ,@(loop for b in (jump-range pos)
                  collect `(setf (aref *jump* ,b)
                                 (jump-lambda ,b ,@body)))))
