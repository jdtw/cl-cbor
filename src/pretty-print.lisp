;;;; pretty-print.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *print-jump*
  (make-array 256 :element-type 'function
                  :initial-element (lambda (stream indent)
                                     (declare (ignore stream indent))
                                     (error "Invalid data type"))))

(defun pprint-cbor (stream)
  (print-jump stream 0)
  (values))

(defun pprint-cbor-sequence (sequence)
  (with-input-from-sequence (stream sequence :element-type '(unsigned-byte 8))
    (pprint-cbor stream)
    (values)))

(defun print-jump (stream indent)
  (funcall (aref *print-jump* (read-byte stream)) stream indent))

(defmacro type-name (byte)
  (case (major byte)
    (0 "uint")
    (1 "neg-int")
    (2 "bytes")
    (3 "utf8")
    (4 "array")
    (5 "map")
    (6 "tag")
    (7 "simple")))

(defmacro print-cbor-padding* (indent)
  `(write-string (loop repeat ,indent
                       append (list #\space #\space) into pad
                       finally (return (coerce pad 'string)))))

(defmacro print-cbor-uint* (byte stream indent)
  `(lambda (transformer)
     (let ((uint (read-cbor-uint* ,byte ,stream)))
       (print-cbor-padding* ,indent)
       (format t "~2,'0x" ,byte)
       ,(when (> (info byte) 23)
          `(format t " ~v,'0x"
                   (* 2 (pad-count uint)) uint))
       (format t " # ~a(~a)~%"
               (type-name ,byte)
               (funcall transformer uint))
       uint)))

(defmacro print-cbor-bytes* (count stream indent)
  `(let* ((bytes (read-cbor-bytes* ,count ,stream)))
     (print-cbor-padding* ,indent)
     (format t "  ~{~2,'0x~}~%" (coerce bytes 'list))))

(defmacro pprint-loop* (byte stream indent &rest rest)
  (with-gensyms (decoded halt)
    (cond
      ((= (info byte) 0)
       `(funcall (print-cbor-uint* ,byte ,stream ,indent) #'identity))
      ((= (info byte) 31)
       `(let ((,halt (gensym)))
          (progn
            (print-cbor-padding* ,indent)
            (format t "~2,'0x #~a(_)~%" ,byte (type-name ,byte))
            (loop for ,decoded = (handler-case (print-jump ,stream (1+ ,indent))
                                   (unexpected-break () ,halt))
                  until (eq ,decoded ,halt)
                  ,@rest)
            (format t "FF  #break~%"))))
      (t `(loop repeat (funcall (print-cbor-uint* ,byte ,stream ,indent) #'identity)
                for ,decoded = (print-jump ,stream (1+ ,indent))
                ,@rest)))))

(defmacro print-jump-lambda (byte &body body)
  (with-gensyms (stream indent)
    `(lambda (,stream ,indent)
       (labels ((print-cbor-padding () (print-cbor-padding* ,indent))
                (print-cbor-uint (&optional (transformer #'identity))
                  (funcall (print-cbor-uint* ,byte ,stream ,indent) transformer))
                (print-cbor-bytes ()
                  (print-cbor-bytes* (print-cbor-uint) ,stream ,indent))
                (pprint-cbor () (print-jump ,stream (1+ ,indent))))
         (declare (ignorable (function print-cbor-padding)
                             (function print-cbor-bytes)
                             (function pprint-cbor)))
         (macrolet ((pprint-loop (&rest rest)
                      `(pprint-loop* ,',byte ,',stream ,',indent ,@,'rest)))
           ,@body)))))

(defmacro def-print-jump (pos &body body)
  `(progn ,@(loop for b in (jump-range pos)
                  collect `(setf (aref *print-jump* ,b)
                                 (print-jump-lambda ,b ,@body)))))


;; Unsigned integers
(def-print-jump ((#x00 #x1b)) (print-cbor-uint))

;; Negative ints
(def-print-jump ((#x20 #x3b)) (print-cbor-uint (lambda (uint) (- -1 uint))))

;; Byte strings and utf8 strins are printed as bytes
(def-print-jump ((#x40 #x5b) (#x60 #x7b)) (print-cbor-bytes))
(def-print-jump (#x5f #x7f) (pprint-loop))

;; Arrays
(def-print-jump ((#x80 #x9b) #x9f) (pprint-loop))

;; Hash tables
(def-print-jump ((#xa0 #xbb) #xbf) (pprint-loop do (pprint-cbor)))

;; Tags
(def-print-jump ((#xc0 #xdb)) (print-cbor-uint) (pprint-cbor))

;; Simple values
(def-print-jump ((#xe0 #xf3) #xf8) (print-cbor-uint))
(def-print-jump (#xf4) (print-cbor-uint (lambda (n) (declare (ignore n)) "false")))
(def-print-jump (#xf5) (print-cbor-uint (lambda (n) (declare (ignore n)) "true")))
(def-print-jump (#xf6) (print-cbor-uint (lambda (n) (declare (ignore n)) "null")))
(def-print-jump (#xf7) (print-cbor-uint (lambda (n) (declare (ignore n)) "undef")))
(def-print-jump (#xf9) (print-cbor-uint #'decode-float16))
(def-print-jump (#xfa) (print-cbor-uint #'decode-float32))
(def-print-jump (#xfb) (print-cbor-uint #'decode-float64))

;; Break
(def-print-jump (#xff) (error 'unexpected-break))
