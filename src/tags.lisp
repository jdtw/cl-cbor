;;;; tags.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(in-package #:cl-cbor)

(defparameter *tags-table* (make-hash-table)
  "Tag handlers. This library supports some tags, but the caller may add any
tags they wish to this table (or remove/redefine any of the default tag
handlers)")
(defun add-tag-decoder (tag decoder)
  (check-type tag (integer 0))
  (check-type decoder function)
  (setf (gethash tag *tags-table*) decoder))
(defun remove-tag-decoder (tag)
  (remhash tag *tags-table*))
(defmacro deftag ((tag thing) &body body)
  `(add-tag-decoder ,tag (lambda (,thing) ,@body)))

(defparameter *ignore-tags* nil
  "*IGNORE-TAGS* may be nil, t, or :UNKNOWN.
- nil :: Tags with handlers in the table will be decoded, and tags not in the
         table will be returned as instances of TAGGED-ITEM.
- t :: All tags are ignored, even tags with handlers in the table. The data items
       are returned as-is.
- :UNKNOWN :: Tags with handlers in the table are decoded. Unknown tags are ignored,
              and the data items are returned as-is.")
(defparameter *warn-unknown-tags* t
  "Issue a warning for tags not in the handler table.")

(defclass tagged-item ()
  ((tag :initarg :tag :reader item-tag)
   (item :initarg :item :reader item)))

(defun make-tagged-item (tag item)
  (check-type tag (integer 0))
  (make-instance 'tagged-item :tag tag :item item))

(defmethod print-object ((obj tagged-item) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A:~A" (item-tag obj) (item obj))))

(defun decode-tag (tag thing)
  (check-type tag (integer 0))
  (when (eq *ignore-tags* t)
    (return-from decode-tag thing))
  (multiple-value-bind (tagger present)
      (gethash tag *tags-table*)
    (if present
        (funcall tagger thing)
        (progn
          (when *warn-unknown-tags*
            (warn 'unknown-tag :tag tag :item thing))
          (ccase *ignore-tags*
            (:unknown thing)
            ((nil) (make-tagged-item tag thing)))))))
