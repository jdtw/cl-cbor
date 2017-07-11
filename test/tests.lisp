;;;; tests.lisp

;;;; Copyright (c) 2017, John Wood <j@jdtw.us>
;;;; See LICENSE for additional information.

(defpackage #:cl-cbor-test
  (:use #:cl
        #:prove
        #:alexandria
        #:local-time))

(in-package #:cl-cbor-test)

(setf *enable-colors* nil)
(setf *default-test-function* #'equalp)

(plan nil)

(defmacro is-encode-decode (thing)
  (with-gensyms (test)
    `(let ((,test ,thing))
       (is (cbor:decode-sequence
            (cbor:encode-to-sequence ,test))
           ,test))))

(defmacro is-encode-decode* (enc dec)
  (with-gensyms (e d)
    `(let ((,e ,enc) (,d ,dec))
       (is (cbor:decode-sequence
            (cbor:encode-to-sequence ,e))
           ,d))))

(defmacro is-tagged-item (tag item)
  (with-gensyms (enc dec)
    `(let* ((,enc (cbor:make-tagged-item ,tag ,item))
            (,dec (cbor:decode-sequence
                   (cbor:encode-to-sequence
                    ,enc))))
       (is (cbor:item-tag ,enc) (cbor:item-tag ,dec))
       (is (cbor:item ,enc) (cbor:item ,dec)))))

(subtest "Testing integers"
  (is-encode-decode 0)
  (is-encode-decode 1)
  (is-encode-decode 23)
  (is-encode-decode 24)
  (is-encode-decode #xffff)
  (is-encode-decode #x10000)
  (is-encode-decode #xffffff)
  (is-encode-decode #xffffffff)
  (is-encode-decode #xffffffffffffffff)
  (is-encode-decode -1)
  (is-encode-decode -23)
  (is-encode-decode -24)
  (is-encode-decode #x-ffff)
  (is-encode-decode #x-10000)
  (is-encode-decode #x-ffffff)
  (is-encode-decode #x-ffffffff)
  (is-encode-decode #x-ffffffffffffffff)
  (is-encode-decode most-positive-fixnum)
  (is-encode-decode most-negative-fixnum))

(subtest "Testing floats"
  (is-encode-decode 0.0)
  (is-encode-decode -0.0)
  (is-encode-decode 1.1)
  (is-encode-decode 1.5)
  (is-encode-decode -1.5)
  (is-encode-decode 1.2345678)
  (is-encode-decode pi)
  (is-encode-decode (- pi))
  (is-encode-decode most-positive-short-float)
  (is-encode-decode most-negative-short-float)
  (is-encode-decode most-positive-single-float)
  (is-encode-decode most-negative-single-float)
  (is-encode-decode most-positive-long-float)
  (is-encode-decode most-negative-long-float)
  (is-encode-decode most-positive-double-float)
  (is-encode-decode most-negative-double-float))

(subtest "Testing byte strings"
  (is-encode-decode #())
  (is-encode-decode #(1 2 3 4 5))
  (is-encode-decode (make-array #x100 :element-type '(unsigned-byte 8)
                                      :initial-element 1))
  (is-encode-decode (make-array #x10000 :element-type '(unsigned-byte 8)
                                        :initial-element #xff)))

(subtest "Testing strings"
  (is-encode-decode "﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥")
  (is-encode-decode "᚛᚛ᚉᚑᚅᚔᚉᚉᚔᚋ ᚔᚈᚔ ᚍᚂᚐᚅᚑ ᚅᚔᚋᚌᚓᚅᚐ᚜")
  (is-encode-decode "Ic mæg glæs eotan ond hit ne hearmiað me.")
  (is-encode-decode "⠊⠀⠉⠁⠝⠀⠑⠁⠞⠀⠛⠇⠁⠎⠎⠀⠁⠝⠙⠀⠊⠞⠀⠙⠕⠑⠎⠝⠞⠀⠓⠥⠗⠞⠀⠍⠑")
  (is-encode-decode "ᛖᚴ ᚷᛖᛏ ᛖᛏᛁ ᚧ ᚷᛚᛖᚱ ᛘᚾ ᚦᛖᛋᛋ ᚨᚧ ᚡᛖ ᚱᚧᚨ ᛋᚨᚱ")
  (is-encode-decode "Mogę jeść szkło i mi nie szkodzi.")
  (is-encode-decode "Мога да ям стъкло, то не ми вреди.")
  (is-encode-decode "Կրնամ ապակի ուտել և ինծի անհանգիստ չըներ։")
  (is-encode-decode "আমি কাঁচ খেতে পারি, তাতে আমার কোনো ক্ষতি হয় না।")
  (is-encode-decode "मैं काँच खा सकता हूँ और मुझे उससे कोई चोट नहीं पहुंचती.")
  (is-encode-decode "මට වීදුරු කෑමට හැකියි. එයින් මට කිසි හානියක් සිදු නොවේ.")
  (is-encode-decode "איך קען עסן גלאָז און עס טוט מיר נישט װײ.")
  (is-encode-decode "က္ယ္ဝန္‌တော္‌၊က္ယ္ဝန္‌မ မ္ယက္‌စားနုိင္‌သည္‌။ ၎က္ရောင္‌့ ထိခုိက္‌မ္ဟု မရ္ဟိပာ။")
  (is-encode-decode "ຂອ້ຍກິນແກ້ວໄດ້ໂດຍທີ່ມັນບໍ່ໄດ້ເຮັດໃຫ້ຂອ້ຍເຈັບ.")
  (is-encode-decode "Би шил идэй чадна, надад хортой биш")
  (is-encode-decode "ཤེལ་སྒོ་ཟ་ནས་ང་ན་གི་མ་རེད།")
  (is-encode-decode "私はガラスを食べられます。それは私を傷つけません。")
  (is-encode-decode "ᐊᓕᒍᖅ ᓂᕆᔭᕌᖓᒃᑯ ᓱᕋᙱᑦᑐᓐᓇᖅᑐᖓ"))

(subtest "Testing arrays"
  (let ((cbor:*encode-nil-as* 'list))
    (is-encode-decode '())
    (is-encode-decode '(((()))))
    (is-encode-decode '(1 2 3 4 5))
    (is-encode-decode '((1 ()) "abcd" (-1 1.1)))))

(subtest "Test indefinite length"
  (is (cbor:decode-sequence
       (cbor:with-output-to-sequence (stream)
         (cbor:with-array (stream)
           (cbor:with-utf8 (stream)
             (cbor:encode-utf8 "foo")
             (cbor:encode-utf8 "bar"))
           (cbor:with-array (stream))
           (cbor:with-bytes (stream)
             (cbor:encode-bytes #(1 2 3))
             (cbor:encode-bytes #())
             (cbor:encode-bytes #(4 5 6))))))
      '("foobar" nil #(1 2 3 4 5 6))))

(subtest "Test unknown tags"
  (is-tagged-item 6 "foo bar baz")
  (is-tagged-item 31 (list 1 nil 3 #(1 2 3) 5))
  (is-tagged-item #xdeadbeef #(1 2 3 4 5))
  (let* ((tag 1234) (item #xffffffff)
         (encoded (cbor:with-output-to-sequence (stream)
                    (cbor:encode-tagged tag item stream)))
         (decoded (cbor:decode-sequence encoded)))
    (is tag (cbor:item-tag decoded))
    (is item (cbor:item decoded))
    ;; When we ignore tags and decode, the tag should be stripped.
    (let ((cbor:*ignore-tags* t))
      (setf decoded (cbor:decode-sequence encoded)))
    (is item decoded)))

(subtest "Test timestamps"
  (let ((*default-test-function* #'local-time:timestamp=))
    (is-encode-decode (now))
    (is-encode-decode* (cbor:make-tagged-item cbor:+time+ "2013-03-21T20:04:00Z")
                       (parse-timestring "2013-03-21T20:04:00Z"))
    (is-encode-decode* (cbor:make-tagged-item cbor:+epoch+ 0)
                       (unix-to-timestamp 0))
    (is-encode-decode* (cbor:make-tagged-item cbor:+epoch+ 1.1d0)
                       (unix-to-timestamp cbor:+epoch+ :nsec 100000000))
    (is-encode-decode* (cbor:make-tagged-item cbor:+epoch+ -1.1d0)
                       (unix-to-timestamp -2 :nsec 900000000))))

(subtest "Test self-describe"
  (is-encode-decode*
   (cbor:make-tagged-item
    cbor:+self-describe-cbor+
    (cbor:make-tagged-item
     cbor:+self-describe-cbor+
     (cbor:make-tagged-item
      cbor:+self-describe-cbor+
      (cbor:make-tagged-item
       cbor:+self-describe-cbor+
       cbor:+self-describe-cbor+))))
   cbor:+self-describe-cbor+))

(subtest "Test bignums"
  (is-encode-decode #xbaadf00ddeadb33ffacefeed)
  (is-encode-decode #x-baadf00ddeadb33ffacefeed)
  (is-encode-decode #xffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
  (is-encode-decode #x-8000000000000000000000000000000000000000000000000000000000))

(finalize)
