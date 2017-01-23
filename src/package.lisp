#|
  This file is a part of bit-ops project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage bit-ops
  (:use :cl :iterate :alexandria :trivia)
  (:export
   #:make-bit-vector
   #:bit-if-then-else
   #:bit-implies
   #:dlet
   #:dlet*)
  (:shadowing-import-from :immutable-struct :ftype))
(in-package :bit-ops)

;; blah blah blah.

;; #+sbcl
;; sb-int:truly-dynamic-extent

(defmacro dlet (bindings &body body)
  `(let ,bindings
     (declare (dynamic-extent ,@(mapcar (compose #'first #'ensure-list) bindings)))
     ,@body))

(defmacro dlet* (bindings &body body)
  `(let* ,bindings
     (declare (dynamic-extent ,@(mapcar (compose #'first #'ensure-list) bindings)))
     ,@body))

(declaim (inline make-bit-vector))
(ftype make-bit-vector unsigned-byte simple-bit-vector)
(defun make-bit-vector (length)
  (make-array length :element-type 'bit))

(declaim (inline bit-if-then-else))
(ftype bit-if-then-else bit-vector bit-vector bit-vector &optional bit-vector bit-vector)
(defun bit-if-then-else (condition then else
                         &optional (res (make-bit-vector (length condition))))
  "if A_i=1 then B_i else C_i"
  (declare (optimize (speed 2) (safety 0)))
  (dlet ((tmp (make-bit-vector (length condition))))
    (bit-and then condition tmp)
    (bit-andc2 else condition res)
    (bit-ior tmp res res)
    res))

(assert (equal (bit-if-then-else #*00001111
                                 #*00110011
                                 #*01010101)
               #*01010011))


(declaim (inline bit-implies))
(ftype bit-implies bit-vector bit-vector &optional bit-vector bit-vector)
(defun bit-implies (bv1 bv2 &optional (bv3 (make-array (length bv1) :element-type 'bit)))
  "a => b :- not a or b"
  (bit-not bv1 bv3)
  (bit-ior bv2 bv3 bv3)
  bv3)



