#|
  This file is a part of bit-ops project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage bit-ops
  (:use :cl :iterate :alexandria :trivia :lisp-namespace)
  (:export
   #:make-bit-vector
   #:dlet
   #:dlet*)
  (:shadowing-import-from :immutable-struct :ftype :defstruct))
(in-package :bit-ops)

;;; basic constructs.

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


;;; define macro bitwise operation.

(lispn:define-namespace bitwise-operation function nil
                        "Stores the macro definitions for compound bitwise operations")

(defmacro define-bitwise-operation (name lambda-list &body body)
  "Defines a bitwise operation that is available within AS-BITWISE-OPERATIONS macro.

Primitive operators corresponds to ANSI CL functions: For example, (not subform) is compiled
into (bit-not subform <temporary storage>) .

  not and andc1 andc2 eqv ior nand nor orc1 orc2 xor

"
  (match body
    ((or (list* (and doc (string*)) body)
         (and body (<> doc nil)))
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (symbol-bitwise-operation ',name)
              (lambda ,lambda-list ,@body))
        ,@(when doc
            ;; this code is borrowed from trivia
            ;; lisp-namespace
            `((setf (documentation ',name 'pattern)
                    ,(let ((*print-pretty* t))
                       #-clisp
                       (format nil "~<Lambda-List: ~s~
                                 ~:@_~<  ~@;~a~:>~
                                 ~:@_Defined in ~a~
                               ~:>"
                               (list args (list (first body)) *compile-file-pathname*))
                       #+clisp
                       (format nil "Lambda-List: ~s~%~a"
                               args (first body))))))))))




(defmacro as-bitwise-operations (() &body body)
  "Compute bitwise operations using bit vector arithmetic.
BODY accepts a single form.

The compiler handles various optimizations:

* Nested expressions store the results into dynamic-extent temporary vectors.
* Common subexpressions are eliminated.
* Macros for bitwise operations can be defined with DEFINE-BITWISE-OPERATION.

Primitive operators corresponds to ANSI CL functions: For example, (not subform) is compiled
into (bit-not subform <temporary storage>) .

  not and andc1 andc2 eqv ior nand nor orc1 orc2 xor

"
  (assert (= (length body) 1))
  (compile-bitwise-operations (first body)))

(defstruct op
  form
  storage)

(defvar *ops* nil)
(defvar *first-variable* nil)
(defun compile-bitwise-operations (form)
  (let ((*ops* nil)
        (*first-variable* nil))
    (let ((result-symbol (parse-form form)))
      (with-gensyms (len)
        `(let* ((,len (length ,*first-variable*))
                (,result-symbol (make-bit-vector ,len)))
           (dlet* ((+zero+ (make-bit-vector ,len))
                   (+one+  (make-bit-vector ,len))
                   ,@(mapcar (lambda (op)
                               `(,(op-storage op) (make-bit-vector ,len)))
                             (rest      ; since the first op should be the result-symbol
                              *ops*)))
             (declare (ignorable +zero+ +one+))
             ,@(mapcar (lambda-ematch
                         ((op form storage)
                          `(,@form ,storage))) (reverse *ops*))
             ,result-symbol))))))

(defun parse-form (form)
  (flet ((accumulate (op)
           (if-let ((op2 (find op *ops* :key #'op-form :test #'equalp)))
             (op-storage op2)
             (progn
               (push op *ops*)
               (op-storage op)))))
    (ematch form
      (0 '+zero+)
      (1 '+one+)
      ((symbol)
       (setf *first-variable* form))
      ((list 'not arg)
       (accumulate (op `(bit-not ,(parse-form arg)) (gensym))))
      ((list (and binary-op
                  (or 'andc1 'andc2 'orc1 'orc2))
             arg1 arg2)
       (accumulate (op `(,(symbolicate 'bit- binary-op)
                          ,(parse-form arg1)
                          ,(parse-form arg2)) (gensym))))
      ((list (and commutative-op
                  (or 'and 'eqv 'ior 'nand 'nor 'xor))
             arg)
       (parse-form arg))
      ((list* (and commutative-op
                   (or 'and 'eqv 'ior 'nand 'nor 'xor))
              arg args)
       (accumulate (op `(,(symbolicate 'bit- commutative-op)
                          ,(parse-form arg)
                          ,(parse-form `(,commutative-op ,@args))) (gensym))))
      ((list* (and macro-op (symbol)) args)
       (parse-form
        (funcall (symbol-bitwise-operation macro-op)
                 args))))))

(as-bitwise-operations ()
  (and a b))

(as-bitwise-operations ()
  (and a b c))


;; 
;; ->
;; (ftype fn simple-bit-vector simple-bit-vector simple-bit-vector simple-bit-vector)
;; (defun fn (a b c)
;;   (declare (optimize (speed 2) (safety 0)))
;;   (dlet ((tmp (make-bit-vector (length a))))
;;     (bit-and a b tmp)
;;     (bit-and tmp c)))
