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




(defvar *register-allocation-optimization* t)
(defvar *common-subexpression-elimination* t)

(defstruct op
  name
  inputs
  output)

(defvar *ops* nil)
(defvar *first-variable* nil)
(defun compile-bitwise-operations (form)
  (let ((*ops* nil)
        (*first-variable* nil))
    (let ((result-symbol (parse-form form)))
      (setf *ops* (nreverse *ops*))     ;in order
      (when *register-allocation-optimization*
        (reduce-allocation))
      (build-forms result-symbol))))

(defun reduce-allocation ()
  (tagbody
    :start
    (print *ops*)
    (iter (for op1 in *ops*)
          (ematch op1
            ((op :name n1 :inputs i1 :output o1)
             (let ((successors
                    (iter (for op2 in *ops*)
                          (when (and (member o1 (op-inputs op2))
                                     (not (eq o1 (op-output op2)))
                                     (not (member (op-output op2) (op-inputs op2))))
                            (collect op2)))))
               (match successors
                 ((list (and op2 (op :name n2 :inputs i2 :output o2)))
                  ;; only 1 op depends on op; share storage
                  (print `(:removing ,o1))
                  (setf *ops* (substitute (op n1 i1 o2) op1 *ops*))
                  (setf *ops* (substitute (op n2 (substitute o2 o1 i2) o2)
                                          op2 *ops*))
                  (go :start))))))
          (finally
           (return-from reduce-allocation)))))

(defun build-forms (result-symbol)
  (with-gensyms (len)
    `(let* ((,len (length ,*first-variable*))
            (,result-symbol (make-bit-vector ,len)))
       (dlet* ((+zero+ (make-bit-vector ,len))
               (+one+  (make-bit-vector ,len))
               ,@(mapcar (lambda (out)
                           `(,out (make-bit-vector ,len)))
                         (remove-duplicates
                          (remove result-symbol
                                  (mapcar #'op-output *ops*)))))
         (declare (ignorable +zero+ +one+))
         ,@(mapcar (lambda-ematch
                     ((op name inputs output)
                      `(,name ,@inputs ,output))) *ops*)
         ,result-symbol))))

(defun-match* common-subexpression (op1 op2)
  (((op name inputs)
    (op :name (eq name) :inputs (equal inputs)))
   t))

(defun accumulate (op)
  ;; it is not ok to remove-duplicate in post-processing since
  ;; subsequent call to the same expression should be rewritten.
  (if-let ((op2 (and *common-subexpression-elimination*
                     (find op *ops* :test #'common-subexpression))))
    (op-output op2)
    (progn
      (push op *ops*)
      (op-output op))))

(defun parse-form (form)
  (ematch form
    (0 '+zero+)
    (1 '+one+)
    ((symbol)
     (setf *first-variable* form))
    ((list 'not arg)
     (accumulate (op 'bit-not (list (parse-form arg)) (gensym))))
    ((list (and binary-op
                (or 'andc1 'andc2 'orc1 'orc2))
           arg1 arg2)
     (accumulate (op (symbolicate 'bit- binary-op)
                     (list (parse-form arg1)
                           (parse-form arg2)) (gensym))))
    ((list (or 'and 'eqv 'ior 'nand 'nor 'xor) arg)
     (parse-form arg))
    ((list* (and commutative-op
                 (or 'and 'eqv 'ior 'nand 'nor 'xor))
            (list* (eq commutative-op) sub-args)
            args)
     ;; commutative fusion
     (parse-form `(,commutative-op ,@sub-args ,@args)))
    ((list* (and commutative-op
                 (or 'and 'eqv 'ior 'nand 'nor 'xor))
            arg args)
     (accumulate (op (symbolicate 'bit- commutative-op)
                     (list (parse-form arg)
                           (parse-form `(,commutative-op ,@args))) (gensym))))
    ((list* (and macro-op (symbol)) args)
     (parse-form
      (funcall (symbol-bitwise-operation macro-op)
               args)))))


#+nil
(as-bitwise-operations ()
  (and a b))

#+nil
(as-bitwise-operations ()
  (and a b c))

#+nil
(as-bitwise-operations ()
  (and (and a b)
       (and a b)
       c))

#+nil
(as-bitwise-operations ()
  (and (and a b)
       (and c d)))

;; 
;; ->
;; (ftype fn simple-bit-vector simple-bit-vector simple-bit-vector simple-bit-vector)
;; (defun fn (a b c)
;;   (declare (optimize (speed 2) (safety 0)))
;;   (dlet ((tmp (make-bit-vector (length a))))
;;     (bit-and a b tmp)
;;     (bit-and tmp c)))

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
