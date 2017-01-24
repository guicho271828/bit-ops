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
   #:dlet*
   #:bit-if-then-else
   #:bit-implies
   #:implies
   #:bitwise-operation
   #:define-bitwise-operation
   #:as-bitwise-operations)
  (:shadowing-import-from :immutable-struct :ftype))
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

(declaim (inline make-zero))
(ftype make-zero unsigned-byte simple-bit-vector)
(defun make-zero (length)
  (make-array length :element-type 'bit :initial-element 0))

(declaim (inline make-one))
(ftype make-one unsigned-byte simple-bit-vector)
(defun make-one (length)
  (make-array length :element-type 'bit :initial-element 1))


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

(declaim (inline op))
(defun op (name inputs output)
  (make-op :name name :inputs inputs :output output))

(defvar *ops* nil)
(defvar *first-variable* nil)
(defvar *result-variable* nil)
(defun compile-bitwise-operations (form result)
  (let ((*ops* nil)
        (*first-variable* nil))
    (let ((*result-variable* (parse-form form)))
      (setf *ops* (nreverse *ops*))     ;in order
      (when *register-allocation-optimization*
        (reduce-allocation))
      (build-forms result))))

(defun reduce-allocation ()
  (iter (for op1 in *ops*)
        (print *ops*)
        (ematch op1
          ((op :inputs (place i1) :output (place o1))
           (let ((successors
                  (iter (for op2 in *ops*)
                        (when (member o1 (op-inputs op2))
                          (collect op2)))))
             (match successors
               ((list (op :inputs (place i2) :output (place o2 o2-orig)))
                ;; only 1 op depends on op; share storage
                (unless (member o2 i2) ; check if already reduced
                  (print `(:removing ,o2))
                  (setf o2 o1)
                  (iter (for op3 in *ops*)
                        (ematch op3
                          ((op :inputs (place i3))
                           (setf i3 (substitute o1 o2-orig i3)))))
                  (when (eq *result-variable* o2-orig)
                    (setf *result-variable* o1))))))))))

(defun build-forms (result)
  (with-gensyms (len)
    `(let* ((,len (length ,*first-variable*))
            (,*result-variable* ,(or result `(make-bit-vector ,len))))
       (dlet* ((+zero+ (make-zero ,len))
               (+one+  (make-one  ,len))
               ,@(mapcar (lambda (out)
                           `(,out (make-bit-vector ,len)))
                         (remove-duplicates
                          (remove *result-variable*
                                  (mapcar #'op-output *ops*)))))
         (declare (ignorable +zero+ +one+))
         ,@(mapcar (lambda-ematch
                     ((op name inputs output)
                      `(,name ,@inputs ,output))) *ops*)
         ,*result-variable*))))

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
                (or 'nand 'nor 'andc1 'andc2 'orc1 'orc2))
           arg1 arg2)
     (accumulate (op (symbolicate 'bit- binary-op)
                     (list (parse-form arg1)
                           (parse-form arg2)) (gensym))))
    ((list (or 'and 'eqv 'ior 'xor) arg)
     (parse-form arg))
    ((list* (and commutative-op
                 (or 'and 'eqv 'ior 'xor))
            (list* (eq commutative-op) sub-args)
            args)
     ;; commutative fusion
     (parse-form `(,commutative-op ,@sub-args ,@args)))
    ((list* (and commutative-op
                 (or 'and 'eqv 'ior 'xor))
            arg args)
     (accumulate (op (symbolicate 'bit- commutative-op)
                     (list (parse-form arg)
                           (parse-form `(,commutative-op ,@args))) (gensym))))
    ((list* (and macro-op (symbol)) args)
     (parse-form
      (apply (symbol-bitwise-operation macro-op)
             args)))))


#+nil
(as-bitwise-operations ()
  (and a b))

#+nil
(as-bitwise-operations ()
  (and a b c))

#+nil
(as-bitwise-operations ()
  (ior (and a b)
       (and a b)
       c))

#+nil
(as-bitwise-operations ()
  (and (and a b)
       (and c d)))

(defmacro as-bitwise-operations ((&key result) &body body)
  "
Compute bitwise operations using bit vector arithmetic.
BODY accepts a single form. Within BODY, one can use variables holding bit-vectors as arguments to
bitwise operations, as well as constants 0 and 1, which maps to the bit vector filled with
0 or 1, respectively.

Primitive operators corresponds to ANSI CL functions: For example, (not subform) is compiled
into (bit-not subform <temporary storage>) . Following operators are available:

  not and andc1 andc2 eqv ior nand nor orc1 orc2 xor

The final computation result is stored in a newly allocated vector, or in RESULT if specified,
in spirit similar to the optional argument of Common Lisp bit-vector functions.
The entire form returns the bit-vector which contains the result.

The compiler does various optimizations:

* Nested expressions store the results into dynamic-extent temporary vectors.
* Common subexpressions are eliminated.
* The number of temporary vectors are minimized/shared in spirit similar to register allocation.
* Macros for bitwise operations can be defined with DEFINE-BITWISE-OPERATION.
"
  (assert (= (length body) 1))
  (compile-bitwise-operations (first body) result))
