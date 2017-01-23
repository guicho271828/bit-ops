
(in-package :bit-ops)


(define-bitwise-operation if (condition then else)
  `(ior (and ,condition ,then)
        (andc1 ,condition ,else)))

(declaim (inline bit-if-then-else))
(ftype bit-if-then-else bit-vector bit-vector bit-vector &optional bit-vector bit-vector)
(defun bit-if-then-else (condition then else
                         &optional (res (make-bit-vector (length condition))))
  "if A_i=1 then B_i else C_i"
  (declare (optimize (speed 2) (safety 0)))
  (as-bitwise-operations (:result res)
    (if condition then else)))

#+hand-written
(dlet ((tmp (make-bit-vector (length condition))))
  (bit-and then condition tmp)
  (bit-andc2 else condition res)
  (bit-ior tmp res res)
  res)


(assert (equal (bit-if-then-else #*00001111
                                 #*00110011
                                 #*01010101)
               #*01010011))


(define-bitwise-operation implies (lhs rhs)
  `(ior (not ,lhs)
        ,rhs))



(declaim (inline bit-implies))
(ftype bit-implies bit-vector bit-vector &optional bit-vector bit-vector)
(defun bit-implies (bv1 bv2 &optional (bv3 (make-array (length bv1) :element-type 'bit)))
  "a => b :- not a or b"
  (declare (optimize (speed 2) (safety 0)))
  (as-bitwise-operations (:result bv3)
    (implies bv1 bv2)))

#+hand-written
(progn
  (bit-not bv1 bv3)
  (bit-ior bv2 bv3 bv3)
  bv3)
