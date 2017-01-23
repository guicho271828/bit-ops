
(in-package :bit-ops)


(define-bitwise-operation if (condition then else)
  `(ior (and ,condition ,then)
        (andc1 ,condition ,else)))

(define-bitwise-operation implies (lhs rhs)
  `(ior (not ,lhs)
        ,rhs))


(define-bitwise-operation half-adder-sum (a b)
  `(xor ,a ,b))
(define-bitwise-operation half-adder-carry (a b)
  `(and ,a ,b))

(define-bitwise-operation full-adder-sum (a b x)
  `(half-adder-sum (half-adder-sum ,a ,b)
                   ,x))

(define-bitwise-operation full-adder-carry (a b x)
  `(ior (half-adder-carry ,a ,b)
        (half-adder-carry (half-adder-sum ,a ,b)
                          ,x)))

#+nil
(as-bitwise-operations ()
  (full-adder-carry a b x))

;; multi-digit adder...

;; carry lookahead ...

;; subtraction...
