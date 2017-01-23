
(in-package :bit-ops)


(define-bitwise-operation if (condition then else)
  `(ior (and ,condition ,then)
        (andc1 ,condition ,else)))

(define-bitwise-operation implies (lhs rhs)
  `(ior (not ,lhs)
        ,rhs))
