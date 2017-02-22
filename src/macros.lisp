(in-package :bit-ops)


(define-bitwise-operation nand (&rest args)
  "This is a commutative version of NAND,
in contrast to 2-arg NAND compiled into primitive BIT-NAND.
Take the conjunction of arguments and invert the results."
  `(not (and ,@args)))

(define-bitwise-operation nor (&rest args)
  "This is a commutative version of NOR,
in contrast to 2-arg NOR compiled into primitive BIT-NOR.
Take the disjunction of arguments and invert the results."
  `(not (and ,@args)))


(define-bitwise-operation if (condition then else)
  "Take THEN bit when CONDITION bit is true, take ELSE otherwise."
  `(ior (and ,condition ,then)
        (andc1 ,condition ,else)))

(define-bitwise-operation implies (lhs rhs)
  "LHS => RHS :- (or (not LHS) RHS)"
  `(ior (not ,lhs)
        ,rhs))


(define-bitwise-operation half-adder-sum (a b)
  "sum output of half adder "
  `(xor ,a ,b))
(define-bitwise-operation half-adder-carry (a b)
  "carry output of half adder "
  `(and ,a ,b))

(define-bitwise-operation full-adder-sum (a b x)
  "sum output of full adder "
  `(half-adder-sum (half-adder-sum ,a ,b)
                   ,x))

(define-bitwise-operation full-adder-carry (a b x)
  "carry output of full adder "
  `(ior (half-adder-carry ,a ,b)
        (half-adder-carry (half-adder-sum ,a ,b)
                          ,x)))

#+nil
(as-bitwise-operations ()
  (full-adder-carry a b x))


#+nil
(as-bitwise-operations ()
  (full-adder-carry (subseq a 25) b x))

;; (define-bitwise-operation adder (a b prefetch)
;;   "Perform full addition of a and b. The end carry bit is ignored.
;; PREFETCH specifies how many intermediate carry out bits to prefetch.
;; "


;; multi-digit adder...

;; carry lookahead ...

;; subtraction...
