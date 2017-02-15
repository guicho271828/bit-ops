
# Bit-Ops - Tools for Writing Optimized Bit-Vector Operations

[![Build Status](https://travis-ci.org/guicho271828/bit-ops.svg?branch=master)](https://travis-ci.org/guicho271828/bit-ops)

In the modern Common Lisp implementations, bit-vector operation functions such as `bit-and` are compiled into word-size iterations where 32 or 64 bits are processed at once. However, it requires a careful handling when multiple operations are combined, e.g. for avoiding the generation of intermediate vectors by destructively modifying the existing vectors. For example, 

```lisp
(bit-and a (bit-and b c))
```

causes consing since the intermediate value `(bit-and b c)` is generated on the heap. This library addresses this problem as well as the scarsity of useful bit-vector functions in ANSI CL.

## Related work

This section provides a review of related libraries, which I believe is important for
alleviating choise paralysis.

[BIT-SMASHER](http://quickdocs.org/bit-smasher/) provides functions for
converting bit-vector to/from integers, octets and hex strings. BIT-OPS does not
have such conversion functions. BIT-SMASHER also provides functions for
arithmetic, such as addition, subtraction, shifting. However, note that those
operations are not always optimized and runs bitvec->integer->bitvec conversion
each time, with possibly consing.

[BITFIELD-SCHEMA](http://quickdocs.org/bitfield-schema/) provides several
functions analogous to DPB and LPB for integers (GET/PUT-INTEGER). It also
provides a DSL for writing accessors to bit-vectors (ala union type in C).

Example from the doc: 
```lisp
(defbitfield-schema tree-node (:offset offt)
  (disabled-p   :width 1)
  (values       :width 16 :count 10)
  (left-child   :width 24)
  (right-child  :width 7))
```

[BINARY-TYPES](http://quickdocs.org/binary-types/) provides `DEFINE-BITFIELD` and
`DEFINE-BINARY-CLASS` whose role is similar to `BITFIELD-SCHEMA`, but is for 
parsing machine integers, not bit-vectors.
[TRIVIAL-BIT-STREAMS](http://quickdocs.org/trivial-bit-streams/)
provides a buffered stream of bits. [NIBBLES](http://quickdocs.org/nibbles/)
provides optimized access to octet vectors, especially on SBCL by defining
several SSE VOP operations.

## Usage

### Macro AS-BITWISE-OPERATIONS (&key result) &body body

Compute bitwise operations using bit vector arithmetic.
`BODY` accepts a single form.
Within `BODY`, one can use variables holding bit-vectors as arguments to
bitwise operations, as well as constants 0 and 1, which maps to the bit vector filled with
0 or 1, respectively.
All bit-vectors that are involved in bitwise operations should be of the same length.

Primitive operators corresponds to ANSI CL functions: For example, `(not subform)` is compiled
into `(bit-not subform <temporary storage>)` . Following primitive operators are available:

    not and andc1 andc2 eqv ior nand nor orc1 orc2 xor

Additionally, `(SUBSEQ FORM OFFSET)` operator evaluates FORM and
extracts its window starting from OFFSET and of the length equal to the other variables.
FORM is a regular lisp expression, not a bitwise operation, and the result may be different
from the other bit-vectors.

The final computation result is stored in a newly allocated vector, or in `RESULT` if specified,
in spirit similar to the optional argument of Common Lisp bit-vector functions.
The entire form returns the bit-vector which contains the result.

The compiler does various optimizations:

* Nested expressions store the results into dynamic-extent temporary vectors.
* Common subexpressions are eliminated.
* The number of temporary vectors are minimized/shared in spirit similar to register allocation.
* Macros for bitwise operations can be defined with `DEFINE-BITWISE-OPERATION`.


```lisp

(as-bitwise-operations ()
  (and a b c))

->

(LET* ((#:LEN835 (LENGTH C)) (#:G833 (MAKE-BIT-VECTOR #:LEN835)))
  (LET* ((+ZERO+ (MAKE-ZERO #:LEN835))
         (+ONE+ (MAKE-ONE #:LEN835)))
    (DECLARE (DYNAMIC-EXTENT +ZERO+ +ONE+))
    (DECLARE (IGNORABLE +ZERO+ +ONE+))
    (BIT-AND B C #:G833)
    (BIT-AND A #:G833 #:G833)
    #:G833))



(define-bitwise-operation if (condition then else)
  `(ior (and ,condition ,then)
        (andc1 ,condition ,else)))

(as-bitwise-operations (:result r)
  (if a b c))

->

(LET* ((#:LEN839 (LENGTH C)) (#:G836 R))
  (LET* ((+ZERO+ (MAKE-ZERO #:LEN839))
         (+ONE+ (MAKE-ONE #:LEN839))
         (#:G837 (MAKE-BIT-VECTOR #:LEN839)))
    (DECLARE (DYNAMIC-EXTENT +ZERO+ +ONE+ #:G837))
    (DECLARE (IGNORABLE +ZERO+ +ONE+))
    (BIT-AND A B #:G836)
    (BIT-ANDC1 A C #:G837)
    (BIT-IOR #:G836 #:G837 #:G836)
    #:G836))
```

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.3.13 on X86-64 Linux 4.4.0-59-generic (author's environment)
+ CCL 1.11 X86-64 Linux 4.4.0-59-generic (author's environment)

Also, it depends on the following libraries:

+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ trivia :
    

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


