#|
  This file is a part of bit-ops project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :bit-ops.test
  (:use :cl
        :bit-ops
        :fiveam
        :iterate :alexandria :trivia))
(in-package :bit-ops.test)



(def-suite :bit-ops)
(in-suite :bit-ops)

;; run test with (run! test-name) 

(test bit-ops

  )



