#|
  This file is a part of bit-ops project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Optimized bit-vector operations

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage bit-ops-asd
  (:use :cl :asdf))
(in-package :bit-ops-asd)


(defsystem bit-ops
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :homepage "https://github.com/guicho271828/bit-ops"
  :bug-tracker "https://github.com/guicho271828/bit-ops/issues"
  :source-control (:git "https://github.com/guicho271828/bit-ops.git")
  :license "LLGPL"
  :depends-on (:iterate :alexandria :trivia :immutable-struct :lisp-namespace)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "macros"))))
  :description "Optimized bit-vector operations"
  :in-order-to ((test-op (test-op :bit-ops.test))))
