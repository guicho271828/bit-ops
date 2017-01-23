#|
  This file is a part of bit-ops project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage bit-ops.test-asd
  (:use :cl :asdf))
(in-package :bit-ops.test-asd)


(defsystem bit-ops.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of bit-ops"
  :license "LLGPL"
  :depends-on (:bit-ops
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(let ((res (5am:run :bit-ops)))
     (explain! res)
     (every #'fiveam::TEST-PASSED-P res))"))
))
