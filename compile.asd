(in-package :cl-user)

(defpackage lambdaman
  (:use :cl :asdf))

(in-package :lambdaman)

(defsystem lambdaman
  :name "Lambda-man lisp compiler"
  :version "1.0"
  :description "Compiler for the Lambda-man lisp co-processor"
  :author "Raging Mushrooms"
  :components ((:file "compile"))
  :depends-on nil)
