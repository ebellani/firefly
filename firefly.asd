;;;; firefly.asd

(asdf:defsystem #:firefly
  :serial t
  :description "Set of functions/macros to work with SBCL backtraces"
  :author "Eduardo Bellani <ebellani@gmail.com>"
  :license "THE BEER-WARE LICENSE (Revision 42) See LICENSE"
  :components ((:file "package")
               (:file "firefly")))
