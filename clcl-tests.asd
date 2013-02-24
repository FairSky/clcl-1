(asdf:defsystem #:clcl-tests
  :depends-on (#:fiveam #:clcl)
  :serial t
  :components ((:file "test-package")
               (:file "test-unify")))
