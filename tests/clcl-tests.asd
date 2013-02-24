(asdf:defsystem clcl
  :depends-on (#:fiveam #:clcl)
  :serial t
  :components
  (:file "tests/test-package")
  (:file "tests/test-unify"))
