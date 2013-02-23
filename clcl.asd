(asdf:defsystem clcl
  :depends-on (#:alexandria #:closer-mop #:iterate #:x.let-star)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "sum-types")
   (:file "env")
   (:file "expr")
   (:file "unify")))
