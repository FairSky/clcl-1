(asdf:defsystem clcl
  :depends-on (#:alexandria #:closer-mop #:iterate #:x.let-star #:optima)
  :serial t
  :components
  ((:file "package")
   (:file "sum-types")
   (:file "util")
   (:file "env")
   (:file "expr")
   (:file "unify")))
