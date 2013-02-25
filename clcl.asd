(asdf:defsystem #:clcl
  :depends-on (#:alexandria #:closer-mop #:iterate #:x.let-star #:optima #:fset)
  :serial t
  :components
  ((:file "package")
   (:file "sum-types")
   (:file "util")
   (:file "env")
   (:file "types")
   (:file "unify")))
