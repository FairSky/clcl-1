(cl:defpackage #:clcl
  (:use #:cl #:alexandria #:x.let-star  #:closer-mop #:iterate #:hu.dwim.def)
  (:shadowing-import-from #:closer-mop
                          #:defmethod #:defgeneric #:standard-generic-function)
  (:shadowing-import-from #:x.let-star #:let*))
