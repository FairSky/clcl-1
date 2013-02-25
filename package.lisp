(cl:defpackage #:clcl
  (:use #:cl #:alexandria  #:closer-mop #:iterate #:x.let-star #:optima #:fset)
  (:shadowing-import-from #:closer-mop
                          #:defmethod #:defgeneric #:standard-generic-function)
  (:shadowing-import-from #:x.let-star #:let*)
  (:shadowing-import-from #:fset
                          ;; Shadowed type/constructor names
                          #:set #:map
                          ;; Shadowed set operations
                          #:union #:intersection #:set-difference #:complement
                          ;; Shadowed sequence operations
                          #:first #:last #:subseq #:reverse #:sort #:stable-sort
                          #:reduce
                          #:find #:find-if #:find-if-not
                          #:count #:count-if #:count-if-not
                          #:position #:position-if #:position-if-not
                          #:remove #:remove-if #:remove-if-not
                          #:substitute #:substitute-if #:substitute-if-not
                          #:some #:every #:notany #:notevery
                          #:appendf #:removef #:unionf #:compose)
  (:shadowing-import-from #:iterate #:with)
  (:export #:unify-types
           #:ocl-type
           #:native-type
           #:variable-type
           #:module
           #:function-type
           #:functor
           #:dimension
           #:unified
           #:unify-error
           #:occurs-check
           #:unify-state
           #:ocl-env
           #:simple-name
           #:functorized-name))
