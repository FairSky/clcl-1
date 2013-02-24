(cl:in-package #:clcl)

(deftype type-designator ()
  '(cons symbol (cons fixnum null)))

(deftype name-designator ()
  'symbol)

(define-sum-type ocl-type
    ((name type-designator))
  (native-type)
  (variable-type)
  (module (free-kind list-of-vars)
          (member-names list-of-names)
          (member-types list-of-types)
          (path list-of-strings))
  (function-type (free-kind list-of-vars)
                 (member-names list-of-names)
                 (member-types list-of-types))
  (functor (bound-kind list-of-types)
           (bound-vars list-of-vars)
           (inner-type ocl-type))
  (dimension (int (integer 1))))

(define-list-predicate list-of-names name-designator)
(define-list-predicate list-of-types type-designator)
(define-list-predicate list-of-vars variable-type)
(define-list-predicate list-of-strings string)

(define-sum-type expr-repr ()
  infix
  prefix
  postfix
  function-call)
