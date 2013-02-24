(cl:in-package #:clcl)

(deftype type-designator ()
  '(cons symbol (cons fixnum null)))

(deftype name-designator ()
  'symbol)

(define-sum-type ocl-type
    ((name type-designator))
  (native-type)
  (variable-type)
  (user-type (free-kind list-of-vars)
             (member-names list-of-names)
             (member-types list-of-types))
  (function-type (free-kind list-of-vars)
                 (member-names list-of-names)
                 (member-types list-of-types))
  (functor (free-kind list-of-types)
           (free-vars list-of-vars)
           (inner-type ocl-type))
  (dimension (int (integer 1))))

(define-list-predicate list-of-names name-designator)
(define-list-predicate list-of-types type-designator)
(define-list-predicate list-of-vars variable-type)

(define-sum-type expr-repr ()
  infix
  prefix
  postfix
  function-call)
