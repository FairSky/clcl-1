(cl:in-package #:clcl)

(define-sum-type name-designator ((name symbol) (counter integer))
  simple-name
  (functorized-name types names))

(defgeneric functorize-name (name vars kinds))

(defmethod functorize-name ((name simple-name) vars kinds)
  (make-instance 'functorized-name
                 :types kinds
                 :names vars
                 :name (name-of name)))

(defmethod functorize-name ((name functorized-name) vars kinds)
  (make-instance 'functorized-name
                 :types (append kinds (types-of name))
                 :names (append vars (types-of name))
                 :name (name-of name)))

(define-sum-type ocl-type
    ((name name-designator))
  (native-type)
  (variable-type (counter integer))
  (module (member-names list-of-names)
          (member-types list-of-types)
          (path list-of-strings))
  (function-type (member-names list-of-names)
                 (member-types list-of-types))
  (dimension (int (integer 1))))

(define-list-predicate list-of-names name-designator)
(define-list-predicate list-of-types ocl-type)
(define-list-predicate list-of-vars variable-type)
(define-list-predicate list-of-strings string)

(define-sum-type expr-repr ()
  infix
  prefix
  postfix
  function-call)
