(cl:in-package #:clcl)

(defmacro define-list-predicate (name type)
  (with-unique-names (x-name y-name)
    `(progn (defun ,name (,x-name)
              (every (lambda (,y-name)
                       (typep ,y-name ',type))
                     ,x-name))
            (deftype ,name ()
              `(and list
                    (satisfies ,',name))))))

(defmacro with-accessors* ((&rest declarations) &body body)
  (ematch declarations
    ((cons (cons obj slots) xs)
     `(with-accessors ,slots ,obj
        (with-accessors* ,xs ,@body)))
    ('nil
     `(progn . ,body))))
