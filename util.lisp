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

(defun mklist (x)
  (if (listp x)
      x
      (list x)))
