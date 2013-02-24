(cl:in-package #:clcl-tests)
(in-suite* unification)

(defvar *env*)

(defmacro with-ocl-env (&body body)
  `(let ((*env* (make-instance 'ocl-env)))
     ,@body))

(defun unify (place expr)
  (unify-types place expr (unified '() '() '() *env*)))

(defun print-id (x)
  (write x)
  (terpri)
  x)

(defun unify-success (place expr)
  (is (typep (print-id (unify place expr)) 'unified)))

(defun unify-failure (place expr)
  (is (typep (print-id (unify place expr)) 'unify-error)))

(defun unify-loop (place expr)
  (is (typep (print-id (unify place expr)) 'occurs-check)))

(test simple
  (with-ocl-env
    (unify-success (variable-type 'foo) (dimension 42 'array-rank))
    (unify-success (variable-type 'foo) (variable-type 'foo))))

(run!)
