(cl:in-package #:clcl)

(define-sum-type env ()
  (ocl-env (lexenv t (empty-map))))

(defun augment-env* (env d x)
  (values (make-instance 'ocl-env
                         :lexenv (fset:with (lexenv-of env) d x))
          d))

(defun get-type-by-symbol (env d)
  (lookup (lexenv-of env) d))

(defun augment-env (env x &aux (name (string (car (name-of x)))))
  (augment-env* env
                (iter (with sym = (make-symbol name))
                      (with cnt = (list sym 1))
                      (if (not (get-type-by-symbol env cnt))
                          (return cnt))
                      (incf (second cnt))) x))
