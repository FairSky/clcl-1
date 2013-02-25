(cl:in-package #:clcl)

(define-sum-type env ()
  (ocl-env (lexenv t (empty-map))
           (global-names t (empty-map))))

(defun augment-env* (env d x)
  (make-instance 'ocl-env
                 :lexenv (fset:with (lexenv-of env) d x)
                 :global-names (fset:with (lexenv-of env) d x)))

(defun get-type-by-symbol (env d)
  (lookup (lexenv-of env) d))

(defun augment-env (env x &aux (name (string (name-of x))))
  (let ((d (iter (with sym = (make-symbol name))
                 (with cnt = (make-instance 'simple-name :counter 1 :name sym))
                 (if (not (get-type-by-symbol env cnt))
                     (return cnt))
                 (reinitialize-instance cnt :counter (1+ (counter-of cnt))))))
    (values (augment-env* env d x) d)))

(defun add-global-name (env string symbol value)
  (let ((foo (list symbol 0)))
    (values (fset:with (global-names-of env)
                       string
                       foo)
            foo)))
