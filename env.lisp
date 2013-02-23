(cl:in-package #:clcl)

(define-sum-type env
  (ocl-env (symbols-to-types t '())))

(defun augment-env* (env d x)
  (with-accessors ((tts types-to-symbols-of)
                   (stt symbols-to-types-of))
      env
    (values (make-instance 'ocl-env
                           :symbols-to-types (list* (cons d x) stt))
            d)))

(defun get-env-by-symbol (env d)
  (cdr (assoc d (symbols-to-types-of env))))

(defun get-env-by-type (env d)
  (car (find d (symbols-to-types-of env) :key #'cdr)))

(defun augment-env (env x &aux (name (string (car (name-of x)))))
  (augment-env* env
                (iter (with sym = (make-symbol name))
                      (with cnt = (list sym 1))
                      (if (not (get-env-by-symbol env cnt))
                          (return cnt))
                      (incf (second cnt))) x))
