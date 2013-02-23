(cl:in-package #:clcl)

(define-sum-type env
  (ocl-env (symbols-to-types t (make-hash-table :test #'eq))
           (types-to-symbols t (make-hash-table :test #'eq))
           (gensym-counter integer -1)))

(defun augment-env (env x)
  (with-accessors ((tts types-to-symbols-of)
                   (stt symbols-to-types-of)
                   (g gensym-counter-of))
      env
    (or (gethash x tts)
        (let* ((designator (list (make-symbol (string (car (name-of x))))
                                 (1+ g)))
               (stt (copy-hash-table stt))
               (tts (copy-hash-table tts)))
          (setf (gethash designator stt) x)
          (setf (gethash x tts) designator)         
          (values (make-instance 'ocl-env
                                 :gensym-counter (1+ g)
                                 :types-to-symbols tts
                                 :symbols-to-types stt)
                  designator)))))

(defun get-env (env designator)
  (gethash designator
           (types-to-symbols-of env)))

(defun get-env-or-die (env designator)
  (or (get-env env designator)
      (error "Undefined variable binding ~S" designator)))
