(cl:in-package #:clcl)

(define-sum-type env
  (ocl-env (symbols-to-types t (make-hash-table :test #'equal))
           (types-to-symbols t (make-hash-table :test #'equal))))

(defun get-env (env designator)
  (gethash designator
           (types-to-symbols-of env)))

(defun augment-env (env x)
  (with-accessors ((tts types-to-symbols-of)
                   (stt symbols-to-types-of))
      env
    (or (gethash x tts)
        (let* ((designator (iter (with sym = (make-symbol (string (car (name-of x)))))
                                 (with cnt = (list sym 0))
                                 (if (not (get-env env cnt))
                                     (return cnt))
                                 (incf (second cnt))))
               (stt (copy-hash-table stt))
               (tts (copy-hash-table tts)))
          (setf (gethash designator stt) x)
          (setf (gethash x tts) designator)         
          (values (make-instance 'ocl-env
                                 :types-to-symbols tts
                                 :symbols-to-types stt)
                  designator)))))

(defun get-env-or-die (env designator)
  (or (get-env env designator)
      (error "Undefined variable binding ~S" designator)))
