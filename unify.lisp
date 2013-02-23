(cl:in-package #:clcl)

(define-sum-type unify-state
  (unified aliases visited)
  (unify-error place expr)
  (occurs-check place expr visited))

(defgeneric unify-types (env place expr state))

(defun visited-check (place expr state)
  (match state
    ((class unified visited aliases)
     (if (not (position (list place expr) state :key #'equal))
         (make-instance 'unified :aliases aliases :visited visited)
         (make-instance 'occurs-check
                        :place place
                        :expr expr
                        :visited (list* (list place expr) visited))))
    (x x)))

(defun resolve-var (state name seen)
  (match state
    ((class unified)
     (with-accessors ((aliases aliases-of)) state
       (if (position name seen :test #'equal)
           nil
           (match (cdr (find name aliases :key #'car))
             ((and name (type variable-type))
              (resolve-var state name (list* name seen)))
             (x x)))))
    (_ nil)))

(defun maybe-resolve-var (state type)
  (match type
    ((class variable-type)
     (or (resolve-var state type '())
         ))
    (x x)))

(defmethod unify-types :around ((env ocl-env)
                                (place ocl-type)
                                (expr ocl-type)
                                (state unify-state))
  (setf state (visited-check place expr state))
  (if (typep state 'unified)
      (call-next-method env
                        (maybe-resolve-var state place)
                        (maybe-resolve-var state expr)
                        state)
      state))

(defmethod unify-types ((env ocl-env)
                        (place ocl-type)
                        (expr ocl-type)
                        (state unify-state))
  (make-instance 'unify-error :place place :expr expr))

(defmacro define-unification ((type-name env-name place-name expr-name state)
                              &body body)
  `(defmethod unify-types ((,env-name ocl-env)
                           (,place-name ,type-name)
                           (,expr-name ,type-name)
                           (,state unify-state))
     ,@body))

(define-unification (native-type env place expr state)
  (equal (name-of place) (name-of expr)))

(defmethod unify-types ((env ocl-env)
                        (place variable-type)
                        (expr ocl-type)
                        (state unify-state))
    (make-instance 'unified
                   :aliases (list* (cons place expr)
                                   (aliases-of state))
                   :visited (visited-of state)))

(defmethod unify-types ((env ocl-env)
                        (place ocl-type)
                        (expr variable-type)
                        (state unify-state))
  (make-instance 'unify-error :place place :expr expr))

(define-unification (variable-type env place expr state)
  (make-instance 'unified
                 :aliases (list* (cons place expr)
                                 (cons expr place)
                                 (aliases-of state))
                 :visited (visited-of state)))


