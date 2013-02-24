(cl:in-package #:clcl)

(define-sum-type unify-state (env)
  (unified aliases visited)
  (unify-error place expr)
  (occurs-check place expr visited))

(defgeneric unify-types (place expr state))

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
           (iter (for (name2 . dest) in aliases)
                 (if (equal name name2)
                     (let ((ret (resolve-var state dest (list* dest seen))))
                       (and ret (return ret))))))))
    (_ nil)))

(defun maybe-resolve-var (state type)
  (match type
    ((class variable-type)
     (or (resolve-var state type '())
         ))
    (x x)))

(defmethod unify-types :around ((place ocl-type)
                                (expr ocl-type)
                                (state unify-state))
  (setf state (visited-check place expr state))
  (if (typep state 'unified)
      (call-next-method (maybe-resolve-var state place)
                        (maybe-resolve-var state expr)
                        state)
      state))

(defmethod unify-types ((place ocl-type)
                        (expr ocl-type)
                        (state unify-state))
  (make-instance 'unify-error :place place :expr expr))

(defmacro define-unification ((type-name place-name expr-name state)
                              &body body)
  `(defmethod unify-types ((,place-name ,type-name)
                           (,expr-name ,type-name)
                           (,state unify-state))
     ,@body))

(define-unification (native-type place expr state)
  (equal (name-of place) (name-of expr)))

(defmethod unify-types ((place variable-type)
                        (expr ocl-type)
                        (state unify-state))
    (make-instance 'unified
                   :aliases (list* (cons place expr)
                                   (aliases-of state))
                   :visited (visited-of state)))

(defmethod unify-types ((place ocl-type)
                        (expr variable-type)
                        (state unify-state))
  (make-instance 'unify-error :place place :expr expr))

(define-unification (variable-type place expr state)
  (make-instance 'unified
                 :aliases (list* (cons place expr)
                                 (cons expr place)
                                 (aliases-of state))
                 :visited (visited-of state)))

(defun reduce-state-fn (seed f places exprs)
  (iter (with seed = seed)
        (for p in places)
        (for e in exprs)
        (if (not (typep seed 'unified))
            (return seed))
        (setf seed (funcall f p e seed))
        (finally (return seed))))

(defmacro reduce-state (seed &rest fn-and-list-pairs)
  (multiple-value-ematch (values seed fn-and-list-pairs)
    ((_ '())
     seed)
    ((seed (list* f places exprs xs))
     (with-unique-names (seed-name)
       `(let ((,seed-name (reduce-state-fn ,seed ,f ,places ,exprs)))
          (if (typep ,seed-name 'unified)
              (reduce-state ,seed-name . ,xs)
              ,seed-name))))))

(define-unification (user-type place expr state)
  (with-accessors* ((place (pfkind free-kind-of)
                           (pnames member-names-of)
                           (ptypes list-of-types))
                    (expr (efkind free-kind-of)
                          (enames member-names-of)
                          (etypes list-of-types)))
    (if (and (= (length pfkind)
                (length efkind))
             (= (length pnames)
                (length enames)))
        (reduce-state state
          #'equal pnames enames
          #'unify-types ptypes etypes
          #'unify-types ptypes etypes))))
