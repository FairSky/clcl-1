(cl:in-package #:clcl)

(define-sum-type unify-state (env)
  (unified aliases-place aliases-expr visited)
  (unify-error place expr)
  (occurs-check place expr visited))

(defgeneric unify-types (place expr state))

(defun visited-check (place expr state)
  (match state
    ((class unified visited aliases-place aliases-expr)
     (if (not (position (list place expr) (visited-of state)  :key #'equal))
         (make-instance 'unified
                        :aliases-place aliases-place
                        :aliases-expr aliases-expr
                        :visited visited)
         (make-instance 'occurs-check
                        :place place
                        :expr expr
                        :visited (list* (list place expr) visited))))
    (x x)))

(defun resolve-var (state name seen placep)
  (match state
    ((class unified)
     (with-accessors ((ap aliases-place-of)
                      (ae aliases-expr-of))
         state
       (if (position name seen :test #'equal)
           nil
           (iter (with aliases = (if placep ap ae))
                 (for (name2 . dest) in aliases)
                 (if (equal name name2)
                     (let ((ret (resolve-var state dest (list* dest seen) placep)))
                       (and ret (return ret))))))))
    (_ nil)))

(defun maybe-resolve-var (state type placep)
  (match type
    ((class variable-type)
     (or (resolve-var state type '() placep)
         type))
    (x x)))

(defmethod unify-types :around ((place ocl-type)
                                (expr ocl-type)
                                (state unify-state))
  (setf state (visited-check place expr state))
  (if (typep state 'unified)
      (call-next-method (maybe-resolve-var state place t)
                        (maybe-resolve-var state expr nil)
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

(defmethod unify-types ((place ocl-type)
                        (expr ocl-type)
                        (state unify-state)))

(defmethod unify-types ((place variable-type)
                        (expr ocl-type)
                        (state unify-state))
    (make-instance 'unified
                   :aliases-place (list* (cons place expr)
                                         (aliases-place-of state))
                   :aliases-expr (aliases-expr-of state)
                   :visited (visited-of state)))

(defmethod unify-types ((place ocl-type)
                        (expr variable-type)
                        (state unify-state))
  (make-instance 'unify-error :place place :expr expr))

(define-unification (variable-type place expr state)
  (make-instance 'unified
                 :aliases-place (list* (cons place expr)
                                       (aliases-place-of state))
                 :aliases-expr (list* (cons expr place)
                                      (aliases-expr-of state))
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
          #'unify-types ptypes etypes)
        (make-instance 'unify-error
                       :place place
                       :expr expr
                       :env (env-of state)))))

(define-unification (function-type place expr state)
  (with-accessors* ((place (pkinds free-kind-of)
                           (ptypes member-types-of))
                    (expr (ekinds free-kind-of)
                          (etypes member-types-of)))
    (if (and (= (length pkinds)
                (length ekinds))
             (= (length ptypes)
                (length etypes)))
        (reduce-state state #'unify-types ptypes etypes)
        (make-instance 'unify-error
                       :place place
                       :expr expr
                       :env (env-of state)))))

(define-unification (functor place expr state)
  (with-accessors* ((place (pktypes free-kind-of)
                           (pkvars free-vars-of)
                           (pinner inner-type-of))
                    (place (ektypes free-kind-of)
                           (ekvars free-vars-of)
                           (einner inner-type-of)))
    (if (and (= (length pktypes)
                (length ektypes)))
        (reduce-state state
                      #'unify-types pktypes ektypes
                      #'unify-types (list pinner) (list einner))
        (make-instance 'unify-error
                       :place place
                       :expr expr
                       :env (env-of state)))))

(defun unify-something-with-functor (place expr state)
  (if  (= (length (free-kind-of place))
          (length (free-kind-of expr)))
       (= (length (member-names-of place))
          (length (member-names-of (inner-type-of expr))))
       (unify-types place (inner-type-of expr))
       (make-instance 'unify-error
                      :place place
                      :expr expr
                      :env (env-of state))))

(defmethod unify-types ((place user-type) (expr functor) state)
  (unify-something-with-functor place expr state))

(defmethod unify-types ((place function-type) (expr functor) state)
  (unify-something-with-functor place expr state))
