(cl:in-package #:clcl)

(define-sum-type unify-state (env)
  (unified aliases-place aliases-expr visited)
  (unify-error place expr)
  (occurs-check place expr visited))

(defun derive-unify-state (env
                           &key aliases-place aliases-expr visited)
  (if (typep env 'unified)
      (make-instance 'unified
                     :visited (or visited
                                  (visited-of env))
                     :aliases-expr (or aliases-expr
                                       (aliases-expr-of env))
                     :aliases-place (or aliases-place
                                        (aliases-place-of env))
                     :env env)
      env))

(defgeneric unify-types (place expr state))

(defun visited-check (place expr state)
  (match state
    ((class unified visited)
     (if (not (position (list place expr) (visited-of state) :test #'equal))
         (derive-unify-state state
                             :visited (list* (list place expr) visited))
         (make-instance 'occurs-check
                        :place place
                        :expr expr
                        :visited visited
                        :env (env-of state))))
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
      (multiple-value-call (lambda (state result-type)
                             (if (typep state 'unified)
                                 (check-type result-type ocl-type)
                                 (check-type result-type null))
                             (values state result-type))
        (call-next-method (maybe-resolve-var state place t)
                          (maybe-resolve-var state expr nil)
                          state))
      (values state
              nil)))

(defmethod unify-types ((place ocl-type)
                        (expr ocl-type)
                        (state unify-state))
  (values (make-instance 'unify-error
                         :place place
                         :expr expr
                         :env (env-of state))
          nil))

(defmacro def-unify-types ((type-name place-name expr-name state)
                              &body body)
  `(defmethod unify-types ((,place-name ,type-name)
                           (,expr-name ,type-name)
                           (,state unify-state))
     ,@body))

(def-unify-types (native-type place expr state)
  (if (equal (name-of place) (name-of expr))
      (values state expr)))

(defmethod unify-types ((place variable-type)
                        (expr ocl-type)
                        (state unify-state))
  (derive-unify-state 'unified
                      :aliases-place (list* (cons place expr)
                                            (aliases-place-of state))))

(defmethod unify-types ((place ocl-type)
                        (expr variable-type)
                        (state unify-state))
  (values (make-instance 'unify-error
                         :place place
                         :expr expr
                         :env (env-of state))
          nil))

(def-unify-types (variable-type place expr state)
  (if (equal (name-of place) (name-of expr))
      state
      (values (derive-unify-state state
               :aliases-place (list* (cons place expr)
                                     (aliases-place-of state))
               :aliases-expr (list* (cons expr place)
                                    (aliases-expr-of state))))))

(defun reduce-state-fn (seed expr f places exprs)
  (iter (with seed = seed)
        (for p in places)
        (for e in exprs)
        (if (not (typep seed 'unified))
            (return (values seed nil)))
        (setf seed (funcall f p e seed))
        (finally (return (values seed expr)))))

(defmacro reduce-state (seed expr &rest fn-and-list-pairs)
  (multiple-value-ematch (values seed fn-and-list-pairs)
    ((_ '())
     seed)
    ((seed (list* f places exprs xs))
     (with-unique-names (seed-name expr-name)
       `(let* ((,expr-name ,expr)
               (,seed-name (reduce-state-fn ,seed
                                            ,expr-name
                                            ,f
                                            ,places
                                            ,exprs)))
          (if (typep ,seed-name 'unified)
              (reduce-state ,seed-name ,expr-name . ,xs)
              (values ,seed-name nil)))))))

(def-unify-types (module place expr state)
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
        (values (reduce-state state expr
                              #'equal pnames enames
                              #'unify-types ptypes etypes)
                expr)
        (values (make-instance 'unify-error
                               :place place
                               :expr expr
                               :env (env-of state))
                nil))))

(def-unify-types (function-type place expr state)
  (with-accessors* ((place (pkinds free-kind-of)
                           (ptypes member-types-of))
                    (expr (ekinds free-kind-of)
                          (etypes member-types-of)))
    (if (and (= (length pkinds)
                (length ekinds))
             (= (length ptypes)
                (length etypes)))
        (reduce-state state expr #'unify-types ptypes etypes)
        (values (make-instance 'unify-error
                               :place place
                               :expr expr
                               :env (env-of state))
                nil))))

(def-unify-types (functor place expr state)
  (with-accessors* ((place (pktypes bound-kind-of)
                           (pkvars bound-vars-of)
                           (pinner inner-type-of))
                    (place (ektypes bound-kind-of)
                           (ekvars bound-vars-of)
                           (einner inner-type-of)))
    (if (and (= (length pktypes)
                (length ektypes)))
        (reduce-state state expr
                      #'unify-types pktypes ektypes
                      #'unify-types (list pinner) (list einner))
        (values (make-instance 'unify-error
                               :place place
                               :expr expr
                               :env (env-of state))
                nil))))

(defun unify-something-with-functor (place expr state)
  (if  (and (= (length (member-names-of place))
               (length (member-names-of (inner-type-of expr))))
            (iter (for type in (bound-kind-of expr))
                  (for name in (bound-vars-of expr))
                  (always (and (find type
                                     (member-types-of place)
                                     :test #'equal)
                               (find name
                                     (member-names-of place)
                                     :test #'equal)))))
       (unify-types place (inner-type-of expr) state)
       (values (make-instance 'unify-error
                              :place place
                              :expr expr
                              :env (env-of state))
               nil)))

(defmethod unify-types ((place module) (expr functor) state)
  (unify-something-with-functor place expr state))

(defmethod unify-types ((place function-type) (expr functor) state)
  (unify-something-with-functor place expr state))

(defgeneric functorize (type bound-kinds bound-vars state))

(defmethod functorize ((type ocl-type) bound-kinds bound-vars state)
  nil)

#+ (or)
(defmethod functorize ((type functor) bound-kinds bound-vars state)
  (let ((foundp nil))
    (flet ((f (x y)
             (let ((foo (unify-types x y state)))
               ))))
    (mapcar (lambda (x)
              ))))
