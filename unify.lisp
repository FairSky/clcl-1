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
      (values state nil)))

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
      (values state expr)
      (values (make-instance 'unify-error
                             :place place
                             :expr expr
                             :env (env-of state))
              nil)))

(defmethod unify-types ((place variable-type)
                        (expr ocl-type)
                        (state unify-state))
  (values (derive-unify-state state
                              :aliases-place (list* (cons place expr)
                                                    (aliases-place-of state)))
          expr))

(defmethod unify-types ((place ocl-type)
                        (expr variable-type)
                        (state unify-state))
  (values (make-instance 'unify-error
                         :place place
                         :expr expr
                         :env (env-of state))
          nil))

(def-unify-types (variable-type place expr state)
  (if (and (eql (counter-of place) (counter-of expr))
           (eq (name-of place) (name-of expr)))
      (values state expr)
      (values (derive-unify-state state
               :aliases-place (list* (cons place expr)
                                     (aliases-place-of state))
               :aliases-expr (list* (cons expr place)
                                    (aliases-expr-of state)))
              expr)))

(defun reduce-state-fn (seed expr f places exprs)
  (iter (with seed = seed)
        (for p in places)
        (for e in exprs)
        (if (not (typep seed 'unified))
            (return seed))
        (setf seed (funcall f p e seed))
        (finally (return seed))))

(defmacro reduce-state (seed expr &rest fn-and-list-pairs)
  (multiple-value-ematch (values seed fn-and-list-pairs)
    ((_ '())
     `(values ,seed ,expr))
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
  (with-accessors* ((place (pnames member-names-of)
                           (ptypes list-of-types))
                    (expr (enames member-names-of)
                          (etypes list-of-types)))
    (if (= (length pnames)
           (length enames))
        (reduce-state state expr
                      #'equal pnames enames
                      #'unify-types ptypes etypes)
        (values (make-instance 'unify-error
                               :place place
                               :expr expr
                               :env (env-of state))
                nil))))

(def-unify-types (function-type place expr state)
  (with-accessors* ((place (ptypes member-types-of))
                    (expr (etypes member-types-of)))
    (if (= (length ptypes)
           (length etypes))
        (reduce-state state expr #'unify-types ptypes etypes)
        (values (make-instance 'unify-error
                               :place place
                               :expr expr
                               :env (env-of state))
                nil))))

(defgeneric functorize (type bound-kinds bound-vars state))

(defmethod functorize ((type ocl-type) bound-kinds bound-vars state)
  nil)

(defun apply-functor-to-type-list (list bound-vars bound-kinds state)
  (labels ((rec (bound-vars bound-kinds acc)
             (if (endp bound-vars)
                 (nreverse acc)
                 (let ((kind (first bound-kinds))
                       (var (first bound-vars)))
                   (mapcar (lambda (type)
                             (if (and (typep type 'variable-type)
                                      (eql (name-of var)
                                           (name-of type))
                                      (eql (counter-of var)
                                           (counter-of type)))
                                 (multiple-value-call
                                     (lambda (_ type2)
                                       (declare (ignore _))
                                       (rec (rest bound-vars)
                                            (rest bound-kinds)
                                            (cons (or type2 type)
                                                  acc)))
                                   (unify-types type kind state))
                                 type))
                           list)))))
    (rec bound-vars bound-kinds '())))

(defmethod functorize ((type module) bound-kinds bound-vars state)
  (let ((types (apply-functor-to-type-list (member-types-of type)
                                           bound-vars
                                           bound-vars
                                           state)))
    
    (and (not (equal types (member-types-of type)))
         (make-instance 'module
                        :path (path-of type)
                        :member-types types
                        :member-names (member-names-of type)
                        :name (name-of type)))))

(defmethod functorize ((type function-type) bound-kinds bound-vars state)
  (let ((types (apply-functor-to-type-list (member-types-of type)
                                           bound-vars
                                           bound-vars
                                           state)))
    (and (not (equal types (member-types-of type)))
         (make-instance 'function-type
                        :member-types types
                        :member-names (member-names-of type)
                        :name (name-of type)))))

(defmethod functorize ((type variable-type) bound-kinds bound-vars state)
  (let ((pos (position (cons (name-of type)
                             (counter-of type))
                       bound-vars
                       :key (lambda (x)
                              (cons (name-of x)
                                    (counter-of x)))
                       :test #'equal)))
    (if pos
        (elt bound-kinds pos)
        nil)))
