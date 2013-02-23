(cl:in-package #:clcl)

(define-sum-type unify-state
  (unified aliases visited)
  (unify-error place expr)
  (occurs-check place expr visited))

(defgeneric unify-same-types (env place expr state))

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

(defmacro define-unification ((type-name env-name place-name expr-name visited-name)
                              &body body)
  `(defmethod unify-same-types (,env-name
                                (,place-name ,type-name)
                                (,expr-name ,type-name)
                                (,visited-name t))
     (visited-check ,place-name ,expr-name ,visited-name)
     ,@body))

(defun resolve-var (env name seen)
  (if (position name seen :test #'equal)
      nil
      (ematch (get-env env name)
        ((and name (type variable-type))
         (resolve-var env name (list* name seen)))
        (x x))))

(define-unification (native-type env place expr visited)
  (equal (name-of place) (name-of expr)))

(define-unification (variable-type env place expr visited)
  )

(defun unify (env place expr visited)
  )
