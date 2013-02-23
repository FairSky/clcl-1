(cl:in-package #:clcl)

(defun ensure-var (x)
  (etypecase x
    (symbol
     (list x 't '()))
    ((cons symbol (cons t null))
     (append x (list '())))
    ((cons symbol (cons t (cons t null)))
     x)))

(defun bastardize-symbol (orig)
  (intern (format nil "~A-~A" orig #.(string '#:of))))

(define-condition missing-argument-error (error)
  ((name :accessor missing-argument-error-name :initarg :name)
   (type :accessor missing-argument-error-type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Mising argument ~S for ~S"
                     (missing-argument-error-name condition)
                     (missing-argument-error-type condition)))))

(defun need-arg (name type)
  (error 'missing-argument-error :name name :type type))

(defun ensure-class-name (class-name)
  (etypecase class-name
    ((cons symbol t) class-name)
    (symbol (cons class-name '()))))

(defun parse-sum-record (class-name parent body)
  (let* ((slots (iter (for var-decl in body)
                      (for (var type initform) = (ensure-var var-decl))
                      (collect `(,var :type ,type
                                      :reader ,(bastardize-symbol var)
                                      :initarg ,(intern (symbol-name var) '#:keyword)
                                      :initform ,(if (null initform)
                                                     `(need-arg ',var ',class-name)
                                                     initform)))))
         ((class-name . options) (ensure-class-name class-name)))
    `(defclass ,class-name (,parent)
       ,slots
       ,@options)))

(defun make-sum-type-body (class-name declarations)
  (iter (with (class-name . options) = (ensure-class-name class-name))
        (for (name . body) in (mapcar #'mklist declarations))
        (collect (parse-sum-record name class-name body) into slots)
        (finally (return `(progn (defclass ,class-name () () . ,options)
                                 ,@slots
                                 ',class-name)))))

(defmacro define-sum-type (class-name &body declarations)
  (make-sum-type-body class-name declarations))
