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
    ((cons symbol (cons t null)) (values-list class-name))
    (symbol (values class-name '()))))

(defun parse-record-slots (body class-name)
  (iter (for var-decl in (mklist body))
        (for (var type initform) = (ensure-var var-decl))
        (collect `(,var :type ,type
                        :reader ,(bastardize-symbol var)
                        :initarg ,(intern (symbol-name var) '#:keyword)
                        :initform ,(if (null initform)
                                       `(need-arg ',var ',class-name)
                                       initform)))))

(defun parse-sum-record (class-name parent body)
  (let* ((slots (parse-record-slots body class-name))
         ((:mval class-name options) (ensure-class-name class-name)))
    `(defclass ,class-name ,(if parent `(,parent) '())
       ,slots
       ,@options)))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun make-sum-type-body (class-name class-declarations declarations)
  (let* (((:mval class-name options) (ensure-class-name class-name)))
    (iter (for (name . body) in (mapcar #'mklist declarations))
          (collect (parse-sum-record name class-name body) into slots)
          (finally (return `(progn (defclass ,class-name () ,(parse-record-slots class-declarations class-name) . ,options)
                                   ,@slots
                                   ',class-name))))))

(defmacro define-sum-type (class-name class-declarations &body declarations)
  (make-sum-type-body class-name class-declarations declarations))
