(cl:in-package #:clcl)

(defun ensure-var (x)
  (etypecase x
    ((cons symbol null)
     (list (car x) 't '()))
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

(defun ensure-class-name (class-name)
  (etypecase class-name
    ((cons symbol (cons t null)) (values-list class-name))
    (symbol (values class-name '()))))

(defun print-sum-type (obj stream)
  (write-char #\( stream)
  (write (class-name (class-of obj)) :stream stream)
  (dolist (i (class-slots (class-of obj)))
    (let ((i (slot-definition-name i)))
      (format stream " ~A = ~A;" (string-downcase i) (and (slot-boundp obj i) (slot-value obj i)))))
  (write-char #\) stream)
  nil)

(defun parse-record-slots (body)
  (iter (for var-decl in (mklist body))
        (for (var type initform) = (ensure-var var-decl))
        (collect `(,var :type ,type
                        :reader ,(bastardize-symbol var)
                        :initarg ,(intern (symbol-name var) '#:keyword)
                        ,@(and initform (list :initform initform))))))

(defun parse-sum-record (class-name parent body class-declarations)
  (let* ((slots (parse-record-slots body))
         (class-declarations (parse-record-slots class-declarations))
         ((:mval class-name options) (ensure-class-name class-name)))
    `(progn (defclass ,class-name ,(if parent `(,parent) '())
              ,slots
              ,@options)
            (defmethod print-object ((obj ,class-name) stream)
              (print-sum-type obj stream))
            (defun ,class-name ,(mapcar #'first (append slots class-declarations))
              (make-instance ',class-name
                             ,@(mappend (lambda (x)
                                          (list (intern (string x) '#:keyword)
                                                x))
                                        (append (mapcar #'first slots)
                                                (mapcar #'first class-declarations))))))))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun make-sum-type-body (class-name class-declarations declarations)
  (let* (((:mval class-name options) (ensure-class-name class-name)))
    (iter (for (name . body) in (mapcar #'mklist declarations))
          (collect (parse-sum-record name class-name body class-declarations) into slots)
          (finally (return `(progn (defclass ,class-name () ,(parse-record-slots class-declarations) . ,options)
                                   ,@slots
                                   ',class-name))))))

(defmacro define-sum-type (class-name class-declarations &body declarations)
  (make-sum-type-body class-name class-declarations declarations))
