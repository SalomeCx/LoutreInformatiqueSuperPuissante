(load "abstract-enum.lisp")

;;Enumérateur parallèle

(defclass parallel-enum (abstract-enum) 
  ((list-element :accessor list-element :initarg :list-element)))

(defun make-parallel-enum (list)
  (make-instance 'parallel-enum
		 :list-element (mapcar #'copy-enum list)))

(defmethod init-enum ((e parallel-enum))
  (mapcar #'init-enum (list-element e))
  e)

(defmethod copy-enum ((e parallel-enum))
  (make-parallel-enum (list-element e)))

(defmethod next-element-p ((e parallel-enum))
  (every #'next-element-p (list-element e)))

(defmethod next-element ((e parallel-enum))
  (mapcar #'call-enum (list-element e)))