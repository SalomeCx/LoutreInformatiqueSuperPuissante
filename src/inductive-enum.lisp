(load "abstract-enum.lisp")

;;Enumérateur inductif

(defclass inductive-enum (abstract-enum)
  ((first-element :accessor first-element :initarg :first-element)
   (function-enum :accessor function-enum :initarg :function-enum)
   (next-elem :accessor next-elem :initarg :next-elem)))

(defun make-inductive-enum (function first-value)
  (make-instance 'inductive-enum
		 :first-element first-value
		 :function-enum function
		 :next-elem (funcall function first-value)))

(defmethod init-enum ((e inductive-enum))
  (setf (next-elem e) (funcall (function-enum e) (first-element e)))
  e)

(defmethod copy-enum((e inductive-enum))
  (make-inductive-enum (function-enum e) (first-element e)))

(defmethod next-element-p ((e inductive-enum))
  t)

(defmethod next-element ((e inductive-enum))
  (prog1 (next-elem e)
    (setf (next-elem e) (funcall (function-enum e) (next-elem e)))))