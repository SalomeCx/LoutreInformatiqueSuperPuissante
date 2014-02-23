(load "abstract-enum.lisp")

;;Enumérateur à filtre

(defclass filter-enum (abstract-enum)
  ((f-enum :accessor f-enum :initarg :f-enum)
   (pred :accessor pred :initarg :pred)
   (next-elem :accessor next-elem :initarg :next-elem)))

(defun make-filter-enum (e p)
  (let ((enum (copy-enum e)))
    (make-instance 'filter-enum
		   :f-enum enum
		   :pred p
		   :next-elem nil)))

(defun filtrer(p e)
  (make-filter-enum e p))

(defmethod init-enum ((e filter-enum))
  (init-enum (f-enum e))
  (setf (next-elem e) nil)
  e)

(defmethod copy-enum ((e filter-enum))
  (make-filter-enum (f-enum e)
		    (pred e)))

(defmethod next-element-p ((e filter-enum))
  (next-element-p (f-enum e)))

(defmethod next-element ((e filter-enum))
  (setf (next-elem e) (next-element (f-enum e)))
  (next-elem e))

(defmethod call-enum ((e filter-enum))
    (if (not (next-element-p e))
	(values nil nil)
	 (let ((next (next-element e)))
	   (if (not (funcall (pred e) next))
	       (call-enum e)
	       (values next t)))))