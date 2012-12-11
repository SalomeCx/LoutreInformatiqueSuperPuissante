(defclass abstract-enum () 
  ((first-element :accessor first-element :initarg :first-element) 
   (next-elem :accessor next-elem :initarg :next-elem)))


(defgeneric init-enum (enum)
  (:documentation
   "reinitializes and returns ENUM"))

(defgeneric copy-enum (enum)
  (:documentation
   "returns a reinitialized copy of ENUM"))

(defgeneric next-element-p (enum)
  (:documentation
   "returns NIL if there is no next element, a non NIL value otherwise"))

(defgeneric next-element (enum)
  (:documentation
   "returns the next element, moves to the following one"))

(defgeneric call-enum (enum)
  (:documentation
   "if there is a next element e, returns e and T and moves to the next element, otherwise returns NIL and NIL"))

(defmethod init-enum ((e abstract-enum))
  e)

(defmethod call-enum ((e abstract-enum))
  (if (next-element-p e)
      (values (next-element e) t)
      (values nil nil)))



(defclass list-enum (abstract-enum)
  ((list-elem :accessor list-elem :initarg :list-elem) 
   (is-circular :accessor is-circular :initarg :is-circular)))

(defun make-list-enum (l &optional (circ nil))
  (make-instance 'list-enum 
		 :first-element (car l) 
		 :list-elem l 
		 :is-circular circ 
		 :next-elem l))

(defmethod init-enum ((e list-enum))
  (setf (next-elem e) (list-elem e))
  e)

(defmethod copy-enum ((e list-enum))
  (make-list-enum (list-elem e) (is-circular e)))

(defmethod next-element-p ((e list-enum))
  (not (endp (next-elem e))))

(defmethod next-element ((e list-enum))
  (prog1 (car (next-elem e))
    (setf (next-elem e) (cdr (next-elem e)))))

(defmethod call-enum ((e list-enum))
  (when (is-circular e)
      (when (not (next-element-p e))
	  (init-enum e)))
  (call-next-method e))
 


(defclass inductive-enum (abstract-enum)
  ((function-enum :accessor function-enum :initarg :function-enum)))

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
    


(defclass parallel-enum (abstract-enum)
  ((list-enum :accessor list-enum :initarg :list-enum)))

(defun make-parallel-enum (list)
  (make-instance 'parallel-enum
		 :list-enum (mapcar #'copy-enum list)
		 :first-element (mapcar #'first-element list)
		 :next-elem '()))

(defmethod init-enum ((e parallel-enum))
  (mapcar #'init-enum (list-enum e))
  (setf (next-elem e) '())
  e)

(defmethod copy-enum ((e parallel-enum))
  (make-parallel-enum (list-enum e)))

(defmethod next-element-p ((e parallel-enum))
  (every #'next-element-p (list-enum e)))

(defmethod next-element ((e parallel-enum))
  (setf (next-elem e) (mapcar #'next-element (list-enum e)))
  (next-elem e))

(defclass filter-enum (abstract-enum)
  ((f-enum :accessor f-enum :initarg :f-enum)
   (pred :accessor pred :initarg :pred)))

(defun make-filter-enum (e p)
  (let ((enum (copy-enum e)))
  (make-instance 'filter-enum
		 :f-enum enum
		 :pred p
		 :first-element (first-element enum)
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
  (let ((next (next-element-p e))
	(tmp (next-element e)))
    (if (not next) 
	(values nil nil)
	(if (not (funcall (pred e) tmp))
	    (call-enum e)
	    (values tmp t)))))

(defclass concatenation-enum (abstract-enum)
  ((list-enum :accessor list-enum :initarg :list-enum)))

(defun make-concatenation-enum (list)
  (make-instance 'concatenation-enum
		 :list-enum (mapcar #'copy-enum list)
		 :first-element (first-element (car list))
		 :next-elem (mapcar #'copy-enum list)))

(defmethod init-enum ((e concatenation-enum))
  (mapcar #'init-enum (list-enum e))
  (setf (next-elem e) (mapcar #'copy-enum (list-enum e)))
  e)

(defmethod copy-enum ((e concatenation-enum))
  (make-concatenation-enum (list-enum e)))

(defmethod next-element-p ((e concatenation-enum))
  (if (endp (next-elem e))
      nil
      (if (equal (length (next-elem e)) 1)
	  (next-element-p (car (next-elem e)))
	  t)))
     

(defmethod next-element ((e concatenation-enum))
  (if (next-element-p e)
      (if (not (next-element-p (car (next-elem e))))
	  (prog2 (setf (next-elem e) (cdr (next-elem e)))
	      (next-element e))
	  (next-element (car (next-elem e))))))

	  
  