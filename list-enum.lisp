(load "abstract-enum.lisp")

;;Enumérateur implémenté par des listes.

(defclass list-enum (abstract-enum)
  ((is-circular :accessor is-circular :initarg :is-circular)
   (next-elem :accessor next-elem :initarg :next-elem)
   (list-element :accessor list-element :initarg :list-element)))

(defun make-list-enum (l &optional (circ nil))
  (make-instance 'list-enum 
		 :list-element l
		 :is-circular circ 
		 :next-elem l))

(defmethod init-enum ((e list-enum))
  (setf (next-elem e) (list-element e))
  e)

(defmethod copy-enum ((e list-enum))
  (make-list-enum (list-element e) (is-circular e)))

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