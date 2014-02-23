(load "abstract-enum.lisp")

;;Enumérateur concatenation d'énumérateurs.

(defclass concatenation-enum (abstract-enum)
  ((list-element :accessor list-element :initarg :list-element)
   (next-elem :accessor next-elem :initarg :next-elem)))

(defun make-concatenation-enum (list)
  (let ((l (mapcar #'copy-enum list)))
  (make-instance 'concatenation-enum
		 :list-element l
		 :next-elem l)))

(defmethod init-enum ((e concatenation-enum))
  (mapcar #'init-enum (list-element e))
  (setf (next-elem e) (list-element e))
  e)

(defmethod copy-enum ((e concatenation-enum))
  (make-concatenation-enum (list-element e)))

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
	      (call-enum e))
	  (call-enum (car (next-elem e))))))