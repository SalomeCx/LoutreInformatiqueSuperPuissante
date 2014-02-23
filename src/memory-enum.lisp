(load "abstract-enum.lisp")

;; Enumérateur à mémoire.

(defclass memory-enum (abstract-enum)
  ((m-enum :accessor m-enum :initarg :m-enum)
   (next-elem :accessor next-elem :initarg :next-elem)
   (rep :accessor rep :initarg :rep)
   (end :accessor end :initarg :end)
   (out :accessor out :initarg :out)))

(defun make-memory-enum (en)
  (let ((e (copy-enum en)))
    (init-enum (make-instance 'memory-enum
		   :rep nil
		   :m-enum e
		   :end (not (next-element-p e))
		   :out (not (next-element-p e))
		   :next-elem nil))))

(defmethod init-enum ((e memory-enum))
  (setf (rep e) nil)
  (init-enum (m-enum e))
  (setf (end e) (not (next-element-p (m-enum e))))
  (setf (next-elem e) nil)
  (init-enum (m-enum e))
  (setf (out e) (not (next-element-p (m-enum e))))
  e)

(defmethod copy-enum ((e memory-enum))
  (make-memory-enum (m-enum e)))

(defmethod next-element-p ((e memory-enum))
  (if (rep e)
      (not (out e))
      (not (end e))))
    
(defmethod next-element ((e memory-enum))
  (if (rep e)
      (next-elem e)
      (prog1 
	  (setf (next-elem e) (next-element (m-enum e)))
	(if (not (next-element-p (m-enum e)))
	    (setf (end e) t)))))

(defmethod call-enum ((e memory-enum))
  (if (next-element-p e)
      (values (next-element e) t)
      (progn (setf (out e) t)
	     (values nil nil))))

(defmethod unset-memo ((e memory-enum))
  (setf (rep e) nil)
  e)

(defmethod set-memo ((e memory-enum))
  (setf (rep e) t)
  e)