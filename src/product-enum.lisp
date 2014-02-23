(load "abstract-enum.lisp")
(load "memory-enum.lisp")

;; Enumérateur produit d'énumérateurs.

(defclass product-enum (abstract-enum)
  ((list-element :accessor list-element :initarg :list-element)
   (fun :accessor fun :initarg :fun)
  (has-next :accessor has-next :initarg :has-next)))

(defun make-product-enum (list fun)
  (let ((e (make-instance 'product-enum
			  :list-element (mapcar #'make-memory-enum list) 
			  :fun fun
			  :has-next nil)))
    (setf (has-next e) (every #'next-element-p (list-element e)))
    (mapcar #'call-enum (list-element e))
    (mapcar #'set-memo (list-element e))
    e))

(defmethod init-enum ((e product-enum))
  (mapcar #'init-enum (list-element e))
  (setf (has-next e) (every #'next-element-p (list-element e)))
  (mapcar #'call-enum (list-element e))
  (mapcar #'set-memo (list-element e))
  e)

(defmethod copy-enum ((e product-enum))
  (make-product-enum (list-element e) (fun e)))

(defmethod next-element-p ((e product-enum))
  (has-next e))

(defmethod next-element ((e product-enum))
  (prog1 (apply (fun e) (mapcar #'call-enum (list-element e)))
    (setf (has-next e) (suivant (reverse (list-element e))))))

(defun suivant (l)
  (unset-memo (car l))
  (call-enum (car l))
  (set-memo (car l))
  (if (not (next-element-p (car l)))
    (if (not (eq (length l) 1))
	(progn (init-enum (car l))
	  (call-enum (car l))
	  (set-memo (car l))
	  (suivant (cdr l)))
	nil)
      t))