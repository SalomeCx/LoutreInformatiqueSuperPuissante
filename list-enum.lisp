(load "abstract-enum.lisp")

(defclass list-enum (abstract-enum) 
  ((list-elem :reader list-elem) (is-circular :reader is-circular)))

(defun make-list-enum (l &optional (circ nil))
  (assert (l)
	  (make-instance 'list-enum :first-element (car l) :list-elem l :is-circular circ)))


