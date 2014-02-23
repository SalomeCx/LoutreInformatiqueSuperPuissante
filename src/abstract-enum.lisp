;;Class abstraite

(defclass abstract-enum ()())

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