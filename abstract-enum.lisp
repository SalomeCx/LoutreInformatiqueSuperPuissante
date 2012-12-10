(defclass abstract-enum () 
  ((first-element :reader first-element)))


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