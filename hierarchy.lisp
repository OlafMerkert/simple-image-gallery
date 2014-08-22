(in-package :simple-image-gallery)

(defclass hierarchy-object ()
  ((identifier :initarg :identifier
               :initform ""
               :reader identifier)
   )
  (:documentation "TODO"))

(defgeneric super-object (object))

(defgeneric sub-objects (object &optional type))

(defgeneric find-sub-object (identifier object &optional type))

(defpar root-object 'gallery-root)


(defun find-object (identifier-list &optional (object root-object))
  (if identifier-list
      (aif (find-sub-object (first identifier-list) object)
           (find-object (rest identifier-list) it))
      object))

(defun find-object-hierarchy (identifier-list &optional (object root-object))
  (labels ((find% (identifier-list object object-list)
             (if identifier-list
                 (aif (find-sub-object (first identifier-list) object)
                      (find% (rest identifier-list) it (cons it object-list)))
                 (values object-list object))))
    (find% identifier-list object (list object))))
