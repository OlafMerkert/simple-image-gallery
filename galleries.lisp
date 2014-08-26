(in-package :simple-image-gallery)

(defclass abstract-gallery (hierarchy-object)
  ((title :initarg :title
          :initform ""
          :reader title)
   (description :initarg :description
                :initform ""
                :reader description)
   (image-sequence :initarg :image-sequence
                   :initform #()
                   :reader image-sequence)
   (gallery-sequence :initarg :gallery-sequence
                     :initform #()
                     :reader gallery-sequence)
   (last-updated :initarg :last-updated
                 :initform nil
                 :reader last-updated))
  (:documentation "TODO"))

(defmethod initialize-instance :after  ((abstract-gallery abstract-gallery) &key)
  (if (string-equal (identifier abstract-gallery) "data")
      (warn "Galleries with name \"data\" cannot be accessed from the web interface.")))

(defclass gallery (abstract-gallery)
  ((password :initarg :password
             :initform nil
             :reader password))
  (:documentation "A gallery has just a title, description and
  describes a sequence of images. There may also be some date
  information, and we use the folder name as identifier (this also
  allows to reconstruct the path easily)."))

(defclass sub-gallery (abstract-gallery)
  ((parent-gallery :initarg :parent
         :initform nil
         :reader parent-gallery))
  (:documentation "TODO"))

(defmethod super-object ((gallery gallery))
  root-object)

(defmethod super-object ((sub-gallery sub-gallery))
  (parent-gallery sub-gallery))

(defmethod sub-objects ((gallery abstract-gallery) &optional type)
  (case type
    (image (image-sequence gallery))
    (gallery (gallery-sequence gallery))
    ((nil) (concatenate 'vector
                        (gallery-sequence gallery)
                        (image-sequence gallery)))))

(defmethod find-sub-object ((identifier string) (gallery abstract-gallery) &optional type)
  (case type
    ((image)
     #1=(find identifier (image-sequence gallery) :key #'identifier :test #'string=))
    ((gallery)
     #2=(find identifier (gallery-sequence gallery) :key #'identifier :test #'string=))
    ((nil) (or #1# #2#))))

(defmethod find-sub-object ((index integer) (gallery abstract-gallery) &optional type)
  (case type
    ((nil image)
     (aref (image-sequence gallery) index))
    ((gallery)
     (aref (gallery-sequence gallery) index))))

(defmethod protected-p ((gallery gallery))
  (password gallery))

(defmethod protection-identifier ((sub-gallery sub-gallery))
  (aif (super-object sub-gallery) (protection-identifier it)))

(defmethod protected-p ((sub-gallery sub-gallery))
  (aif (super-object sub-gallery) (protected-p it)))

(create-standard-print-object abstract-gallery identifier)

(defvar *galleries* nil
  ;;(generate-galleries)
  )


;; implementation for the root-object
(defmethod super-object ((object (eql 'gallery-root))) nil)

(defmethod sub-objects ((object (eql 'gallery-root)) &optional type)
  (case type
    ((nil gallery) *galleries*)))

(defmethod find-sub-object ((identifier string) (object (eql 'gallery-root)) &optional type)
  (case type
    ((nil gallery)
     (find identifier *galleries* :key #'identifier :test #'string=))))

(defmethod title ((object (eql 'gallery-root)))
  "Image Galleries")

(defmethod identifier ((object (eql 'gallery-root)))
  "/simple-gallery")


(defmethod protected-p ((object (eql 'gallery-root))) nil)

;; todo move up

;;; statistics functions
(defun nr-of-images (gallery)
  (length (image-sequence gallery)))

(memodefun total-nr-of-images (gallery)
  (reduce #'+ (gallery-sequence gallery)
          :key #'total-nr-of-images
          :initial-value (nr-of-images gallery)))

(defmacro define-gallery-statistic (name reducer image-key initial-value)
  `(progn
     (memodefun ,name (gallery)
       (reduce #',reducer (image-sequence gallery)
               :key #',image-key
               :initial-value ,initial-value))
     (memodefun ,(symb 'total- name) (gallery)
       (reduce #',reducer (gallery-sequence gallery)
               :key #',(symb 'total- name)
               :initial-value (,name gallery)))))

(define-gallery-statistic compound-size + original-image-size 0)
(define-gallery-statistic oldest-image-date min datetime (get-universal-time))
(define-gallery-statistic newest-image-date max datetime 0)
