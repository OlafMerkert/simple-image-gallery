(in-package :simple-image-gallery)

(defclass image ()
  ((identifier :initarg :identifier
               :initform ""
               :reader identifier)
   (original-path :initarg :original-path
                  :initform nil
                  :reader original-path)
   (thumbnail-path :initarg :thumbnail-path
                   :initform nil)
   (slideshow-path :initarg :slideshow-path
                   :initform nil)
   (gallery :initarg :gallery
         :initform nil
         :reader gallery)
   (gallery-position :initarg :gallery-position
                     :initform 0
                     :reader gallery-position))
  (:documentation "Keep track of filename and file locations. Later
  perhaps also cache EXIF metadata."))

(create-standard-print-object image identifier)

(defpar thumbnail-size 128)

(defpar slideshow-size 1280)

(defmethod thumbnail-path ((image image))
  ;; todo automatically test if the thumbnail exists and generate it
  ;; if necessary
  (with-slots (thumbnail-path) image
    (or thumbnail-path
        
        ))
  )

(defmethod slideshow-path ((image image))
  ;; todo ditto
  )

(defun image-from-path (pathname gallery &optional (position 0))
  (make-instance 'image :original-path pathname
                 :gallery gallery
                 :gallery-position position
                 :identifier (pathname-name pathname)))

