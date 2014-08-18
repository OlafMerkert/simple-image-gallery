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
   original-image-size
   (gallery :initarg :gallery
            :initform nil
            :reader gallery)
   (gallery-position :initarg :gallery-position
                     :initform 0
                     :reader gallery-position)
   (datetime :initform nil
         :reader datetime))
  (:documentation "Keep track of filename and file locations. Later
  perhaps also cache EXIF metadata."))

(defmethod protected-p ((image image))
  (protected-p (gallery image)))

(defmethod protection-identifier ((image image))
  (gallery image))

(create-standard-print-object image identifier)

(defpar thumbnail-size 128)

(defpar slideshow-size 1024)

(bind-multi ((thumbnail-path thumbnail-path slideshow-path)
             (thumbnail-size thumbnail-size slideshow-size))
  (defmethod thumbnail-path ((image image))
    "automatically test if the thumbnail exists and generate it if
  necessary"
    (with-slots (original-path thumbnail-path) image
      ;; ensure we have a valid path
      (unless thumbnail-path
        (setf thumbnail-path (generate-cache-pathname image 'thumbnail-size)))
      ;; ensure the scaled version was already generated
      (unless (fad:file-exists-p thumbnail-path)
        (ensure-directories-exist thumbnail-path)
        (im-convert original-path thumbnail-path thumbnail-size))
      thumbnail-path)))

(defun image-from-path (pathname gallery &optional (position 0))
  (make-instance 'image :original-path pathname
                 :gallery gallery
                 :gallery-position position
                 :identifier (pathname-name pathname)))

;;; downsizing images with imagemagick
(defun im-convert (source target size)
  (sb-ext:run-program "/usr/bin/convert"
                      (list (mkstr source)
                            ;; respect orientation, which we maybe
                            ;; need to pull from EXIF data
                            "-auto-orient"
                            "-resize"
                            (format nil "~Ax~A" size size)
                            (mkstr target))
                      ;; no point going to background, anyway we
                      ;; usually only access this during a http request
                      :wait t))

(defpar im-type "jpg")

(defmethod generate-cache-pathname ((image image) suffix)
  (merge-pathnames
   (make-pathname :name (identifier image)
                  :type im-type
                  :directory (list :relative
                                   (identifier (gallery image))
                                   (mkstr suffix)))
   image-cache-dir))

;; navigation the image stream
(defmethod next-image ((image image))
  (let* ((gallery (gallery image))
         (current (gallery-position image))
         (l (length (image-sequence gallery)))
         (next (mod (+ 1 current) l)))
    (elt (image-sequence gallery) next)))

(defmethod previous-image ((image image))
  (let* ((gallery (gallery image))
         (current (gallery-position image))
         (l (length (image-sequence gallery)))
         (previous (mod (- current 1) l)))
    (elt (image-sequence gallery) previous)))

;;; metadata about the images
(defmethod image-name ((image image))
  (mkstr (identifier image)))

(defmethod original-image-size ((image image))
  (if (slot-boundp image 'original-image-size)
      #1=(slot-value image 'original-image-size)
      (setf #1# (ql-util:file-size (original-path image)))))

(memodefun image-dimensions% (pathname)
  (cl-gd:with-image-from-file* (pathname)
    (cons (cl-gd:image-width)
          (cl-gd:image-height))))

(defun image-dimensions (pathname)
  (when (fad:file-exists-p pathname)
    (image-dimensions% pathname)))

(defstruct (foto-parameters (:conc-name foto-))
  focal-length
  aperture
  shutter-speed
  iso
  flash)

(defun read-exif-data (image)
  "Extract the date from EXIF data if available, otherwise use the
`file-write-date'."
  (handler-case
      (let ((exif (zpb-exif:make-exif (original-path image))))
        (setf (slot-value image 'datetime)
              (zpb-exif:parsed-exif-value "DateTime" exif)))
    (zpb-exif:invalid-stream ()))
  (when (or (not (slot-boundp image 'datetime))
            (not (slot-value image 'datetime)))
    (setf (slot-value image 'datetime)
          (file-write-date (original-path image)))))

(defmethod initialize-instance :after ((image image) &key)
  (read-exif-data image))

