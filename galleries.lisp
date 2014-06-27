(in-package :simple-image-gallery)

(defclass gallery ()
  ((identifier :initarg :identifier
               :initform ""
               :reader identifier)
   (title :initarg :title
          :initform ""
          :reader title)
   (description :initarg :description
                :initform ""
                :reader description)
   (password :initarg :password
             :initform nil
             :reader password)
   (last-updated :initarg :last-updated
                 :initform nil
                 :reader last-updated)
   (image-sequence :initarg :image-sequence
                   :initform #()
                   :reader image-sequence))
  (:documentation "A gallery has just a title, description and
  describes a sequence of images. There may also be some date
  information, and we use the folder name as identifier (this also
  allows to reconstruct the path easily)."))

(defmethod protected-p ((gallery gallery))
  (password gallery))

(defmethod protection-identifier ((gallery gallery))
  gallery)

(create-standard-print-object gallery identifier)

(defpar gallery-folders '(#P "/home/olaf/data/galleries/")
  "Define a list of locations where we look for galleries.")

(defpar image-cache-dir #P "/home/olaf/data/image-cache/")

(defun gallery-pathnames ()
  "Search for all directories in the gallery-folders"
  (mapcan (lambda (dir)
            (remove-if-not #'fad:directory-pathname-p (fad:list-directory dir)))
          gallery-folders))

(defmacro! plist-bind (bindings o!plist &body body)
  `(let ,(mapcar #`(,a1 (getf ,g!plist ,(keyw a1))) bindings)
     ,@body))

(define-condition missing-gallery-file (warning)
  ((gallery-path :initarg :gallery-path
                 :initform nil
                 :reader gallery-path)))

(defun gallery-from-path (gallery-path)
  "Look at the special file in the given `path', and create objects
for the gallery and the images it contains."
  (let ((sexp-file (merge-pathnames "simple-gallery.sexp" gallery-path)))
    (if (fad:file-exists-p sexp-file)
        (plist-bind (title description last-updated images password) (rest (read-file-1 sexp-file))
          (aprog1
              (make-instance 'gallery
                             :identifier (last1 (pathname-directory gallery-path))
                             :title title :description description :last-updated last-updated
                             :password password)
            (setf (slot-value it 'image-sequence)
                  (let ((index -1))
                    (map 'vector (clambda (image-from-path (merge-pathnames x! gallery-path)
                                                      it (incf index)))
                         images)))
            ;; compute the date
            (setf (slot-value it 'last-updated)
                  (gallery-compute-date it))))
        (warn 'missing-gallery-file :gallery-path gallery-path))))

(defun gallery-compute-date (gallery)
  (reduce #'max (image-sequence gallery) :key #'datetime :initial-value 0))

(defun generate-galleries ()
  (sort 
   (filter #'gallery-from-path (gallery-pathnames))
   #'>= :key #'last-updated))

(defvar *galleries* nil
  ;;(generate-galleries)
  )

(defun update-galleries ()
  (setf *galleries* (generate-galleries)))

(defpar daily (clon:make-typed-cron-schedule :day-of-month '*))

#+sbcl
(defun schedule-update-galleries ()
  (clon:schedule-function 'update-galleries
                          (clon:make-scheduler daily :allow-now-p t)
                          :name "Simple Gallery scan for galleries"
                          :thread t))


(defun find-gallery-by-identifier (identifier)
  (find identifier *galleries* :key #'identifier :test #'string=))

(defun find-image-by-identifiers (gallery-identifier image-identifier)
  (aif (find-gallery-by-identifier gallery-identifier)
       (find image-identifier (image-sequence it) :key #'identifier :test #'string=)))


