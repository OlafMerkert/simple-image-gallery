(in-package :simple-image-gallery)

(defpar gallery-folders '(#P "/home/olaf/data/galleries/")
  "Define a list of locations where we look for galleries.")

(defpar image-cache-dir #P "/home/olaf/data/image-cache/")

(defun gallery-pathnames ()
  "Search for all directories in the gallery-folders"
  (mapcan (lambda (dir)
            (remove-if-not #'fad:directory-pathname-p (fad:list-directory dir)))
          gallery-folders))

(define-condition missing-gallery-file (warning)
  ((gallery-path :initarg :gallery-path
                 :initform nil
                 :reader gallery-path)))

(defun gallery-compute-date (gallery)
  (max (reduce #'max (image-sequence gallery) :key #'datetime :initial-value 0)
       (reduce #'max (gallery-sequence gallery) :key #'last-updated :initial-value 0)))

(defun gallery-from-path (gallery-path)
  "Look at the special file in the given `path', and create objects
for the gallery and the images it contains."
  (labels ((setup-sub-objects (gallery galleries images)
             (setf (slot-value gallery 'gallery-sequence)
                   (map 'vector (clambda (create-subgallery gallery (second x!) (nthcdr 2 x!)))
                        galleries)
                   (slot-value gallery 'image-sequence)
                   (let ((index -1))
                     (map 'vector (clambda (image-from-path (merge-pathnames x! gallery-path)
                                                       gallery (incf index)))
                          images)))
             (setf (slot-value gallery 'last-updated)
                   (gallery-compute-date gallery))
             ;; compute the date
             gallery)
           (create-gallery (identifier sexp)
             (plist-bind (title description images galleries password) sexp
               (setup-sub-objects
                (make-instance 'gallery
                               :identifier identifier
                               :title title :description description
                               :password password)
                galleries images)))
           (create-subgallery (parent identifier sexp)
             (plist-bind (title description images galleries) sexp
               (setup-sub-objects
                (make-instance 'sub-gallery
                               :parent parent
                               :identifier identifier
                               :title title :description description)
                galleries images)))
           ;; images
           (image-from-path (pathname gallery &optional (position 0))
             (make-instance 'image :original-path pathname
                            :gallery gallery
                            :gallery-position position
                            :identifier (pathname-name pathname))))
    (let ((sexp-file (merge-pathnames "simple-gallery.sexp" gallery-path))
          (identifier (last1 (pathname-directory gallery-path))))
      (if (fad:file-exists-p sexp-file)
          (create-gallery identifier (rest (read-file-1 sexp-file))) ; gallery data
          (warn 'missing-gallery-file :gallery-path gallery-path)))))

(defun generate-galleries ()
  (sort 
   (filter #'gallery-from-path (gallery-pathnames))
   #'>= :key #'last-updated))

(defun update-galleries ()
  (setf *galleries* (generate-galleries)))

(defpar daily (clon:make-typed-cron-schedule :day-of-month '*))

#+sbcl
(defun schedule-update-galleries ()
  (clon:schedule-function 'update-galleries
                          (clon:make-scheduler daily :allow-now-p t)
                          :name "Simple Gallery scan for galleries"
                          :thread t))
