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

(defun filter-by-filetype (type pathnames)
  "Filter a sequence of `pathnames' to those have given `type'."
  (remove type pathnames :key #'pathname-type :test-not #'string-equal))


(defun resolve-images (gallery-path images &key (type "jpg"))
  "Produce a sequence of pathnames from a list of `images' or a scan
command. Currently supported: `:auto' and `(:auto subpath)'."
  (labels ((resolve (subpath)
             (filter-by-filetype type (fad:list-directory
                                       (merge-pathnames subpath gallery-path)))))
    (cond ((and (atom images) (eq images :auto))
           (resolve ""))
          ((and (consp images) (eq (car images) :auto))
           (resolve (second images)))
          ((listp images)
           (map 'vector (clambda (merge-pathnames x! gallery-path)) images)))))

(defun gallery-from-path (gallery-path)
  "Look at the special file in the given `path', and create objects
for the gallery and the images it contains."
  (labels ((setup-sub-objects (gallery galleries images &key sort-date)
             (setf (slot-value gallery 'gallery-sequence)
                   (map 'vector (clambda (create-subgallery gallery (second x!) (nthcdr 2 x!)))
                        galleries))
             (let ((index -1)
                   (images (map 'vector (clambda (image-from-path x! gallery))
                                (resolve-images gallery-path images))))
               (when sort-date
                 (setf images (sort images #'<= :key #'datetime)))
               (map nil (lambda (image) (setf (slot-value image 'gallery-position)
                                          (incf index)))
                    images)
               (setf (slot-value gallery 'image-sequence) images))
             (setf (slot-value gallery 'last-updated)
                   (gallery-compute-date gallery))
             ;; compute the date
             gallery)
           (create-gallery (identifier sexp)
             (plist-bind (title description images galleries password sort-date) sexp
               (setup-sub-objects
                (make-instance 'gallery
                               :identifier identifier
                               :title title :description description
                               :password password)
                galleries images
                :sort-date sort-date)))
           (create-subgallery (parent identifier sexp)
             (plist-bind (title description images galleries password sort-date) sexp
               (setup-sub-objects
                (make-instance 'sub-gallery
                               :parent parent
                               :identifier identifier
                               :title title :description description
                               :password password)
                galleries images
                :sort-date sort-date)))
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
