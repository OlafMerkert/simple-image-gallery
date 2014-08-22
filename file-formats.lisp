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
  (reduce #'max (image-sequence gallery) :key #'datetime :initial-value 0))

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
;; todo support for nested galleries

(defun image-from-path (pathname gallery &optional (position 0))
  (make-instance 'image :original-path pathname
                 :gallery gallery
                 :gallery-position position
                 :identifier (pathname-name pathname)))

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
