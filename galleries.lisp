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

(create-standard-print-object gallery identifier)

(defpar gallery-folders '(#P "/home/olaf/data/galleries/")
  "Define a list of locations where we look for galleries.")

(defpar image-cache-dir #P "/home/olaf/data/image-cache/")

(defun gallery-pathnames ()
  "Search for all directories in the gallery-folders"
  (mapcan (lambda (dir)
            (remove-if-not #'fad:directory-pathname-p (fad:list-directory dir)))
          gallery-folders))

;; todo move to ol-utils
(defun read-file-1 (pathname)
  "Read the first sexp in the given file."
  (with-open-file (stream pathname :if-does-not-exist :error)
    (read stream)))

(defmacro! plist-bind (bindings o!plist &body body)
  `(let ,(mapcar #`(,a1 (getf ,g!plist ,(keyw a1))) bindings)
     ,@body))

(defun gallery-from-path (gallery-path)
  "Look at the special file in the given `path', and create objects
for the gallery and the images it contains."
  (let ((sexp-file (merge-pathnames "simple-gallery.sexp" gallery-path)))
    (when (fad:file-exists-p sexp-file)
      (plist-bind (title description last-updated images) (rest (read-file-1 sexp-file))
        (aprog1
            (make-instance 'gallery
                           :identifier (last1 (pathname-directory gallery-path))
                           :title title :description description :last-updated last-updated)
          (setf (slot-value it 'image-sequence)
                (let ((index -1))
                  (map 'vector (clambda (image-from-path (merge-pathnames x! gallery-path)
                                                    it (incf index)))
                       images)))))))  )

(defun generate-galleries ()
  (filter #'gallery-from-path (gallery-pathnames)))

(defvar *galleries*
  (generate-galleries))

(defun update-galleries ()
  (setf *galleries* (generate-galleries)))

(update-galleries)

(defun find-gallery-by-identifier (identifier)
  (find identifier *galleries* :key #'identifier :test #'string=))

(defun find-image-by-identifiers (gallery-identifier image-identifier)
  (aif (find-gallery-by-identifier gallery-identifier)
       (find image-identifier (image-sequence it) :key #'identifier :test #'string=)))


