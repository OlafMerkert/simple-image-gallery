(in-package :simple-image-gallery-web)

(defgeneric present-object (object))

(defun object-url (object)
  (let ((hier (sig:object-hierarchy object)))
    (rutils.string:strjoin #\/
                           (mapcar #'sig:identifier hier))))

;;; generate urls for images
(defpar image-data-sizes '(("original" . sig:original-path)
                           ("slideshow" . sig:slideshow-path)
                           ("thumbnail" . sig:thumbnail-path)))

(defmethod image-data-url ((image sig:image) size)
  (let ((hier (sig:object-hierarchy image)))
    (format nil "/simple-gallery/data/~A/~A.jpg"
            size
            (rutils.string:strjoin #\/
                                   (mapcar #'sig:identifier (rest hier))))))

(pushnew (create-prefix-dispatcher "/simple-gallery" 'simple-gallery-dispatcher)
         *dispatch-table*)

(pushnew (create-regex-dispatcher "^/scripts/image-grid\\.js$"
                                  (lambda () (handle-static-file #P"/home/olaf/Projekte/simple-image-gallery/image-grid.js" )))
         *dispatch-table*)

(declaim (inline remove-file-ending parse-script-name))
(defun remove-file-ending (string &optional (max-length 4))
  (mvbind (match registers)
      (cl-ppcre:scan-to-strings
       `(:sequence (:register (:greedy-repetition 0 nil :everything))
                   "." (:greedy-repetition 1 ,max-length :word-char-class))
       string)
    (if match
        (aref registers 0)
        string)))

(defun parse-script-name (string)
  (rest (split-sequence #\/
                        (remove-file-ending (url-decode string))
                        :remove-empty-subseqs t)))

(defun simple-gallery-dispatcher ()
  (let* ((identifier-list (parse-script-name (script-name*))))
    ;; todo check if authorised
    (cond ((string-equal (first identifier-list) "data")
           (let ((image (sig:find-object (nthcdr 2 identifier-list)))
                 (size  (assoc1 (second identifier-list) image-data-sizes nil :test #'string-equal)))
             (if (and size image (typep image 'sig:image))
                 (with-protection/silent image
                   (handle-static-file (funcall size image)))
                 (error-code))))
          (t (let ((object (sig:find-object identifier-list)))
               (if object
                   (with-protection object
                     (present-object/full object))
                   (error-code)))))))

(defmacro gallery-template ((&key title breadcrumb) &body body)
  "Main html document layout for all webpages of the simple gallery."
  `(html/document+bs (:title (conc "Simple Image Gallery - ",title)
                        :script "/scripts/image-grid.js"
                        :style "/style/bootstrap-nonav.css")
     ;; todo might we have some use for a navbar -> new keyword
     ;; argument
     ,(when breadcrumb `(apply #'breadcrumbs ,breadcrumb))
     (bs-body
      (:h1 (esc ,title))
      ,@body)))

(defun hierarchy->breadcrumb (object)
  (iter (for o in (sig:object-hierarchy object))
        (collect (sig:identifier o) into parts)
        (collect (rutils:strjoin #\/ parts) into breadcrumbs)
        (collect (sig:title o) into breadcrumbs)
        (finally (return breadcrumbs))))

(defun present-object/full (object)
  (gallery-template (:title (sig:title object)
                       :breadcrumb (hierarchy->breadcrumb object))
    (present-object object)))


(defun fmt-universal-time (time)
  "Convert a timestamp into a pretty string."
  (local-time:format-timestring nil (local-time:universal-to-timestamp time)
                                :format '(:short-weekday " " :short-month " " :day ". " :year ", " :hour  ":" (:min 2))))

(defun gallery-login (password message)
  "Display and process a password prompt form. Return `:success' if
the user supplies `password'. For customisation, we show `message' at
the top of the form."
  (flet ((password-form (&key wrong-password)
           (gallery-template (:title "Password required"
                                :breadcrumb (hierarchy->breadcrumb 'sig:gallery-root))
             (:p :class "user-info bg-info text-info" (esc message))
             (:form :role "form"
                :method "post" :action ""
                    
                (:div :class (if wrong-password "form-group has-error" "form-group")
                   (:label :for "password" "Gallery password:") " "
                   (:input :name "password" :type "password" :value ""
                      :class "form-control")
                   (when wrong-password
                     (htm (:span :class "help-block" "Wrong password - please try again."))))
                (:div :class "form-group"
                   (:button :type "submit"
                      :class "btn btn-default"
                      "Login"))))))
    (let ((password-param (post-parameter "password")))
      (if (and (stringp password-param) (not (length=0 password-param)))
          (if (string= password-param password)
              :success
              (password-form :wrong-password t))
          (password-form)))))

(defun authorised-p (object)
  "Check if the user has already supplied the password for `object'."
  (let ((session (start-session)))
    (aand (session-value 'simple-image-gallery-authorisation session)
          (gethash (sig:protection-identifier object) it))))

(defun authorise (object)
  "Mark `object' as free to access for current user."
  (let* ((session (start-session))
         (table #1=(session-value 'simple-image-gallery-authorisation session)))
    (unless table
      (setf table (make-hash-table)
            #1# table))
    (setf (gethash (sig:protection-identifier object) table) t)))

(defun with-protection% (object body-function)
  "Backing function for `with-protection'."
  (if (and (sig:protected-p object)
           (not (authorised-p object)))
      ;; produce the login form
      (let ((lf (gallery-login (sig:protected-p object) "This gallery is protected by a password.")))
        (if (eq lf :success)
            (progn
              (authorise object)
              (funcall body-function))
            lf))
      ;; provide the page content
      (funcall body-function)))

(defmacro! with-protection (object &body body)
  "Helper macro, which causes a password prompt to be shown if the
`object' is protected."
  `(with-protection% ,object (lambda () ,@body)))

(defmacro! with-protection/silent (o!object &body body)
  "Helper macro, which denies access to `object' if it is protected
and no yet authorised."
  `(if (or (not (sig:protected-p ,g!object))
           (authorised-p ,g!object))
       (progn ,@body)
       (error-code hunchentoot:+http-forbidden+)))

(defun present-galleries (galleries)
  (html/node
    (:ul :class "bigger list-unstyled"
       (dolist (g galleries)
         (htm (:li
                 (:span :class "glyphicon glyphicon-picture"
                    :style (inline-css :margin-right "1em")) 
                 (:a :href (object-url g)
                    (esc (sig:title g)))
                 (:span :class "datetime" " (" (str (fmt-universal-time (sig:last-updated g))) ") ")
                 (when (sig:protected-p g)
                   (htm " " (:span :class "protected" "(password required)") " "))))))))

(defmethod present-object ((object (eql 'sig:gallery-root)))
  "Display a list of all known galleries"
  (present-galleries (sig:sub-objects object)))

(defun unit (number &optional (unit 'px))
  (format nil "~A~(~A~)" number unit))

(defmethod present-object ((gallery sig:abstract-gallery))
  "Display a grid of all images in a gallery"
  (html/node
    (:p :style (inline-css :margin-bottom "1em")
       (esc (sig:description gallery)))
    ;; add a list of subgalleries (if there are any)
    (let ((galleries (sig:sub-objects gallery 'sig:gallery)))
      (unless (length=0 galleries)
        (present-galleries galleries)))
    ;; show a grid with all the images
    (:div :class "image-grid"
       (map nil
            (lambda (image)
              (htm (:a :href (object-url image)
                      (:img :src (image-data-url image "thumbnail")
                         :class "img-thumbnail"
                         :style (aif (sig:image-dimensions (sig:thumbnail-path image))
                                     (inline-css :width (unit (car it))
                                                 :height (unit (cdr it)))                                               
                                     "")))))
            (sig:sub-objects gallery 'sig:image)))))

(defmethod present-object ((image sig:image))
  "Display a single image of a gallery"
  (html/node
    (:div :class "image-slideshow"
       (:div :class "form-group"
          (:div :class "btn-group"
             (:a :class "slideshow-motion btn btn-default"
                :href (object-url (sig:previous-image image))
                "Previous")
             (:a :class "slideshow-motion btn btn-default"
                :href (object-url (sig:next-image image))
                "Next"))
          "&nbsp;" "&nbsp;"
          (:span :class "datetime btn btn-default text-right" :disabled "disabled"
             (str (fmt-universal-time (sig:datetime image)))))
       (:a :href (object-url (sig:gallery image))
          (:img :class "img-responsive img-thumbnail" :src (image-data-url image "slideshow")))
       
       (:p :class "text-center"
          (:a :href (image-data-url image "original") :target "_blank"
             "Download original image")
          " [ " (esc (sig:format-file-size (sig:original-image-size image))) " ] "))))
