(in-package :simple-image-gallery-web)

(defgeneric present-object (object))

;;; generate urls for images
(defpar image-data-sizes '(("original" . sig:original-path)
                           ("slideshow" . sig:slideshow-path)
                           ("thumbnail" . sig:thumbnail-path)))

(defmethod image-data-url ((image sig:image) size)
  (format nil "/simple-gallery/data/~A/~A/~A.jpg" size
          (sig:identifier (sig:gallery image))
          (sig:identifier image)))

(defmethod image-slideshow-url ((image sig:image))
  (conc "/simple-gallery/" (sig:identifier (sig:gallery image)) "/" (sig:identifier image)))

(defmethod gallery-overview-url ((image sig:image))
  (gallery-overview-url (sig:gallery image)))

(defmethod gallery-overview-url ((gallery sig:gallery))
  (conc "/simple-gallery/" (sig:identifier gallery) "/"))


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
                 (handle-static-file (funcall size image))
                 (error-code))))
          (t (let ((object (sig:find-object identifier-list)))
               (if object
                   (present-object object)
                   (error-code)))))))


(defmacro gallery-template ((&key title breadcrumb) &body body)
  "Main html document layout for all webpages of the simple gallery."
  `(html/document+bs (:title ,title
                        :script "/scripts/image-grid.js"
                        :style "/style/bootstrap-nonav.css")
     ;; todo might we have some use for a navbar -> new keyword
     ;; argument
     ,(when breadcrumb `(render-breadcrumb ,breadcrumb))
     (bs-body
      (:h1 (esc ,title))
      ,@body)))

(defun render-breadcrumb (alist)
  "Generate Hierarchy navigation, expect an list of `(url . title)'."
  (apply #'breadcrumbs (iter (for br in alist)
                             (collect (car br))
                             (collect (cdr br)))))

(defpar top-breadcrumb '(("/simple-gallery" . "Image Galleries")))

(defun fmt-universal-time (time)
  "Convert a timestamp into a pretty string."
  (local-time:format-timestring nil (local-time:universal-to-timestamp time)
                                :format '(:short-weekday " " :short-month " " :day ". " :year ", " :hour  ":" (:min 2))))

(defun gallery-login (password message)
  "Display and process a password prompt form. Return `:success' if
the user supplies `password'. For customisation, we show `message' at
the top of the form."
  (flet ((password-form (&key wrong-password)
           (gallery-template (:title "Simple Image Gallery - Login"
                                :breadcrumb top-breadcrumb)
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
       ,@body
       (error-code hunchentoot:+http-forbidden+)))

(defmethod present-object ((object (eql 'sig:gallery-root)))
  "Display a list of all known galleries"
  (gallery-template (:title "Simple Image Gallery - Overview"
                       :breadcrumb (list (cons "#" (cdar top-breadcrumb))))
    (:ul :class "bigger list-unstyled"
       (dolist (g sig:*galleries*)
         (htm (:li
                 (:span :class "glyphicon glyphicon-picture"
                    :style (inline-css :margin-right "1em")) 
                 (:a :href (gallery-overview-url g)
                    (esc (sig:title g)))
                 (:span :class "datetime" " (" (str (fmt-universal-time (sig:last-updated g))) ") ")
                 (when (sig:protected-p g)
                   (htm " " (:span :class "protected" "(password required)") " "))))))))

;; todo make compatible to hierarchy stuff
(defun image-data-provider ()
  "Serve the actual image files, of the various sizes defined in `image-data-sizes'."
  (ppcre:register-groups-bind (size gal-id image-id)
      (image-data-url-regex (url-decode (script-name*)))
    (let ((size (assoc1 size image-data-sizes nil :test #'string-equal))
          (image (sig:find-image-by-identifiers gal-id image-id)))
      (if (not (and size image))
          (error-code)
          (with-protection/silent image
            (handle-static-file (funcall size image)))))))

(defun unit (number &optional (unit 'px))
  (format nil "~A~(~A~)" number unit))

(defmethod present-object ((gallery sig:gallery))
  "Display a grid of all images in a gallery"
  (if (not gallery)
      (error-code)
      (with-protection gallery
        (gallery-template (:title (sig:title gallery)
                             :breadcrumb (append1 top-breadcrumb
                                                  (cons "#" (sig:title gallery))))
          (:p :style (inline-css :margin-bottom "1em")
             (esc (sig:description gallery)))
          (:div :class "image-grid"
             (map nil
                  (lambda (image)
                    (htm (:a :href (image-slideshow-url image)
                            (:img :src (image-data-url image "thumbnail")
                               :class "img-thumbnail"
                               :style (aif (sig:image-dimensions (sig:thumbnail-path image))
                                           (inline-css :width (unit (car it))
                                                       :height (unit (cdr it)))                                               
                                           "")))))
                  (sig:image-sequence gallery)))))))

(defmethod present-object ((image sig:image))
  "Display a single image of a gallery"
  (let* ((gallery (if image (sig:gallery image))))
    (if (not image)
        (error-code)
        (with-protection image
          (gallery-template
              (:title (conc (sig:title gallery) " - Image "
                            (mkstr (+ 1 (sig:gallery-position image))))
                 :breadcrumb (append top-breadcrumb
                                     (list (cons (conc "/simple-gallery/" (sig:identifier gallery)) (sig:title gallery))
                                           (cons "#" (sig:image-name image))))) 
            (:div :class "image-slideshow"
               (:div :class "form-group"
                  (:a :class "slideshow-motion btn btn-default"
                     :href (image-slideshow-url (sig:previous-image image))
                     "Previous")
                  "&nbsp;"
                  (:a :class "slideshow-motion btn btn-default"
                     :href (image-slideshow-url (sig:next-image image))
                     "Next")
                  "&nbsp;" "&nbsp;"
                  (:span :class "datetime btn btn-default text-right" :disabled "disabled"
                     (str (fmt-universal-time (sig:datetime image)))))
               (:a :href (gallery-overview-url image)
                  (:img :class "img-responsive img-thumbnail" :src (image-data-url image "slideshow")))
               
               (:p :class "text-center"
                  (:a :href (image-data-url image "original") :target "_blank"
                     "Download original image")
                  " [ " (esc (sig:format-file-size (sig:original-image-size image))) " ] ")))))))
