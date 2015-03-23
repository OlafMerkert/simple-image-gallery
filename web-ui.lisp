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

(pushnew (create-regex-dispatcher "^/scripts/image-slideshow\\.js$"
                                  (lambda () (handle-static-file #P"/home/olaf/Projekte/simple-image-gallery/image-slideshow.js" )))
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
                        ;; :script "/scripts/image-slideshow.js"
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

(defmethod acquire-authorisation ((gallery sig:gallery) (password string))
  (gallery-login password "This gallery is protected by a password."))

(defun present-galleries (galleries)
  (html/node
    (:ul :class "bigger list-unstyled"
       (map nil
            (lambda (g)
              (htm (:li
                      (:span :class "glyphicon glyphicon-picture icon-space-right") 
                      (:a :href (object-url g)
                         (esc (sig:title g)))
                      ;; (:span :class "datetime" " (" (str (fmt-universal-time (sig:last-updated g))) ") ")
                      (when (and (sig:protected-p g)
                                 (not (authorised-p g)))
                        (htm " "
                             (:span :class "glyphicon glyphicon-lock")
                             (:span :class "sr-only" "(password required)") " ")))))
            galleries))))

(defmethod present-object ((object (eql 'sig:gallery-root)))
  "Display a list of all known galleries"
  (present-galleries (sig:sub-objects object)))

(defun unit (number &optional (unit 'px))
  (format nil "~A~(~A~)" number unit))

(defmacro! sgs-insert-fields (keyw form)
  `(mapcar (lambda (,g!column)
             (let ((it (getf ,g!column ,keyw)))
               ,form))
           columns))

(defun show-gallery-statistics (gallery)
  (with-gensyms!
    (macrolet
        ((stat-table (columns)
           `(html/node
              ;; show statistics about the gallery
              (:table :class "table table-striped statistics-table"
                 (:thead
                    (:tr (:th "Quantity")
                       ,@(sgs-insert-fields :head
                                            `(:th :class "text-right" ,it))))
                 (:tbody
                    (:tr (:td "Number of images")
                       ,@ (sgs-insert-fields :nr-of-imgs
                                             `(:td :class "text-right" (str ,it))))
                    (:tr (:td "Total Image file size")
                       ,@(sgs-insert-fields :compound-size
                                            `(:td :class "text-right" (str (format-file-size ,it)))))
                    (:tr (:td "Oldest image date")
                       ,@ (sgs-insert-fields :oldest-image
                                             `(:td :class "text-right" (str (fmt-universal-time ,it)))))
                    (:tr (:td "Newest image date")
                       ,@ (sgs-insert-fields :newest-image
                                             `(:td :class "text-right" (str (fmt-universal-time ,it))))))))))
      (cond ((and #1=(< 0 (length (sig:image-sequence gallery)))
                  #2=(< 0 (length (sig:gallery-sequence gallery))))
             (stat-table (#3=(:head "in this gallery"
                                :nr-of-imgs (sig:nr-of-images gallery)
                                :compound-size (sig:compound-size gallery)
                                :oldest-image (sig:oldest-image-date gallery)
                                :newest-image  (sig:newest-image-date gallery))
                             #4=(:head "including subgalleries"
                                   :nr-of-imgs  (sig:total-nr-of-images gallery)
                                   :compound-size (sig:total-compound-size gallery)
                                   :oldest-image (sig:total-oldest-image-date gallery)
                                   :newest-image (sig:total-newest-image-date gallery)
                                   ))))
            (#1# (stat-table (#3#)))
            (#2# (stat-table ((:head "in subgalleries"
                                 :nr-of-imgs  (sig:total-nr-of-images gallery)
                                 :compound-size (sig:total-compound-size gallery)
                                 :oldest-image (sig:total-oldest-image-date gallery)
                                 :newest-image (sig:total-newest-image-date gallery)))))))))

(defmethod present-object ((gallery sig:abstract-gallery))
  "Display a grid of all images in a gallery"
  (html/node
    (:p :class "medskip-after"
       (esc (sig:description gallery)))
    ;; add a list of subgalleries (if there are any)
    (let ((galleries (sig:sub-objects gallery 'sig:gallery)))
      (unless (length=0 galleries)
        (present-galleries galleries)))
    ;; show a grid with all the images
    (let ((images (sig:sub-objects gallery 'sig:image)))
      (unless (length=0 images)
        (htm (:h3 "Images")
             (:div :class "image-grid"
                (map nil
                     (lambda (image)
                       (htm (:a :href (object-url image)
                               (:img :src (image-data-url image "thumbnail")
                                  :class "img-thumbnail"
                                  :style (aif (sig:image-dimensions (sig:thumbnail-path image))
                                              (css-lite:inline-css
                                               :width (unit (car it))
                                               :height (unit (cdr it)))
                                              "")))))
                     images)))))
    (show-gallery-statistics gallery)))

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
             "Download original image"))
       (:table :class "table table-striped" :style (css-lite:inline-css :font-size "80%"
                                                               :width "auto"
                                                               :margin-top "1em")
          (:thead (:tr (:th "Property") (:th "Value")))
          (:tbody
             (:tr (:td "Original file size")
                (:td (str (sig:format-file-size (sig:original-image-size image)))))
             (awhen (sig:foto-parameters image)
               (htm (:tr (:td "Focal length")
                       (:td (fmt "~F mm" (sig:foto-focal-length it))))
                    (:tr (:td "Aperture")
                       (:td (str (sig:foto-aperture it))))
                    (:tr (:td "Shutter speed")
                       (:td (str (sig:foto-shutter-speed it))))
                    (:tr (:td "ISO level")
                       (:td (str (sig:foto-iso it))))
                    (:tr (:td "Flash")
                       (:td (fmt "~:[no~;yes~]" (sig:foto-flash it)))))))
          ))))

(register-web-application "Simple Image Gallery" "/simple-gallery")
