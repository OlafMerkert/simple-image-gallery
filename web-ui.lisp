(in-package :simple-image-gallery-web)

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

;; regular expressions and dispatchers
(defpar image-data-url-regex "/simple-gallery/data/([^/]+)/([^/]+)/([^/]+)\\.([^/.]+)")

(defpar gallery-overview-regex "^/simple-gallery/([^/?&]+)/?$")

(defpar gallery-slideshow-regex "^/simple-gallery/([^/?&]+)/([^/?&]+)/?$")

(pushnew (create-regex-dispatcher "^/simple-gallery/?$" 'gallery-list)
         *dispatch-table*)

(pushnew (create-regex-dispatcher image-data-url-regex 'image-data-provider)
         *dispatch-table*)

(pushnew (create-regex-dispatcher gallery-overview-regex 'gallery-overview)
         *dispatch-table*)

(pushnew (create-regex-dispatcher gallery-slideshow-regex 'gallery-slideshow)
         *dispatch-table*)

(pushnew (create-regex-dispatcher "^/simple-gallery/green-squares\\.png$"
                                  (lambda () (handle-static-file #P"/home/olaf/Projekte/simple-image-gallery/green-squares.png" )))
         *dispatch-table*)


(defmacro gallery-template ((&key title breadcrumb) &body body)
  `(html/document (:title ,title
                          :style "/simple-gallery/base.css")
     (:img :class "header-image" :src "/simple-gallery/green-squares.png")
     (:h1 (esc ,title))
     ,@body))

(defun render-breadcrumb (alist)
  (html/node
    (:div :class "breadcrumb"
          " [ "
          (let ((first t))
            (dolist (br alist)
              (if first
                  (setf first nil)
                  (htm " / "))
              (htm 
               (:a :href (car br) (esc (cdr br)))
               )))
          " ] ")))

(defpar top-breadcrumb '("/simple-gallery" . "All Galleries"))

(defun gallery-list ()
  (gallery-template (:title "Simple Image Gallery - Overview")
    (:ul
     (dolist (g sig:*galleries*)
       (htm (:li (:a :href (conc "/simple-gallery/" (sig:identifier g) "/")
                     (esc (sig:title g)))))))))

(defun image-data-provider ()
  (ppcre:register-groups-bind (size gal-id image-id)
      (image-data-url-regex (url-decode (script-name*)))
    (let ((size (assoc1 size image-data-sizes nil :test #'string-equal))
          (image (sig:find-image-by-identifiers gal-id image-id)))
      (if (not (and size image))
          (error-code)
          (handle-static-file (funcall size image))))))

(defun gallery-overview ()
  (ppcre:register-groups-bind (gal-id) (gallery-overview-regex (url-decode (script-name*)))
    (let ((gallery (sig:find-gallery-by-identifier gal-id)))
      (if (not gallery)
          (error-code)
          (gallery-template (:title (sig:title gallery))
            (render-breadcrumb (list top-breadcrumb))
            (:div :class "image-grid"
                  (map nil
                       (lambda (image)
                         (htm (:a :href (image-slideshow-url image)
                                  (:img :src (image-data-url image "thumbnail")))))
                       (sig:image-sequence gallery))))))))

(defun gallery-slideshow ()
  (ppcre:register-groups-bind (gal-id image-id)
      (gallery-slideshow-regex (url-decode (script-name*)))
    (let ((image (sig:find-image-by-identifiers gal-id image-id)))
      (if (not image)
          (error-code)
          (gallery-template (:title (conc (sig:title (sig:gallery image)) " - Image "
                                          (mkstr (+ 1 (sig:gallery-position image)))))
            (let ((gallery (sig:gallery image)))
              (render-breadcrumb (list top-breadcrumb (cons (conc "/simple-gallery/" (sig:identifier gallery))
                                                            (sig:title gallery)))))
            (:div :class "image-slideshow"
                  (:a :class "slideshow-motion"
                      :href (image-slideshow-url (sig:previous-image image))
                      "Previous")
                  "&nbsp;&nbsp;&nbsp;&nbsp;"
                  (:a :class "slideshow-motion"
                      :href (image-slideshow-url (sig:next-image image))
                      "Next")
                  (:br)
                  (:img :src (image-data-url image "slideshow"))
                  (:br)
                  (:a :href (image-data-url image "original") :target "_blank"
                      "Download original image")))))))

;;; CSS stylesheet
(pushnew (create-regex-dispatcher "^/simple-gallery/base\\.css$" 'simple-gallery-css)
         *dispatch-table*)

(defun simple-gallery-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css
    (("body") (:font-family "sans-serif" :font-size "11pt" :line-height "140%"
                            :color "#e8ede5"
                            :background-color "#47662a"
                            :padding-left "110px"))
    ((".header-image") (:display "block"
                                 :position "absolute"
                                 :top "5px"
                                 :left "5px"
                                 :z-index "-10"))
    (("h1") (:color "#d9ffb3"
                    :font-size "250%"
                    :margin "20pt"
                    :margin-top "30pt"))
    (("a:link," "a:visited")
     (:color "#ffffff"
             :text-decoration "underline"
             :font-style "normal"))
    (("a:hover," "a:active," "a:focus")
     (:color "#f1f285"
             :text-decoration "underline"
             :font-style "normal"))
    (("ul," "ol")
     (:line-height "200%"
                   :font-size "120%"
                   :margin-left "4em"))
    ((".image-grid")
     (:margin "2ex"
              ;;:border "1px solid white"
              ;;:padding "5pt"
              )
     (("img")
      (:margin "3pt"
               :border "1px solid white"
               :padding "2pt"
               )))
    ((".image-slideshow")
     ()
     (("img")
      (:margin "4pt"
               :margin-top "5ex"
               :border "1px solid white"
               :padding "3pt"
               :min-width "600px"
               :max-width "90%"
               :display "block"
               :text-align "center"
               :clear "both"))
     (("a.slideshow-motion")
      (:margin "1ex"
               :margin-left "3ex"
               :display "block"
               :float "left"
               :text-decoration "none"
               :border "1px solid white"
               :padding "3pt")))
    ((".breadcrumb")
     (:font-size "80%"
                 :margin "1ex"
                 :margin-left "10ex"
                 :margin-bottom "5ex"
                 :margin-top "-2ex"))))
