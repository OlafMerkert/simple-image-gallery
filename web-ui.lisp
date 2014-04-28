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

(defun gallery-list ()
  (html/document (:title #1="Simple Image Gallery - Overview")
    (:h1 #1#)
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
          (html/document (:title #1=(conc "Simple Gallery - " (sig:title gallery)))
            (:h1 (esc #1#))
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
          (html/document (:title #1=(conc "Simple Gallery - "
                                          (sig:title (sig:gallery image)) " - Image Nr "
                                          (mkstr (+ 1 (sig:gallery-position image)))))
            (:h1 (esc #1#))
            (:div :class "image-slideshow"
                  (:a :class "slideshow-motion"
                      :href (image-slideshow-url (sig:previous-image image))
                      "Previous")
                  "&nbsp;&nbsp;&nbsp;&nbsp;"
                  (:a :class "slideshow-motion"
                      :href (image-slideshow-url (sig:next-image image))
                      "Next")
                  (:br)
                  (:img :src (image-data-url image "slideshow"))))))))
