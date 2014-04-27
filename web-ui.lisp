(in-package :simple-image-gallery-web)

(defun gallery-list ()
  (html/document (:title #1="Simple Image Gallery - Overview")
    (:h1 #1#)
    (:ul
     (dolist (g sig:*galleries*)
       (htm (:li (:a :href (conc "/simple-gallery/" (sig:identifier g) "/")
                     (esc (sig:title g)))))))))

(pushnew (create-regex-dispatcher "^/simple-gallery/?$" 'gallery-list)
         *dispatch-table*)

(defpar gallery-overview-regex "^/simple-gallery/([^/?&]+)/?$")

(pushnew (create-regex-dispatcher gallery-overview-regex 'gallery-overview)
         *dispatch-table*)

(defmethod image-data-url ((image sig:image) size)
  (format nil "/simple-gallery/data/~A/~A/~A.jpg" size
          (sig:identifier (sig:gallery image))
          (sig:identifier image)))

(defpar image-data-sizes '(("original" . sig:original-path)
                           ("slideshow" . sig:slideshow-path)
                           ("thumbnail" . sig:thumbnail-path)))


(defpar image-data-url-regex "/simple-gallery/data/([^/]+)/([^/]+)/([^/]+)\\.([^/.]+)")

(defun image-data-provider ()
  (mvbind (whole registers) (ppcre:scan-to-strings image-data-url-regex (url-decode (script-name*)))
    (if (not whole)
        #2=(error-code)
        (let* ((gal-id (aref registers 1))
               (image-id (aref registers 2))
               (size (assoc1 (aref registers 0) image-data-sizes nil :test #'string-equal))
               (image (sig:find-image-by-identifiers gal-id image-id)))
          (dbug "gal-id: ~S, image-id: ~S, raw-size: ~S, size: ~S, image: ~S" gal-id image-id (aref registers 0) size image)
          (if (not (and size image))
              #2#
              (handle-static-file (funcall size image)))))))

(pushnew (create-regex-dispatcher image-data-url-regex 'image-data-provider)
         *dispatch-table*)


(defun gallery-overview ()
  (mvbind (whole registers) (ppcre:scan-to-strings gallery-overview-regex (url-decode (script-name*)))
    (dbug "whole: ~S, registers: ~S" whole registers)
    (if (not whole)
        #2=(error-code)
        (let* ((gal-id (elt registers 0))
               (gallery (sig:find-gallery-by-identifier gal-id)))
          (if (not gallery)
              #2#
              (html/document (:title #1=(conc "Simple Gallery - " (sig:title gallery)))
                (:h1 (esc #1#))
                (:div :class "image-grid"
                      (map nil
                           (lambda (image)
                             (htm (:a :href (image-slideshow-url image)
                                      (:img :src (image-data-url image "thumbnail")))))
                           (sig:image-sequence gallery)))))))))

(defpar gallery-slideshow-regex "^/simple-gallery/([^/?&]+)/([^/?&]+)/?$")

(pushnew (create-regex-dispatcher gallery-slideshow-regex 'gallery-slideshow)
         *dispatch-table*)

(defmethod image-slideshow-url ((image sig:image))
  (conc "/simple-gallery/" (sig:identifier (sig:gallery image)) "/" (sig:identifier image)))


(defun gallery-slideshow ()
  (mvbind (whole registers) (ppcre:scan-to-strings gallery-slideshow-regex (url-decode (script-name*)))
    (if (not whole)
        #2=(error-code)
        (let* ((gal-id (elt registers 0))
               (image-id (elt registers 1))
               (image (sig:find-image-by-identifiers gal-id image-id)))
          (if (not image)
              #2#
              (html/document (:title #1=(conc "Simple Gallery - "
                                              (sig:title (sig:gallery image)) "- Image Nr "
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
                      (:img :src (image-data-url image "slideshow")))))))))
