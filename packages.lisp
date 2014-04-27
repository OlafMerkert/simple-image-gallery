(in-package :ol-user)

(defpackage :simple-image-gallery
  (:nicknames :sig)
  (:use :cl :ol)
  (:export
   #:*galleries*
   #:gallery
   #:identifier
   #:title
   #:description
   #:last-updated
   #:image
   #:original-path
   #:thumbnail-path
   #:slideshow-path
   #:gallery-position
   #:thumbnail-size
   #:slideshow-size
   #:find-gallery-by-identifier
   #:image-sequence
   #:find-image-by-identifiers))

(defpackage :simple-image-gallery-web
  (:nicknames :sig-web)
  (:use :cl :ol
        :web-utils
        :cl-who
        :hunchentoot)
  (:export))

