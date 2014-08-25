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
   #:find-image-by-identifiers
   #:previous-image
   #:next-image
   #:update-galleries
   #:schedule-update-galleries
   #:missing-gallery-file
   #:original-image-size
   #:format-file-size
   #:datetime
   #:protected-p
   #:protection-identifier
   #:image-dimensions
   #:image-name
   #:gallery-root
   #:hierarchy-object
   #:super-object
   #:sub-objects
   #:find-sub-object
   #:find-object
   #:find-object-hierarchy
   #:object-hierarchy
   #:abstract-gallery
   #:nr-of-images
   #:compound-size
   #:total-size
   #:total-nr-of-images
   #:total-compound-size
   #:newest-image-date
   #:TOTAL-NEWEST-IMAGE-DATE
   #:TOTAL-OLDEST-IMAGE-DATE
   #:oldest-image-date))

(defpackage :simple-image-gallery-web
  (:nicknames :sig-web)
  (:use :cl :ol :iterate
        :web-utils
        :cl-who :bootstrap
        :hunchentoot
        :css-lite
        :split-sequence)
  (:export
   #:object-hierarchy))

