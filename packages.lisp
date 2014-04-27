(defpackage :simple-image-gallery
  (:nicknames :sig)
  (:use :cl :ol)
  (:export
   #:*galleries*
   #:gallery
   #:identifier
   #:title
   #:description
   #:last-updated))

(in-package :simple-image-gallery)


