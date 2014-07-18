(defsystem simple-image-gallery
  :depends-on (ol-utils
               cl-fad
               iterate
               web-utils
               css-lite
               clon
               cl-secure-read
               parenscript
               zpb-exif
               local-time
               cl-gd)
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "images")
                 (:file "galleries")
                 (:file "web-ui")))
