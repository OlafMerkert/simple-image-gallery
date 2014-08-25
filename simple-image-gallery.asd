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
               cl-gd
               split-sequence
               rutils)
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "hierarchy")
                 (:file "images")
                 (:file "galleries")
                 (:file "file-formats")
                 (:file "web-authentification")
                 (:file "web-ui")))
