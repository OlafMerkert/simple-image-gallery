(defsystem simple-image-gallery
  :depends-on (ol-utils
               cl-fad
               iterate
               web-utils
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
                 (:file "web-authentification")
                 (:file "hierarchy")
                 (:file "images")
                 (:file "galleries")
                 (:file "file-formats")
                 (:file "web-ui")))
