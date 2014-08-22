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
               split-sequence)
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "hierarchy")
                 (:file "images")
                 (:file "galleries")
                 (:file "flie-formats")
                 (:file "web-ui")))
