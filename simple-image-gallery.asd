(defsystem simple-image-gallery
  :depends-on (ol-utils
               cl-fad
               iterate
               hunchentoot
               cl-who
               web-utils
               css-lite)
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "images")
                 (:file "galleries")
                 (:file "web-ui")))
