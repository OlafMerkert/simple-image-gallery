(defsystem simple-image-gallery
  :depends-on (ol-utils
               cl-fad
               iterate)
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "images")
                 (:file "galleries")))
