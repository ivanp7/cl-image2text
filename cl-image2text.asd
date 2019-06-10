;;;; cl-image2text.asd

(asdf:defsystem #:cl-image2text
  :description "Image-to-text converter"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria #:bordeaux-threads #:cl-cpus #:opticl)
  :components ((:static-file "README.md")
               (:static-file "lisplogo_fancy_256.png" 
                             :depends-on ("README.md"))
               (:static-file "LICENSE")
               (:static-file "roswell/image2text.ros")
               (:file "package") 
               (:file "buffers" :depends-on ("package"))
               (:file "io" :depends-on ("buffers"))
               (:file "constants" :depends-on ("package"))
               (:file "conversion" :depends-on ("constants"))
               (:file "algorithm" :depends-on ("conversion" "buffers"))))

