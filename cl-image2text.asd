;;;; cl-image2text.asd

(asdf:defsystem #:cl-image2text
  :description "Image-to-text converter"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria #:bordeaux-threads #:cl-cpus #:opticl)
  :components ((:file "package") 
               (:file "buffers" :depends-on ("package"))
               (:file "algorithm" :depends-on ("buffers"))
               (:file "io" :depends-on ("buffers"))))

