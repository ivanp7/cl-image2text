;;;; cl-image2text.asd

(asdf:defsystem #:cl-image2text
  :description "Image-to-text converter"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (#:opticl #:cl-cpus #:bordeaux-threads #:alexandria)
  :components ((:file "src/package") 
               (:file "src/color-buffer" :depends-on ("src/package"))
               (:file "src/terminal-buffer" :depends-on ("src/color-buffer"))
               (:file "src/image" :depends-on ("src/terminal-buffer"))))

