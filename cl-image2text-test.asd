;;;; cl-image2text-test.asd

(asdf:defsystem #:cl-image2text-test
  :description "Image-to-text-converter tests"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-image2text)
  :components ((:file "t/package-test")
               (:file "t/terminal-test")))

