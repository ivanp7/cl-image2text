;;;; traytr.asd

(asdf:defsystem #:traytr
  :description "Describe traytr here"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-charms)
  :components ((:file "package")
               (:file "traytr")))
