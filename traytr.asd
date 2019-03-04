;;;; traytr.asd

(asdf:defsystem #:traytr
  :description "Terminal ray tracer"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-charms)
  :components ((:file "package")
               (:file "src/traytr")))

