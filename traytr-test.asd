;;;; traytr-test.asd

(asdf:defsystem #:traytr-test
  :description "Terminal ray tracer tests"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:traytr)
  :components ((:file "t/package-test")
               (:file "t/server-test")))

