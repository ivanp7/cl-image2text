;;;; traytr.asd

(asdf:defsystem #:traytr
  :description "Terminal ray tracer"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:usocket #:usocket-server #:bordeaux-threads #:alexandria)
  :components ((:file "src/package")
               (:file "src/io-server")
               (:file "src/char-buffer")
               (:file "src/ansi")))

