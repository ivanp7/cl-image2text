;;;; traytr.asd

(asdf:defsystem #:traytr
  :description "Terminal ray tracer"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (#:usocket #:usocket-server #:bordeaux-threads #:alexandria)
  :components ((:file "src/package") 
               (:file "src/io-server" :depends-on ("src/package"))
               (:file "src/color-buffer" :depends-on ("src/package"))
               (:file "src/terminal-buffer" :depends-on ("src/color-buffer"))))

