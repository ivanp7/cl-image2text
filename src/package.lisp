;;;; package.lisp

(defpackage #:traytr
  (:use #:cl)
  (:export :*port* :*io-server-stop-flag* :*io-server-function* 
           :start-io-server :stop-io-server :with-io-server
           :make-buffer-pair :with-buffer-pair-element :set-buffer-pair-element
           :set-shade-block :set-down-block :set-up-block :set-left-block :set-right-block
           :set-quadrant-block :set-inv-quadrant-block :set-quadrant-block-pair
           :set-inv-quadrant-block-pair
           :+initial-string+ :+final-string+ :define-io-server-function/with-ansi
           :write-buffer-pair-element :write-buffer-pair))

