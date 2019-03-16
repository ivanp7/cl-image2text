;;;; package.lisp

(defpackage #:traytr
  (:use #:cl)
  (:export :*port* :*io-server-function* :server-running-p
           :start-io-server :stop-io-server :with-io-server
           :create-color-buffer :with-color-buffer-size 
           :color-buffer-element-color :with-color-buffer-element-colors
           :create-terminal-buffer :with-terminal-buffer-size :with-terminal-buffer-element
           :+horz-ppc+ :+vert-ppc+ :convert-pixel-to-terminal-buffer
           :initialize-terminal :finalize-terminal :define-io-server-function 
           :write-terminal-buffer))

