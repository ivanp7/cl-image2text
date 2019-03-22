;;;; package.lisp

(defpackage #:traytr
  (:use #:cl)
  (:export :*port* :*io-server-function* :server-running-p :read-key :define-io-server-function 
           :start-io-server :stop-io-server :with-io-server
           :create-color-buffer :with-color-buffer-size 
           :color-buffer-element-color :with-color-buffer-element-colors
           :create-terminal-buffer :with-terminal-buffer-size :tb-size-x :tb-size-y 
           :with-terminal-buffer-element :char :fg-red :fg-green :fg-blue :bg-red :bg-green :bg-blue
           :+horz-ppc+ :+vert-ppc+ :convert-image-to-text
           :initialize-terminal :finalize-terminal :clear-terminal
           :*color-change-tolerance* :write-terminal-buffer
           :*application-function* :*application-postfunction*))

