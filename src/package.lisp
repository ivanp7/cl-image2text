;;;; package.lisp

(defpackage #:traytr
  (:use #:cl)
  (:export :*port* :*io-server-function* :start-io-server :stop-io-server :with-io-server
           :make-color :red :green :blue :make-cc :chr :fg-color :bg-color
           :shade-block-cc :down-block-cc :up-block-cc :left-block-cc :right-block-cc
           :quadrant-block-cc :inv-quadrant-block-cc :quadrant-block-pair-cc
           :point :point-x :point-y :make-cc-buffer :cc-buffer-element
           :cc-to-string :cc-buffer-to-string))

