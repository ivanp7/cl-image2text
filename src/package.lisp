;;;; package.lisp

(defpackage #:traytr
  (:use #:cl)
  (:export :*port* :output-broadcast 
           :open-output-server :close-output-server :with-output-server
           :make-color :red :green :blue :make-cc :cc-char :cc-fg-color :cc-bg-color
           :shade-block-cc :down-block-cc :up-block-cc :left-block-cc :right-block-cc
           :quadrant-block-cc :inv-quadrant-block-cc
           :point :point-x :point-y :*screen-size* :make-text-buffer :text-buffer-element
           :ansi-cc-string :ansi-set-position :ansi-shift-position))

