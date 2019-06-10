;;;; package.lisp

(defpackage #:cl-image2text
  (:use #:cl)
  (:export :create-pixel-buffer :with-pixel-buffer-size 
           :pixel-buffer-color :with-pixel-buffer-colors
           :create-terminal-buffer :with-terminal-buffer-size
           :with-terminal-buffer-element :char :fg-red :fg-green :fg-blue 
           :bg-red :bg-green :bg-blue
           :define-conversion +horz-ppc+ +vert-ppc+ +ppc+ 
           :convert-pixels/single-thread :convert-pixels :convert-image-to-text
           :read-image :*color-change-tolerance* :write-text))

