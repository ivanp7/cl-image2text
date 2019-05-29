;;;; package.lisp

(defpackage #:cl-image2text
  (:use #:cl)
  (:export :create-color-buffer :with-color-buffer-size 
           :color-buffer-element-color :with-color-buffer-element-colors
           :create-terminal-buffer :with-terminal-buffer-size :tb-size-x :tb-size-y 
           :with-terminal-buffer-element :char :fg-red :fg-green :fg-blue :bg-red :bg-green :bg-blue
           :+horz-ppc+ :+vert-ppc+ :convert-pixels-to-text :with-parallel-terminal-buffer-processing
           :*color-change-tolerance* :write-terminal-buffer
           :read-image :print-image-as-text))

