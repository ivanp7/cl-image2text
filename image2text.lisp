;;;; image2text.lisp

(in-package #:cl-image2text)

(defun read-convert-output (filename &key x y)
  (write-terminal-buffer (convert-image-to-text (read-image filename :x x :y y))))

