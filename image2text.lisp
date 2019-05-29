;;;; image2text.lisp

(in-package #:cl-image2text)

(defun print-image-as-text (filename)
  (let ((pixel-buffer (read-image filename)))
    (with-color-buffer-size pb pixel-buffer
      (let ((size-x (the fixnum (/ pb-size-x +horz-ppc+)))
            (size-y (the fixnum (/ pb-size-y +vert-ppc+))))
        (let ((terminal-buffer (create-terminal-buffer size-x size-y)))
          #+nil (with-parallel-terminal-buffer-processing terminal-buffer
            (convert-pixels-to-text pixel-buffer terminal-buffer tb-ymin tb-ymax tb-xmin tb-xmax))
          (convert-pixels-to-text pixel-buffer terminal-buffer)
          (write-terminal-buffer terminal-buffer))))))

