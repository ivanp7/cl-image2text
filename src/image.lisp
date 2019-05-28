;;;; image.lisp

(in-package #:cl-image2text)

(defun read-image (filename)
  (let ((img (opticl:read-image-file filename)))
    (opticl:with-image-bounds (height width) img
      (let* ((size-x (floor width +horz-ppc+)) (size-y (floor height +vert-ppc+))
             (size-px (* size-x +horz-ppc+)) (size-py (* size-y +vert-ppc+)) 
             (pixel-buffer (create-color-buffer size-px size-py)))
        (dotimes (x size-px)
          (dotimes (y size-py)
            (with-color-buffer-element-colors px pixel-buffer x y
              (multiple-value-bind (r g b) (opticl:pixel img y x)
                (setf px-red r px-green g px-blue b)))))
        pixel-buffer))))

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

