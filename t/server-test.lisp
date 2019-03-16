;;;; server-test.lisp

(in-package #:traytr-test)

(defun set-io-server-function/color-test (&optional (size-x 238) (size-y 66))
  (define-io-server-function io-server-function/color-test
    (initialize-terminal stream)
    (let ((bp (create-terminal-buffer size-x size-y)))
    (dotimes (i 256)
      (dotimes (x size-x)
        (dotimes (y size-y)
          (with-terminal-buffer-element bp x y
            (setf char #\▂ 
                  fg-red i fg-green i fg-blue i
                  bg-red (- 255 i) bg-green (- 255 i) bg-blue (- 255 i)))))
      (write-terminal-buffer bp stream)))
    (finalize-terminal stream))

  (setf *io-server-function* #'io-server-function/color-test))

(defun set-io-server-function/smoothness-test (&optional (size-x 238) (size-y 66))
  (define-io-server-function io-server-function/smoothness-test
    (initialize-terminal stream)
    (let ((bp (create-terminal-buffer size-x size-y)))
      (dotimes (i size-x)
        (dotimes (x size-x)
          (dotimes (y size-y)
            (with-terminal-buffer-element bp x y
              (setf char (if (= x i) #\█ #\Space)
                    fg-red 255 fg-green 255 fg-blue 255
                    bg-red 0 bg-green 0 bg-blue 0))))
        (write-terminal-buffer bp stream)))
    (finalize-terminal stream))

  (setf *io-server-function* #'io-server-function/smoothness-test))

(defun set-io-server-function/converter-test (&optional (size-x 238) (size-y 66))
  (define-io-server-function io-server-function/converter-test
    (initialize-terminal stream)
    (let* ((size-px (* size-x +horz-ppc+)) (size-py (* size-y +vert-ppc+)) 
           (pixel-buffer (create-color-buffer size-px size-py))
           (terminal-buffer (create-terminal-buffer size-x size-y)))
      (dotimes (x size-px)
        (dotimes (y size-py)
          (with-color-buffer-element-colors px pixel-buffer x y
            (if (< (+ (expt (- x (/ size-px 2)) 2)
                      (expt (- y (/ size-py 2)) 2))
                   (expt (/ size-py 4) 2))
              (setf px-red 255
                    px-green 255
                    px-blue 255)
              (setf px-red 0
                    px-green 0
                    px-blue 0)))))
      (convert-pixel-to-terminal-buffer pixel-buffer terminal-buffer)
      (write-terminal-buffer terminal-buffer stream)
      (read-char stream)
      (finalize-terminal stream)))

  (setf *io-server-function* #'io-server-function/converter-test))

(defun set-io-server-function/converter-test2 ()
  (define-io-server-function io-server-function/converter-test2
    (initialize-terminal stream)
    (let ((img (read-jpeg-file "t/pic.jpg")))
      (with-image-bounds (height width) img
        (let* ((size-x (floor width +horz-ppc+)) (size-y (floor height +vert-ppc+))
               (size-px (* size-x +horz-ppc+)) (size-py (* size-y +vert-ppc+)) 
               (pixel-buffer (create-color-buffer size-px size-py))
               (terminal-buffer (create-terminal-buffer size-x size-y)))
          (dotimes (x size-px)
            (dotimes (y size-py)
              (with-color-buffer-element-colors px pixel-buffer x y
                (multiple-value-bind (r g b) (pixel img y x)
                  (setf px-red r
                        px-green g
                        px-blue b)))))
          (convert-pixel-to-terminal-buffer pixel-buffer terminal-buffer)
          (write-terminal-buffer terminal-buffer stream)
          (read-char stream)
          (finalize-terminal stream)))))

  (setf *io-server-function* #'io-server-function/converter-test2))
