(asdf:load-system :cl-image2text)

(in-package #:cl-image2text)
(sb-posix:chdir "/home/ivanp7/data/git/code/common-lisp/applications/cl-image2text")
(sb-posix:getcwd)

(defparameter *img* (opticl:read-jpeg-file 
                      (merge-pathnames #P"/home/ivanp7/data/git/code/common-lisp/applications/cl-image2text/t/pic.jpg")))
(opticl:with-image-bounds (h w) *img*
  (defparameter *h* h)
  (defparameter *w* w)
  (defparameter *sx* (floor w +horz-ppc+))
  (defparameter *sy* (floor h +vert-ppc+))
  (defparameter *px* (* *sx* +horz-ppc+))
  (defparameter *py* (* *sy* +vert-ppc+)))

(defparameter *pb* (create-color-buffer *px* *py*))
(defparameter *tb* (create-terminal-buffer *sx* *sy*))
(dotimes (x *px*)
  (dotimes (y *py*)
    (with-color-buffer-element-colors px *pb* x y
      (multiple-value-bind (r g b) (opticl:pixel *img* y x)
        (setf px-red r
              px-green g
              px-blue b)))))

(array-dimensions *pb*)
(with-terminal-buffer-size *tb*
  (list tb-size-x tb-size-y))

(convert-pixels-to-text *pb* *tb*)

(with-parallel-terminal-buffer-processing terminal-buffer
  (convert-image-to-text pixel-buffer terminal-buffer tb-ymin tb-ymax tb-xmin tb-xmax))

(with-open-file (stream "pic.txt" :direction :output :if-exists :overwrite) 
  (write-terminal-buffer *tb* stream))

