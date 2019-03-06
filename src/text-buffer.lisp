;;;; text-buffer.lisp

(in-package #:traytr)

;;; COORDINATES

(defun point (x y)
  (cons x y))

(defmacro point-x (point)
  `(car ,point))

(defmacro point-y (point)
  `(cdr ,point))

;;; TEXT BUFFER

;; 1920x1080, font Terminus u12n, i3 window with size of 75%x75% of the screen
(defparameter *screen-size* (point 238 66)) 

(defun make-text-buffer (&optional (size *screen-size*))
  (make-array `(,(point-x size) ,(point-y size)) 
              :element-type 'vector :initial-element +empty-cc+))

(defvar *text-buffer* (make-text-buffer))

(defun resize-text-buffer (buffer size)
  (adjust-array buffer size))

(defmacro resize-text-buffer (size &optional (buffer *text-buffer*))
  `(setf ,buffer (adjust-array ,buffer ,size)))

(defmacro text-buffer-element (buffer point)
  (alexandria:with-gensyms 
    (buf pt)
    `(let ((,buf ,buffer) (,pt ,point))
       (aref ,buf (point-x ,pt) (point-y ,pt)))))

