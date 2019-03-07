;;;; char-buffer.lisp

(in-package #:traytr)

;;; COLORS

(defun make-color (red green blue)
  (check-type red (integer 0 255))
  (check-type green (integer 0 255))
  (check-type blue (integer 0 255))
  (vector red green blue))

(defmacro red (color)
  `(svref ,color 0))

(defmacro green (color)
  `(svref ,color 1))

(defmacro blue (color)
  `(svref ,color 2))

;;; COLORED CHARACTERS

(defun make-cc (char fg bg)
  (vector char fg bg))

(defmacro chr (colchar)
  `(svref ,colchar 0))

(defmacro fg-color (colchar)
  `(svref ,colchar 1))

(defmacro bg-color (colchar)
  `(svref ,colchar 2))

;;; USED COLORED CHARACTER TYPES

(alexandria:define-constant +shade-blocks+ " ░▒▓█" :test #'equal)

(alexandria:define-constant +down-blocks+ " ▁▂▃▄▅▆▇█" :test #'equal)
(alexandria:define-constant +left-blocks+ " ▏▎▍▌▋▊▉█" :test #'equal)
(alexandria:define-constant +quadrant-blocks+ "▝▗▖▘" :test #'equal)
(alexandria:define-constant +quadrant-block-pairs+ "▞▚" :test #'equal)

(defmacro shade-block-cc (n fg bg)
  `(make-cc (aref ,+shade-blocks+ ,n) ,fg ,bg))

(defmacro down-block-cc (n fg bg)
  `(make-cc (aref ,+down-blocks+ ,n) ,fg ,bg))

(defmacro up-block-cc (n fg bg)
  `(down-block-cc ,(- 7 n) ,bg ,fg))

(defmacro left-block-cc (n fg bg)
  `(make-cc (aref ,+left-blocks+ ,n) ,fg ,bg))

(defmacro right-block-cc (n fg bg)
  `(left-block-cc ,(- 7 n) ,bg ,fg))

(defmacro quadrant-block-cc (n fg bg)
  `(make-cc (aref ,+quadrant-blocks+ ,(1- n)) ,fg ,bg))

(defmacro inv-quadrant-block-cc (n fg bg)
  `(quadrant-block-cc ,n ,bg ,fg))

(defmacro quadrant-block-pair-cc (n fg bg)
  `(make-cc (aref ,+quadrant-block-pairs+ ,(1- n)) ,fg ,bg))

;;; COORDINATES

(defun point (x y)
  (cons x y))

(defmacro point-x (point)
  `(car ,point))

(defmacro point-y (point)
  `(cdr ,point))

;;; COLORED CHARACTER BUFFERS

(let ((empty-cc (make-cc #\Space (make-color 0 0 0) (make-color 0 0 0))))
  (defun make-cc-buffer (size)
    (make-array `(,(point-x size) ,(point-y size)) :element-type 'vector :initial-element empty-cc)))

(defun cc-buffer-element (buffer point)
  (aref buffer (point-x point) (point-y point)))

(defun (setf cc-buffer-element) (new-value buffer point)
  (setf (aref buffer (point-x point) (point-y point)) new-value))

