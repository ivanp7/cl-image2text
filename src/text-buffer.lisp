;;;; text-buffer.lisp

(in-package #:traytr)

;;; COORDINATES

(defun point (x y)
  (cons x y))

(defmacro point-x (point)
  `(car ,point))

(defmacro point-y (point)
  `(cdr ,point))

;; 1920x1080, font Terminus u12n, tmux window, i3 status bar
(defparameter *screen-size* (point 319 87)) 

;;; TEXT BUFFER

(defvar *character-resolution* (point 8 8))

