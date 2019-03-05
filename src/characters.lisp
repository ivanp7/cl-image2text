;;;; characters.lisp

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

;; colored character
(defun make-cc (char fg bg)
  (vector char fg bg))

(defmacro cc-char (colchar)
  `(svref ,colchar 0))

(defmacro cc-fg-color (colchar)
  `(svref ,colchar 1))

(defmacro cc-bg-color (colchar)
  `(svref ,colchar 2))

;;; CHARACTERS

(defconstant +shade-blocks+ " ░▒▓█")
(defconstant +down-blocks+ " ▁▂▃▄▅▆▇█")
(defconstant +left-blocks+ " ▏▎▍▌▋▊▉█")
(defconstant +quadrant-blocks+ "▝▗▖▘")

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
  `(make-cc (aref ,+quarter-blocks+ ,(1- n)) ,fg ,bg))

(defmacro inv-quadrant-block-cc (n fg bg)
  `(make-cc (aref ,+quarter-blocks+ ,(1- n)) ,bg ,fg))

