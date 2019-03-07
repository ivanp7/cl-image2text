;;;; terminal-buffer-pair.lisp

(in-package #:traytr)

;;; CHARACTER BUFFER

(defun make-char-buffer (size)
  (make-array `(,(point-x size) ,(point-y size)) 
              :element-type 'character :initial-element #\Space))

(defun get-char-buffer-element (buffer x y)
  (declare (type (simple-array character (* *)) buffer)
           (type integer x y))
  (aref buffer x y))

(defun set-char-buffer-element (buffer x y char)
  (declare (type (simple-array character (* *)) buffer)
           (type integer x y)
           (type character char))
  (setf (aref buffer x y) char)
  t)

;;; COLOR BUFFER

(defun make-color-buffer (size)
  (make-array `(,(point-x size) ,(point-y size) 6) 
              :element-type '(integer 0 255) :initial-element 0))

(defun get-color-buffer-element (buffer x y)
  (declare (type (simple-array (integer 0 255) (* * 6)) buffer)
           (type integer x y))
  (values (aref buffer x y 0) (aref buffer x y 1) (aref buffer x y 2)
          (aref buffer x y 3) (aref buffer x y 4) (aref buffer x y 5)))

(defmacro with-color-buffer-element (buffer x y &body body)
  `(multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue) 
     (get-color-buffer-element ,buffer ,x ,y)
     ,@body))

(defun set-color-buffer-element (buffer x y fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (declare (type (simple-array (integer 0 255) (* * 6)) buffer)
           (type integer x y)
           (type (integer 0 255) fg-red fg-green fg-blue bg-red bg-green bg-blue))
  (setf (aref buffer x y 0) fg-red
        (aref buffer x y 1) fg-green
        (aref buffer x y 2) fg-blue
        (aref buffer x y 3) bg-red
        (aref buffer x y 4) bg-green
        (aref buffer x y 5) bg-blue)
  t)

;;; CHARACTER-COLOR BUFFER PAIR

(defun make-buffer-pair (size)
  (cons (make-char-buffer size) (make-color-buffer size)))

(defmacro char-buffer (buffer-pair)
  `(car ,buffer-pair))

(defmacro color-buffer (buffer-pair)
  `(cdr ,buffer-pair))

(defmacro with-buffer-pair-element (buffer-pair x y &body body)
  (alexandria:once-only (buffer-pair x y) 
    `(with-color-buffer-element (color-buffer ,buffer-pair) ,x ,y
       (let ((char (get-char-buffer-element (char-buffer ,buffer-pair) ,x ,y)))
         ,@body))))

(defun set-buffer-pair-element (buffer-pair x y char fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (declare (type integer x y)
           (type character char)
           (type (integer 0 255) fg-red fg-green fg-blue bg-red bg-green bg-blue))
  (set-char-buffer-element (char-buffer buffer-pair) x y char)
  (set-color-buffer-element (color-buffer buffer-pair) x y 
                            fg-red fg-green fg-blue bg-red bg-green bg-blue)
  t)

;;; USED CHARACTER TYPES

(defmacro define-buffer-pair-element-setter (macro-name param-name block-string &optional inverse-p)
  (let* ((block-string-value (etypecase block-string
                               (symbol (symbol-value block-string))
                               (sequence block-string)))
         (index-expr (if inverse-p 
                      `(- ,(1- (length block-string-value)) ,param-name) 
                      param-name))
        (color-values-list (if inverse-p
                             ``(bg-red bg-green bg-blue fg-red fg-green fg-blue)
                             ``(fg-red fg-green fg-blue bg-red bg-green bg-blue)))) 
    `(defmacro ,macro-name (buffer-pair x y ,param-name fg-red fg-green fg-blue bg-red bg-green bg-blue)
       `(set-buffer-pair-element ,buffer-pair ,x ,y
                                 (aref ,,block-string-value ,,index-expr)
                                 ,@,color-values-list))))


(alexandria:define-constant +shade-blocks+ " ░▒▓█" :test #'equal)
(define-buffer-pair-element-setter set-bp-element/shade-block shade-degree +shade-blocks+)

(alexandria:define-constant +down-blocks+ " ▁▂▃▄▅▆▇█" :test #'equal)
(define-buffer-pair-element-setter set-bp-element/down-block height +down-blocks+)
(define-buffer-pair-element-setter set-bp-element/up-block height +down-blocks+ t)

(alexandria:define-constant +left-blocks+ " ▏▎▍▌▋▊▉█" :test #'equal)
(define-buffer-pair-element-setter set-bp-element/left-block width +left-blocks+)
(define-buffer-pair-element-setter set-bp-element/right-block width +left-blocks+ t)

(alexandria:define-constant +quadrant-blocks+ "▝▗▖▘" :test #'equal)
(define-buffer-pair-element-setter set-bp-element/quadrant-block quarter +quadrant-blocks+)
(define-buffer-pair-element-setter set-bp-element/inv-quadrant-block quarter +quadrant-blocks+ t)

(alexandria:define-constant +quadrant-block-pair+ "▞" :test #'equal)
(define-buffer-pair-element-setter set-bp-element/quadrant-block-pair num +quadrant-block-pair+)
(define-buffer-pair-element-setter set-bp-element/inv-quadrant-block-pair num +quadrant-block-pair+ t)

