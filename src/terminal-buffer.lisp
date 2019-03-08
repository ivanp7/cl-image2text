;;;; terminal-buffer.lisp

(in-package #:traytr)

;;; CHARACTER ARRAY

(defun make-char-array (x y)
  (make-array `(,x ,y) :element-type 'character :initial-element #\Space))

(defun get-char-array-element (buffer x y)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array character (* *)) buffer)
           (type fixnum x y))
  (aref buffer x y))

(defun set-char-array-element (buffer x y char)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array character (* *)) buffer)
           (type fixnum x y)
           (type character char))
  (setf (aref buffer x y) char)
  t)

;;; COLOR ARRAY

(defun make-color-array (x y)
  (make-array `(,x ,y 6) :element-type '(unsigned-byte 8) :initial-element 0))

(defun get-color-array-element (buffer x y)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (* * 6)) buffer)
           (type fixnum x y))
  (values (aref buffer x y 0) (aref buffer x y 1) (aref buffer x y 2)
          (aref buffer x y 3) (aref buffer x y 4) (aref buffer x y 5)))

(defmacro with-color-array-element (buffer x y &body body)
  `(multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue) 
       (get-color-array-element (the (simple-array (unsigned-byte 8) (* * 6)) ,buffer)
                                 (the fixnum ,x) (the fixnum ,y))
     (declare (type (unsigned-byte 8) fg-red fg-green fg-blue bg-red bg-green bg-blue))
     ,@body))

(defun set-color-array-element (buffer x y fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (* * 6)) buffer)
           (type fixnum x y)
           (type (unsigned-byte 8) fg-red fg-green fg-blue bg-red bg-green bg-blue))
  (setf (aref buffer x y 0) fg-red
        (aref buffer x y 1) fg-green
        (aref buffer x y 2) fg-blue
        (aref buffer x y 3) bg-red
        (aref buffer x y 4) bg-green
        (aref buffer x y 5) bg-blue)
  t)

;;; TERMINAL BUFFER

(defun make-terminal-buffer (x y)
  (cons (the (simple-array character (* *)) (make-char-array x y))
        (the (simple-array (unsigned-byte 8) (* * 6)) (make-color-array x y))))

(defmacro char-array (terminal-buffer)
  `(car ,terminal-buffer))

(defmacro color-array (terminal-buffer)
  `(cdr ,terminal-buffer))

(defmacro with-terminal-buffer-element (terminal-buffer x y &body body)
  (alexandria:once-only (terminal-buffer x y) 
    `(with-color-array-element (color-array ,terminal-buffer) ,x ,y
       (let ((char (get-char-array-element (the (simple-array character (* *)) 
                                                 (char-array ,terminal-buffer)) 
                                            (the fixnum ,x) (the fixnum ,y))))
         (declare (type character char))
         ,@body))))

(defun set-terminal-buffer-element (terminal-buffer x y char fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum x y)
           (type character char)
           (type (unsigned-byte 8) fg-red fg-green fg-blue bg-red bg-green bg-blue))
  (set-char-array-element (char-array terminal-buffer) x y char)
  (set-color-array-element (color-array terminal-buffer) x y 
                            fg-red fg-green fg-blue bg-red bg-green bg-blue)
  t)

;;; USED CHARACTER TYPES

(defmacro define-terminal-buffer-element-setter (macro-name param-name block-string 
                                                 &optional inverse-p)
  (let* ((block-string-value (etypecase block-string
                               (symbol (symbol-value block-string))
                               (sequence block-string)))
         (index-expr (if inverse-p 
                       `(- ,(1- (length block-string-value)) ,param-name) 
                       param-name))
         (color-values-list (if inverse-p
                              ``(,bg-red ,bg-green ,bg-blue ,fg-red ,fg-green ,fg-blue)
                              ``(,fg-red ,fg-green ,fg-blue ,bg-red ,bg-green ,bg-blue)))) 
    `(defmacro ,macro-name (terminal-buffer x y ,param-name 
                            fg-red fg-green fg-blue bg-red bg-green bg-blue)
       `(set-terminal-buffer-element ,terminal-buffer ,x ,y
                                 (aref ,,block-string-value ,,index-expr)
                                 ,@,color-values-list))))

(alexandria:define-constant +down-blocks+ " ▁▂▃▄▅▆▇█" :test #'equal)
(define-terminal-buffer-element-setter set-down-block height +down-blocks+)
(define-terminal-buffer-element-setter set-up-block height +down-blocks+ t)

(alexandria:define-constant +left-blocks+ " ▏▎▍▌▋▊▉█" :test #'equal)
(define-terminal-buffer-element-setter set-left-block width +left-blocks+)
(define-terminal-buffer-element-setter set-right-block width +left-blocks+ t)

(alexandria:define-constant +quadrant-blocks+ "▝▗▖▘" :test #'equal)
(define-terminal-buffer-element-setter set-quadrant-block quarter +quadrant-blocks+)
(define-terminal-buffer-element-setter set-inv-quadrant-block quarter +quadrant-blocks+ t)

(alexandria:define-constant +quadrant-block-diagonal+ "▞" :test #'equal)
(define-terminal-buffer-element-setter set-quadrant-block-diagonal num +quadrant-block-diagonal+)
(define-terminal-buffer-element-setter set-inv-quadrant-block-diagonal num +quadrant-block-diagonal+ t)

;;; TERMINAL BUFFER OUTPUT

;; switch to alt. buffer, clear screen, disable line wrap and hide cursor
(let ((init-string (format nil "~C[?1049h~:*~C[2J~:*~C[?7l~:*~C[?25l" #\Esc))) 
  (defun initialize-terminal (stream)
    (declare (type stream stream))
    (write-string init-string stream)
    t))

;; switch to primary buffer, clear screen, enable line wrap and show cursor
(let ((final-string (format nil "~C[?1049l~:*~C[2J~:*~C[?7h~:*~C[?25h" #\Esc))) 
  (defun finalize-terminal (stream)
    (declare (type stream stream))
    (write-string final-string stream)
    t))

(defmacro define-io-server-function (name &body body)
  `(defun ,name (stream)
     (declare (type stream stream))
     ,@body))

(let ((byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 
  (defun write-terminal-buffer-element (stream char 
                                        fg-red fg-green fg-blue 
                                        bg-red bg-green bg-blue)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream)
             (type character char)
             (type (unsigned-byte 8) fg-red fg-green fg-blue bg-red bg-green bg-blue))
    (write-char #\Esc stream)
    (write-string "[38;2;" stream)
    (write-string (aref byte-to-string fg-red) stream)
    (write-char #\; stream)
    (write-string (aref byte-to-string fg-green) stream)
    (write-char #\; stream)
    (write-string (aref byte-to-string fg-blue) stream)
    (write-char #\m stream)
    (write-char #\Esc stream)
    (write-string "[48;2;" stream)
    (write-string (aref byte-to-string bg-red) stream)
    (write-char #\; stream)
    (write-string (aref byte-to-string bg-green) stream)
    (write-char #\; stream)
    (write-string (aref byte-to-string bg-blue) stream)
    (write-char #\m stream)
    (write-char char stream)))

(let ((linefeed (format nil "~C[E" #\Esc))
      (home (format nil "~C[H" #\Esc))) 
  (defun write-terminal-buffer (terminal-buffer stream)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream)
             (type cons terminal-buffer))
    (let ((size (array-dimensions (char-array terminal-buffer))))
      (dotimes (y (second size))
        (declare (type fixnum y))
        (write-string (if (zerop y) home linefeed) stream)
        (dotimes (x (first size))
          (declare (type fixnum x))
          (with-terminal-buffer-element terminal-buffer x y
            (write-terminal-buffer-element 
              stream char
              fg-red fg-green fg-blue
              bg-red bg-green bg-blue))))
      (force-output stream)
      t)))

