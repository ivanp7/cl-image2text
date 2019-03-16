;;;; terminal-buffer.lisp

(in-package #:traytr)

;;; TERMINAL BUFFER

(defstruct terminal-buffer
  (char-array nil :type (simple-array character (* *)))
  (fg-color-array nil :type (simple-array fixnum (* * 3)))
  (bg-color-array nil :type (simple-array fixnum (* * 3))))

(defun create-terminal-buffer (x y)
  (make-terminal-buffer 
    :char-array (make-array `(,y ,x) :element-type 'character :initial-element #\Space)
    :fg-color-array (create-color-buffer x y)
    :bg-color-array (create-color-buffer x y)))

(defmacro with-terminal-buffer-size (terminal-buffer &body body)
  `(with-color-buffer-size tb (terminal-buffer-fg-color-array ,terminal-buffer)
     ,@body))

(defmacro with-terminal-buffer-element (terminal-buffer x y &body body)
  (alexandria:once-only (terminal-buffer x y) 
    (alexandria:with-gensyms (fg-col-buf bg-col-buf char-buf)
      `(let ((,fg-col-buf (terminal-buffer-fg-color-array ,terminal-buffer))
             (,bg-col-buf (terminal-buffer-bg-color-array ,terminal-buffer))
             (,char-buf (terminal-buffer-char-array ,terminal-buffer))) 
         (with-color-buffer-element-colors fg ,fg-col-buf ,x ,y
           (with-color-buffer-element-colors bg ,bg-col-buf ,x ,y
             (symbol-macrolet ((char (aref ,char-buf ,y ,x)))
               ,@body)))))))

;;; PICTURE-TO-TEXT CONVERSION: RENDER CHARACTERS

(defconstant +horz-ppc+ 4) ; pixels per character, x axis
(defconstant +vert-ppc+ 8) ; pixels per character, y axis
(defconstant +ppc+ (* +horz-ppc+ +vert-ppc+))

(defmacro iterate-character-pixels (buffer px py &body body)
  (alexandria:once-only (buffer)
    (alexandria:with-gensyms (x0 y0 xdispl ydispl)
      `(let ((,x0 (* ,px ,+horz-ppc+)) (,y0 (* ,py ,+vert-ppc+)))
         (loop :for y :below ,+vert-ppc+ :do
               (loop :for x :below ,+horz-ppc+ :do
                     (let ((,xdispl (+ x ,x0)) (,ydispl (+ y ,y0)))
                       (symbol-macrolet ((red (color-buffer-element-color 
                                                red ,buffer ,xdispl ,ydispl))
                                         (green (color-buffer-element-color 
                                                  green ,buffer ,xdispl ,ydispl))
                                         (blue (color-buffer-element-color 
                                                 blue ,buffer ,xdispl ,ydispl)))
                         ,@body))))))))

(defmacro define-render-character (character char-fg-p)
  `(progn
     (setf *render-characters* (nreverse *render-characters*))
     (setf *render-characters*
           (pushnew (cons ,character
                          #'(lambda (buffer px py)
                              (let ((score 0) (fg-pixels 0)
                                    (fg-red-mean 0) (fg-green-mean 0) (fg-blue-mean 0) 
                                    (bg-red-mean 0) (bg-green-mean 0) (bg-blue-mean 0))
                                (iterate-character-pixels buffer px py
                                  (if ,char-fg-p
                                    (progn
                                      (incf fg-pixels)
                                      (incf fg-red-mean red)
                                      (incf fg-green-mean green)
                                      (incf fg-blue-mean blue))
                                    (progn
                                      (incf bg-red-mean red)
                                      (incf bg-green-mean green)
                                      (incf bg-blue-mean blue))))
                                (setf fg-red-mean (if (zerop fg-pixels) 0 
                                                    (round fg-red-mean fg-pixels))
                                      fg-green-mean (if (zerop fg-pixels) 0 
                                                      (round fg-green-mean fg-pixels))
                                      fg-blue-mean (if (zerop fg-pixels) 0 
                                                     (round fg-blue-mean fg-pixels))
                                      bg-red-mean (if (zerop (- +ppc+ fg-pixels)) 0 
                                                    (round bg-red-mean (- +ppc+ fg-pixels)))
                                      bg-green-mean (if (zerop (- +ppc+ fg-pixels)) 0 
                                                      (round bg-green-mean (- +ppc+ fg-pixels)))
                                      bg-blue-mean (if (zerop (- +ppc+ fg-pixels)) 0 
                                                     (round bg-blue-mean (- +ppc+ fg-pixels))))
                                (iterate-character-pixels buffer px py
                                  (incf score
                                        (if ,char-fg-p
                                          (+ (expt (- red fg-red-mean) 2)
                                             (expt (- green fg-green-mean) 2)
                                             (expt (- blue fg-blue-mean) 2))
                                          (+ (expt (- red bg-red-mean) 2)
                                             (expt (- green bg-green-mean) 2)
                                             (expt (- blue bg-blue-mean) 2)))))
                                (values score
                                        fg-red-mean fg-green-mean fg-blue-mean
                                        bg-red-mean bg-green-mean bg-blue-mean))))
                  *render-characters* :key #'car))
     (setf *render-characters* (nreverse *render-characters*))))

(defmacro chdscr-character (chdscr)
  `(car ,chdscr))

(defmacro chdscr-function (chdscr)
  `(cdr ,chdscr))

;;; PICTURE-TO-TEXT CONVERSION: USED RENDER CHARACTERS

(defparameter *render-characters* ())

(define-render-character #\Space nil) ; empty

(define-render-character #\▝ (and (>= x 2) (< y 4))) ; 1st quadrant
(define-render-character #\▗ (and (>= x 2) (>= y 4))) ; 2nd quadrant
(define-render-character #\▖ (and (< x 2) (>= y 4))) ; 3rd quadrant
(define-render-character #\▘ (and (< x 2) (< y 4))) ; 4th quadrant

(define-render-character #\▞ (or (and (< x 2) (>= y 4))
                                (and (>= x 2) (< y 4)))) ; main diagonal

(define-render-character #\▁ (>= y (- 8 1))) ; 1/8 down
(define-render-character #\▂ (>= y (- 8 2))) ; 2/8 down
(define-render-character #\▃ (>= y (- 8 3))) ; 3/8 down
(define-render-character #\▄ (>= y (- 8 4))) ; 4/8 down
(define-render-character #\▅ (>= y (- 8 5))) ; 5/8 down
(define-render-character #\▆ (>= y (- 8 6))) ; 6/8 down
(define-render-character #\▇ (>= y (- 8 7))) ; 7/8 down

(define-render-character #\▎ (< x 1)) ; 1/4 left
(define-render-character #\▌ (< x 2)) ; 2/4 left
(define-render-character #\▊ (< x 3)) ; 3/4 left

;;; PICTURE-TO-TEXT CONVERSION: ALGORITHM

(defun choose-render-character (pixel-buffer px py)
  (let ((min-score most-positive-fixnum) best-char 
        best-fg-red best-fg-green best-fg-blue 
        best-bg-red best-bg-green best-bg-blue)
    (dolist (chdscr *render-characters* (values best-char
                                                best-fg-red best-fg-green best-fg-blue 
                                                best-bg-red best-bg-green best-bg-blue))
      (multiple-value-bind (score fg-red fg-green fg-blue bg-red bg-green bg-blue)
          (funcall (chdscr-function chdscr) pixel-buffer px py)
        (when (< score min-score)
          (setf min-score score
                best-char (chdscr-character chdscr)
                best-fg-red fg-red
                best-fg-green fg-green
                best-fg-blue fg-blue
                best-bg-red bg-red
                best-bg-green bg-green
                best-bg-blue bg-blue))))))

(defun convert-pixel-to-terminal-buffer (pixel-buffer terminal-buffer)
  (with-terminal-buffer-size terminal-buffer
    (dotimes (px tb-size-x)
      (dotimes (py tb-size-y)
        (multiple-value-bind (char-value 
                              fg-red-value fg-green-value fg-blue-value 
                              bg-red-value bg-green-value bg-blue-value)
            (choose-render-character pixel-buffer px py)
          (with-terminal-buffer-element terminal-buffer px py
            (setf char char-value
                  fg-red fg-red-value
                  fg-green fg-green-value
                  fg-blue fg-blue-value
                  bg-red bg-red-value
                  bg-green bg-green-value
                  bg-blue bg-blue-value)))))))

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
             (type fixnum fg-red fg-green fg-blue bg-red bg-green bg-blue))
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
             (type terminal-buffer terminal-buffer))
    (with-terminal-buffer-size terminal-buffer
      (dotimes (y tb-size-y)
        (write-string (if (zerop y) home linefeed) stream)
        (dotimes (x tb-size-x)
          (with-terminal-buffer-element terminal-buffer x y
            (write-terminal-buffer-element 
              stream char
              fg-red fg-green fg-blue
              bg-red bg-green bg-blue)))))
    (force-output stream)
    t))

