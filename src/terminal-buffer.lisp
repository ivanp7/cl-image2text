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
      `(let ((,fg-col-buf (the (simple-array fixnum (* * 3))
                               (terminal-buffer-fg-color-array ,terminal-buffer)))
             (,bg-col-buf (the (simple-array fixnum (* * 3))
                               (terminal-buffer-bg-color-array ,terminal-buffer)))
             (,char-buf (the (simple-array character (* *))
                             (terminal-buffer-char-array ,terminal-buffer)))) 
         (with-color-buffer-element-colors fg ,fg-col-buf ,x ,y
           (with-color-buffer-element-colors bg ,bg-col-buf ,x ,y
             (symbol-macrolet ((char (the character (aref ,char-buf ,y ,x))))
               ,@body)))))))

;;; PICTURE-TO-TEXT CONVERSION: RENDER CHARACTERS

(defconstant +horz-ppc+ 4) ; pixels per character, x axis
(defconstant +vert-ppc+ 8) ; pixels per character, y axis
(defconstant +ppc+ (* +horz-ppc+ +vert-ppc+))

(defmacro iterate-character-pixels (buffer px py &body body)
  (alexandria:once-only (buffer)
    (alexandria:with-gensyms (x0 y0 xdispl ydispl)
      `(let ((,x0 (the fixnum (* ,px ,+horz-ppc+))) (,y0 (the fixnum (* ,py ,+vert-ppc+))))
         (loop :for y :of-type fixnum :below ,+vert-ppc+ :do
               (loop :for x :of-type fixnum :below ,+horz-ppc+ :do
                     (let ((,xdispl (the fixnum (+ x ,x0))) (,ydispl (the fixnum (+ y ,y0))))
                       (symbol-macrolet ((red (color-buffer-element-color 
                                                red ,buffer ,xdispl ,ydispl))
                                         (green (color-buffer-element-color 
                                                  green ,buffer ,xdispl ,ydispl))
                                         (blue (color-buffer-element-color 
                                                 blue ,buffer ,xdispl ,ydispl)))
                         ,@body))))))))

(defun make-render-character-function (description)
  (let ((character (first description)) (char-fg-p (second description)))
    `#'(lambda (buffer px py)
         (declare (optimize (speed 3) (safety 0))
                  (type (simple-array fixnum (* * 3)) buffer)
                  (type fixnum px py))
         (let ((score 0) (fg-pixels 0)
               (fg-red-mean 0) (fg-green-mean 0) (fg-blue-mean 0) 
               (bg-red-mean 0) (bg-green-mean 0) (bg-blue-mean 0))
           (declare (type fixnum score fg-pixels
                          fg-red-mean fg-green-mean fg-blue-mean
                          bg-red-mean bg-green-mean bg-blue-mean))
           (iterate-character-pixels buffer px py
             (if ,char-fg-p
               (progn
                 (setf fg-pixels
                       (the fixnum (1+ fg-pixels)))
                 (setf fg-red-mean
                       (the fixnum (+ fg-red-mean red)))
                 (setf fg-green-mean
                       (the fixnum (+ fg-green-mean green)))
                 (setf fg-blue-mean
                       (the fixnum (+ fg-blue-mean blue))))
               (progn
                 (setf bg-red-mean
                       (the fixnum (+ bg-red-mean red)))
                 (setf bg-green-mean
                       (the fixnum (+ bg-green-mean green)))
                 (setf bg-blue-mean
                       (the fixnum (+ bg-blue-mean blue))))))
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
             (let ((delta-red (the fixnum 
                                   (- red (if ,char-fg-p fg-red-mean bg-red-mean))))
                   (delta-green (the fixnum 
                                     (- green (if ,char-fg-p fg-green-mean bg-green-mean))))
                   (delta-blue (the fixnum 
                                    (- blue (if ,char-fg-p fg-blue-mean bg-blue-mean)))))
               (setf score
                     (the fixnum
                          (+ score
                             (the fixnum (* delta-red delta-red))
                             (the fixnum (* delta-green delta-green))
                             (the fixnum (* delta-blue delta-blue)))))))
           (values score ,character
                   fg-red-mean fg-green-mean fg-blue-mean
                   bg-red-mean bg-green-mean bg-blue-mean)))))

(defvar *render-characters*)

(defmacro define-render-characters (&rest descriptions)
  `(setf *render-characters*
         (make-array ,(length descriptions) :element-type 'function
                     :initial-contents (list ,@(mapcar #'make-render-character-function
                                                       descriptions)))))

;;; PICTURE-TO-TEXT CONVERSION: USED RENDER CHARACTERS

(define-render-characters
  (#\Space nil) ; empty

  (#\▝ (and (>= x 2) (< y 4))) ; 1st quadrant
  (#\▗ (and (>= x 2) (>= y 4))) ; 2nd quadrant
  (#\▖ (and (< x 2) (>= y 4))) ; 3rd quadrant
  (#\▘ (and (< x 2) (< y 4))) ; 4th quadrant

  (#\▞ (or (and (< x 2) (>= y 4))
          (and (>= x 2) (< y 4)))) ; main diagonal

  (#\▁ (>= y (- 8 1))) ; 1/8 down
  (#\▂ (>= y (- 8 2))) ; 2/8 down
  (#\▃ (>= y (- 8 3))) ; 3/8 down
  (#\▄ (>= y (- 8 4))) ; 4/8 down
  (#\▅ (>= y (- 8 5))) ; 5/8 down
  (#\▆ (>= y (- 8 6))) ; 6/8 down
  (#\▇ (>= y (- 8 7))) ; 7/8 down

  (#\▎ (< x 1)) ; 1/4 left
  (#\▌ (< x 2)) ; 2/4 left
  (#\▊ (< x 3)) ; 3/4 left
  )

;;; PICTURE-TO-TEXT CONVERSION: ALGORITHM

(defun convert-pixels-to-character (pixel-buffer px py)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* * 3)) pixel-buffer)
           (type fixnum px py))
  (let ((min-score most-positive-fixnum) (best-char #\!) 
        (best-fg-red 0) (best-fg-green 0) (best-fg-blue 0) 
        (best-bg-red 0) (best-bg-green 0) (best-bg-blue 0))
    (declare (type fixnum min-score best-fg-red best-fg-green best-fg-blue 
                   best-bg-red best-bg-green best-bg-blue))
    (loop :for chdscr :of-type function 
          :across (the (simple-array function (*)) *render-characters*)
          :do
          (multiple-value-bind (score char fg-red fg-green fg-blue bg-red bg-green bg-blue)
              (funcall chdscr pixel-buffer px py)
            (declare (type fixnum score fg-red fg-green fg-blue bg-red bg-green bg-blue)
                     (type character char))
            (when (< score min-score)
              (setf min-score score
                    best-char char
                    best-fg-red fg-red
                    best-fg-green fg-green
                    best-fg-blue fg-blue
                    best-bg-red bg-red
                    best-bg-green bg-green
                    best-bg-blue bg-blue))))
    (values best-char
            best-fg-red best-fg-green best-fg-blue 
            best-bg-red best-bg-green best-bg-blue)))

(defun convert-image-to-text (pixel-buffer terminal-buffer)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* * 3)) pixel-buffer)
           (type terminal-buffer terminal-buffer))
  (with-terminal-buffer-size terminal-buffer
    (dotimes (px tb-size-x)
      (declare (type fixnum px))
      (dotimes (py tb-size-y)
        (declare (type fixnum py))
        (multiple-value-bind (char-value 
                              fg-red-value fg-green-value fg-blue-value 
                              bg-red-value bg-green-value bg-blue-value)
            (convert-pixels-to-character pixel-buffer px py)
          (declare (type character char-value)
                   (type fixnum fg-red-value fg-green-value fg-blue-value 
                         bg-red-value bg-green-value bg-blue-value))
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

