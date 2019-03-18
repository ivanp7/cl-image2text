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

;;; PARALLEL PROCESSING OF A TERMINAL BUFFER

(defmacro with-parallel-terminal-buffer-processing (buffer &body body)
  (alexandria:with-gensyms (number-of-threads)
    `(with-terminal-buffer-size ,buffer
       (let* ((,number-of-threads (the fixnum (1- (cl-cpus:get-number-of-processors))))
              (threads (make-array ,number-of-threads :element-type '(or null bt:thread)
                                   :initial-element nil))
              (segment-size-y (the rational (/ tb-size-y (1+ ,number-of-threads)))))
         (dotimes (thread-i ,number-of-threads)
           (declare (type fixnum thread-i))
           (let* ((tb-xmin 0) (tb-xmax tb-size-x)
                  (tb-ymin (the fixnum (floor (* thread-i segment-size-y)))) 
                  (tb-ymax (the fixnum (floor (* (1+ thread-i) segment-size-y)))))
             (setf (svref threads thread-i) (bt:make-thread #'(lambda () ,@body)))))
         (prog1
           (let* ((tb-xmin 0) (tb-xmax tb-size-x)
                  (tb-ymin (the fixnum (floor (* ,number-of-threads segment-size-y)))) 
                  (tb-ymax tb-size-y))
             ,@body)
           (dotimes (thread-i ,number-of-threads)
             (declare (type fixnum thread-i))
             (bt:join-thread (svref threads thread-i))))))))

;;; PICTURE-TO-TEXT CONVERSION: RENDER CHARACTERS

(defmacro iterate-cell-pixels (buffer px py &body body)
  (alexandria:once-only (buffer)
    (alexandria:with-gensyms (x0 y0 xdispl ydispl)
      `(let ((,x0 (the fixnum (* ,px +horz-ppc+))) 
             (,y0 (the fixnum (* ,py +vert-ppc+))))
         (loop :for y :of-type fixnum :below +vert-ppc+ :do
               (loop :for x :of-type fixnum :below +horz-ppc+ :do
                     (let ((,xdispl (the fixnum (+ x ,x0))) (,ydispl (the fixnum (+ y ,y0))))
                       (symbol-macrolet ((red (color-buffer-element-color 
                                                red ,buffer ,xdispl ,ydispl))
                                         (green (color-buffer-element-color 
                                                  green ,buffer ,xdispl ,ydispl))
                                         (blue (color-buffer-element-color 
                                                 blue ,buffer ,xdispl ,ydispl)))
                         ,@body))))))))

(eval-when (:compile-toplevel :load-toplevel :execute) 
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
             (iterate-cell-pixels buffer px py
               (if ,char-fg-p
                 (progn
                   (setf fg-pixels (the fixnum (1+ fg-pixels)))
                   (modify-places + fixnum (fg-red-mean fg-green-mean fg-blue-mean)
                                  (red green blue)))
                 (modify-places + fixnum (bg-red-mean bg-green-mean bg-blue-mean)
                                  (red green blue))))
             (if (zerop fg-pixels)
               (modify-places * fixnum (fg-red-mean fg-green-mean fg-blue-mean)
                              (0 0 0))
               (modify-places round fixnum (fg-red-mean fg-green-mean fg-blue-mean)
                              (fg-pixels fg-pixels fg-pixels)))
             (if (= fg-pixels +ppc+)
               (modify-places * fixnum (bg-red-mean bg-green-mean bg-blue-mean)
                              (0 0 0))
               (modify-places round fixnum (bg-red-mean bg-green-mean bg-blue-mean)
                              (((- +ppc+ fg-pixels)) ((- +ppc+ fg-pixels)) ((- +ppc+ fg-pixels)))))
             (iterate-cell-pixels buffer px py
               (let* ((delta-red (the fixnum 
                                      (- red (if ,char-fg-p fg-red-mean bg-red-mean))))
                      (delta-red-sq (the fixnum (* delta-red delta-red)))
                      (delta-green (the fixnum 
                                        (- green (if ,char-fg-p fg-green-mean bg-green-mean))))
                      (delta-green-sq (the fixnum (* delta-green delta-green)))
                      (delta-blue (the fixnum 
                                       (- blue (if ,char-fg-p fg-blue-mean bg-blue-mean))))
                      (delta-blue-sq (the fixnum (* delta-blue delta-blue)))
                      (delta (the fixnum
                                  (+ delta-red-sq delta-green-sq delta-blue-sq))))
                 (setf score (the fixnum (+ score delta)))))
             (values score ,character
                     fg-red-mean fg-green-mean fg-blue-mean
                     bg-red-mean bg-green-mean bg-blue-mean))))))

(defmacro define-terminal-cell-constants ((horz-ppc vert-ppc) &rest char-descriptions)
  `(progn
     (defconstant +horz-ppc+ ,horz-ppc) ; pixels per character, x axis
     (defconstant +vert-ppc+ ,vert-ppc) ; pixels per character, y axis
     (defconstant +ppc+ (* +horz-ppc+ +vert-ppc+))
     (alexandria:define-constant +render-characters+
       (make-array ,(length char-descriptions) :element-type 'function
                   :initial-contents (list ,@(mapcar #'make-render-character-function
                                                     char-descriptions)))
       :test (constantly t))))

;;; PICTURE-TO-TEXT CONVERSION: RENDER CHARACTERS SET AND RESOLUTION

(defmacro set-terminal-cell-resolution (resolution)
  (ecase resolution
    (:1x2 `(define-terminal-cell-constants (1 2)
             (#\Space nil) ; empty

             (#\▄ (>= y (- 2 1))) ; 1/2 down
             ))
    (:2x4 `(define-terminal-cell-constants (2 4)
             (#\Space nil) ; empty

             (#\▝ (and (>= x 1) (< y 2))) ; 1st quadrant
             (#\▗ (and (>= x 1) (>= y 2))) ; 2nd quadrant
             (#\▖ (and (< x 1) (>= y 2))) ; 3rd quadrant
             (#\▘ (and (< x 1) (< y 2))) ; 4th quadrant

             (#\▞ (or (and (< x 1) (>= y 2))
                     (and (>= x 1) (< y 2)))) ; main diagonal

             (#\▂ (>= y (- 4 1))) ; 1/4 down
             (#\▄ (>= y (- 4 2))) ; 2/4 down
             (#\▆ (>= y (- 4 3))) ; 3/4 down

             (#\▌ (< x 1)) ; 1/2 left
             ))
    (:4x8 `(define-terminal-cell-constants (4 8)
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
             ))))

(set-terminal-cell-resolution :2x4) ; 2x4 is much faster than 4x8 but just slightly worse

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
          :across (the (simple-array function (*)) +render-characters+)
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

(defun convert-image-to-text (pixel-buffer terminal-buffer &optional ymin ymax xmin xmax)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* * 3)) pixel-buffer)
           (type terminal-buffer terminal-buffer)
           (type (or fixnum null) ymin ymax xmin xmax))
  (with-terminal-buffer-size terminal-buffer
    (loop :for py :of-type fixnum :from (or ymin 0) :below (or ymax tb-size-y) :do
          (loop :for px :of-type fixnum :from (or xmin 0) :below (or xmax tb-size-x) :do
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
(let ((init-string (format nil "~C[?1049h~:*~C[2J~:*~C[?7l~:*~C[?25l" #\Esc))
      (home (format nil "~C[H" #\Esc))) 
  (defun initialize-terminal (stream)
    (declare (type stream stream))
    (write-string init-string stream)
    (write-string home stream)
    (force-output stream)
    t))

;; switch to primary buffer, clear screen, enable line wrap and show cursor
(let ((final-string (format nil "~C[?1049l~:*~C[2J~:*~C[?7h~:*~C[?25h" #\Esc))
      (home (format nil "~C[H" #\Esc))) 
  (defun finalize-terminal (stream)
    (declare (type stream stream))
    (write-string final-string stream)
    (write-string home stream)
    (force-output stream)
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

