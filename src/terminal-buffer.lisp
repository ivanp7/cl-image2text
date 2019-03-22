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
         (declare (ignorable ,char-buf))
         (with-color-buffer-element-colors fg ,fg-col-buf ,x ,y
           (with-color-buffer-element-colors bg ,bg-col-buf ,x ,y
             (symbol-macrolet ((char (aref ,char-buf ,y ,x)))
               (declare (ignorable char))
               ,@body)))))))

(defun clone-terminal-buffer (dest src &optional xmin ymin xmax ymax)
  (declare (optimize (speed 3) (safety 0))
           (type terminal-buffer dest src)
           (type (or null fixnum) xmin ymin xmax ymax))
  (with-terminal-buffer-size src
    (loop :for y :of-type fixnum :from (or ymin 0) :below (or ymax tb-size-y) :do
          (loop :for x :of-type fixnum :from (or xmin 0) :below (or xmax tb-size-x) :do
                (let (char-value fg-red-value fg-green-value fg-blue-value
                      bg-red-value bg-green-value bg-blue-value) 
                  (with-terminal-buffer-element src x y
                    (setf char-value char fg-red-value fg-red fg-green-value fg-green
                          fg-blue-value fg-blue bg-red-value bg-red bg-green-value bg-green
                          bg-blue-value bg-blue))
                  (with-terminal-buffer-element dest x y
                    (setf char-value char fg-red fg-red-value fg-green fg-green-value
                          fg-blue fg-blue-value bg-red bg-red-value bg-green bg-green-value
                          bg-blue bg-blue-value)))))))

;;; PICTURE-TO-TEXT CONVERSION: ALGORITHM

(defconstant +horz-ppc+ 2) ; pixels per character, x axis
(defconstant +vert-ppc+ 4) ; pixels per character, y axis

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-pixel-colors-symbols (i)
    (let ((red (intern (format nil "RED-~A" i)))
          (green (intern (format nil "GREEN-~A" i)))
          (blue (intern (format nil "BLUE-~A" i))))
      (values red green blue)))

  (defun make-character-colors-symbols (charname)
    (let ((fg-red (intern (format nil "FG-RED-~A" charname)))
          (fg-green (intern (format nil "FG-GREEN-~A" charname)))
          (fg-blue (intern (format nil "FG-BLUE-~A" charname)))
          (bg-red (intern (format nil "BG-RED-~A" charname)))
          (bg-green (intern (format nil "BG-GREEN-~A" charname)))
          (bg-blue (intern (format nil "BG-BLUE-~A" charname))))
      (values fg-red fg-green fg-blue bg-red bg-green bg-blue)))
  
  (defun make-character-score-symbol (charname)
    (let ((score (intern (format nil "SCORE-~A" charname))))
      score)))

(defmacro bind-cell-colors (pixel-buffer px py &body body)
  (alexandria:once-only (pixel-buffer px py)
    (alexandria:with-gensyms (x0 y0)
      (let (red-symbols green-symbols blue-symbols bindings)
        (dotimes (y +vert-ppc+)
          (dotimes (x +horz-ppc+)
            (let ((i (+ x (* y +horz-ppc+))))
              (multiple-value-bind (red green blue)
                  (make-pixel-colors-symbols i)
                (setf red-symbols (cons red red-symbols)
                      green-symbols (cons green green-symbols)
                      blue-symbols (cons blue blue-symbols)
                      bindings (list* `(,blue (color-buffer-element-color 
                                                blue ,pixel-buffer
                                                (the fixnum (+ ,x0 ,x)) 
                                                (the fixnum (+ ,y0 ,y))))
                                      `(,green (color-buffer-element-color 
                                                 green ,pixel-buffer
                                                 (the fixnum (+ ,x0 ,x))
                                                 (the fixnum (+ ,y0 ,y))))
                                      `(,red (color-buffer-element-color 
                                               red ,pixel-buffer
                                               (the fixnum (+ ,x0 ,x))
                                               (the fixnum (+ ,y0 ,y))))
                                      bindings))))))
        `(let ((,x0 (the fixnum (* ,px +horz-ppc+))) 
               (,y0 (the fixnum (* ,py +vert-ppc+))))
           (let (,@(reverse bindings))
             (let ((red-total 0) (green-total 0) (blue-total 0))
               ,@(mapcar #'(lambda (symb) `(setf red-total (the fixnum (+ red-total ,symb))))
                         red-symbols)
               ,@(mapcar #'(lambda (symb) `(setf green-total (the fixnum (+ green-total ,symb))))
                         green-symbols)
               ,@(mapcar #'(lambda (symb) `(setf blue-total (the fixnum (+ blue-total ,symb))))
                         blue-symbols)
               ,@body)))))))

(defmacro bind-blocks ((&rest block-descrs) &body body)
  (let (fixnum-symbols charname-symbols bindings)
    (dolist (block-descr block-descrs)
      (let* ((charname (first block-descr)) (char-value (second block-descr)))
        (multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue)
            (make-character-colors-symbols charname)
          (setf charname-symbols (cons charname charname-symbols)
                fixnum-symbols (list* bg-blue bg-green bg-red
                                      fg-blue fg-green fg-red
                                      fixnum-symbols)
                bindings (list* `(,bg-blue 0) `(,bg-green 0) `(,bg-red 0) 
                                `(,fg-blue 0) `(,fg-green 0) `(,fg-red 0)
                                `(,charname ,char-value) bindings)))))
    `(let (,@(reverse bindings))
       (declare (type character ,@charname-symbols)
                (type fixnum ,@fixnum-symbols))
       ,@body)))

(defmacro calc-block-colors-sum (charname components)
  (multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue)
      (make-character-colors-symbols charname)
    (let (exprs)
      (dolist (component components)
        (multiple-value-bind (red/a green/a blue/a)
            (funcall (if (symbolp component) 
                       #'make-character-colors-symbols
                       #'make-pixel-colors-symbols) component)
          (setf exprs (list* `(modify-places fixnum + (,fg-red ,fg-green ,fg-blue)
                                             (,red/a ,green/a ,blue/a))
                             exprs))))
      `(progn
         ,@(reverse exprs)
         (modify-places fixnum + (,bg-red ,bg-green ,bg-blue)
                        (((- red-total ,fg-red)) 
                         ((- green-total ,fg-green)) 
                         ((- blue-total ,fg-blue))))))))

(defmacro calc-block-colors-average (charname pixels)
  (multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue)
      (make-character-colors-symbols charname)
    `(progn
       ,(if (zerop pixels)
          `(modify-places fixnum * (,fg-red ,fg-green ,fg-blue) (0 0 0))
          `(modify-places fixnum round (,fg-red ,fg-green ,fg-blue)
                          (,pixels ,pixels ,pixels)))
       ,(if (= pixels (* +horz-ppc+ +vert-ppc+))
          `(modify-places fixnum * (,bg-red ,bg-green ,bg-blue) (0 0 0))
          `(modify-places fixnum round (,bg-red ,bg-green ,bg-blue)
                          (,(- (* +horz-ppc+ +vert-ppc+) pixels) 
                           ,(- (* +horz-ppc+ +vert-ppc+) pixels)
                           ,(- (* +horz-ppc+ +vert-ppc+) pixels)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-character-score-bindings (charname pixels)
    (multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue)
        (make-character-colors-symbols charname)
      (let (delta-bindings delta-sq-bindings score-binding score-exprs delta-sq-symbols
            (score (intern (format nil "SCORE-~A" charname))))
        (dotimes (i (* +horz-ppc+ +vert-ppc+))
          (multiple-value-bind (red green blue) 
              (make-pixel-colors-symbols i)
            (let ((delta-red (intern (format nil "DELTA-RED-~A-~A" charname i)))
                  (delta-red-sq (intern (format nil "DELTA-RED-SQ-~A-~A" charname i)))
                  (delta-green (intern (format nil "DELTA-GREEN-~A-~A" charname i)))
                  (delta-green-sq (intern (format nil "DELTA-GREEN-SQ-~A-~A" charname i)))
                  (delta-blue (intern (format nil "DELTA-BLUE-~A-~A" charname i)))
                  (delta-blue-sq (intern (format nil "DELTA-BLUE-SQ-~A-~A" charname i))))
              (setf delta-sq-symbols (list* delta-blue-sq delta-green-sq delta-red-sq 
                                            delta-sq-symbols)
                    delta-bindings 
                    (list* 
                      `(,delta-blue (the fixnum (- ,blue ,(if (member i pixels)
                                                            fg-blue bg-blue))))
                      `(,delta-green (the fixnum (- ,green ,(if (member i pixels)
                                                              fg-green bg-green))))
                      `(,delta-red (the fixnum (- ,red ,(if (member i pixels)
                                                          fg-red bg-red))))
                      delta-bindings)
                    delta-sq-bindings 
                    (list* 
                      `(,delta-blue-sq (the fixnum (* ,delta-blue ,delta-blue)))
                      `(,delta-green-sq (the fixnum (* ,delta-green ,delta-green)))
                      `(,delta-red-sq (the fixnum (* ,delta-red ,delta-red)))
                      delta-sq-bindings)))))
        (setf score-binding `(,score 0))
        (setf score-exprs (mapcar #'(lambda (delta-sq) 
                                      `(setf ,score (the fixnum (+ ,score ,delta-sq))))
                                  delta-sq-symbols))
        (values delta-bindings delta-sq-bindings score-binding score-exprs)))))

(defmacro bind-character-scores ((&rest char-descrs) &body body)
  (let (delta-bindings delta-sq-bindings scores-bindings scores-exprs)
    (dolist (char-descr char-descrs)
      (multiple-value-bind (delta-bindings/c delta-sq-bindings/c score-binding/c score-exprs/c)
          (get-character-score-bindings (first char-descr) (second char-descr))
        (setf delta-bindings (nconc delta-bindings/c delta-bindings)
              delta-sq-bindings (nconc delta-sq-bindings/c delta-sq-bindings)
              scores-bindings (list* score-binding/c scores-bindings)
              scores-exprs (nconc score-exprs/c scores-exprs))))
    `(let (,@(reverse delta-bindings))
       (let (,@(reverse delta-sq-bindings))
         (let (,@(reverse scores-bindings))
           ,@scores-exprs
           ,@body)))))

(defmacro bind-best-character-and-colors ((&rest charnames) &body body)
  `(let ((best-score most-positive-fixnum) (best-char #\Space) 
         (best-fg-red 0) (best-fg-green 0) (best-fg-blue 0)
         (best-bg-red 0) (best-bg-green 0) (best-bg-blue 0))
     (declare (type character best-char)
              (fixnum best-score best-fg-red best-fg-green best-fg-blue
                      best-bg-red best-bg-green best-bg-blue))
     ,@(mapcar #'(lambda (charname)
                   (let ((score (make-character-score-symbol charname)))
                     (multiple-value-bind (fg-red fg-green fg-blue bg-red bg-green bg-blue) 
                         (make-character-colors-symbols charname)
                       `(when (< ,score best-score)
                          (setf best-score ,score 
                                best-char ,charname
                                best-fg-red ,fg-red 
                                best-fg-green ,fg-green 
                                best-fg-blue ,fg-blue
                                best-bg-red ,bg-red 
                                best-bg-green ,bg-green 
                                best-bg-blue ,bg-blue)))))
               charnames)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-block-pixels-sum-list (mappings-sublist)
    (let (best-sum-list)
      (labels ((rec (mappings-sublist sum-list)
                 (if (null mappings-sublist)
                   (when (or (null best-sum-list)
                             (< (length sum-list) (length best-sum-list)))
                     (setf best-sum-list sum-list))
                   (progn
                     (rec (rest mappings-sublist) sum-list)
                     (when (subsetp (first (first mappings-sublist)) sum-list :test #'eql)
                       (rec (rest mappings-sublist)
                            (cons (second (first mappings-sublist))
                                  (set-difference sum-list (first (first mappings-sublist))
                                                  :test #'eql))))))))
        (rec (rest mappings-sublist) (first (first mappings-sublist)))
        best-sum-list))))

(defmacro choose-best-character-and-colors ((&rest char/pixel-mappings) pixel-buffer px py)
  (let ((char/pixel-mappings 
          (mapcar #'(lambda (mapping)
                      (list (second mapping)
                            (intern (apply #'concatenate 'string "B"
                                                 (if (null (second mapping))
                                                   '("+NULL")
                                                   (mapcar #'(lambda (pixel)
                                                               (format nil "+~A" pixel))
                                                           (second mapping)))))
                            (first mapping)))
                  (sort char/pixel-mappings #'<
                        :key #'(lambda (mapping) (length (second mapping)))))))
    `(bind-cell-colors ,pixel-buffer ,px ,py
       (bind-blocks (,@(mapcar #'rest char/pixel-mappings))
         ,@(loop :for mappings-sublist :in (reverse (loop :for ms :on (reverse char/pixel-mappings)
                                                          :collect ms))
                 :collect `(calc-block-colors-sum 
                             ,(second (first mappings-sublist))
                             ,(get-block-pixels-sum-list mappings-sublist)))
         ,@(mapcar #'(lambda (mapping) 
                       `(calc-block-colors-average
                          ,(second mapping) ,(length (first mapping))))
                   char/pixel-mappings)
         (bind-character-scores (,@(mapcar #'(lambda (mapping)
                                               `(,(second mapping) ,(first mapping)))
                                           char/pixel-mappings))
           (bind-best-character-and-colors (,@(mapcar #'second char/pixel-mappings))
             (values best-char best-fg-red best-fg-green best-fg-blue
                     best-bg-red best-bg-green best-bg-blue)))))))

(defun convert-cell-to-character (pixel-buffer px py)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* * 3)) pixel-buffer)
           (type fixnum px py))
  ;; Numeration of cell pixels:
  ;; 0 1
  ;; 2 3
  ;; 4 5
  ;; 6 7
  (choose-best-character-and-colors
    ((#\Space ()) (#\▝ (1 3)) (#\▗ (5 7)) (#\▖ (4 6)) (#\▘ (0 2)) (#\▞ (1 3 4 6)) 
                  (#\▂ (6 7)) (#\▄ (4 5 6 7)) (#\▆ (2 3 4 5 6 7)) (#\▌ (0 2 4 6)))
    pixel-buffer px py))

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
                    (convert-cell-to-character pixel-buffer px py)
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

(let ((init-string (format nil "~C[?1049h~:*~C[2J~:*~C[?7l~:*~C[?25l" #\Esc))
      (final-string (format nil "~C[?1049l~:*~C[2J~:*~C[?7h~:*~C[?25h" #\Esc))
      (clear-string (format nil "~C[2J" #\Esc))
      (home (format nil "~C[H" #\Esc))) 

  ;; switch to alt. buffer, clear screen, disable line wrap and hide cursor
  (defun initialize-terminal (stream)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream))
    (write-string init-string stream)
    (write-string home stream)
    (force-output stream)
    t)

  ;; switch to primary buffer, clear screen, enable line wrap and show cursor
  (defun finalize-terminal (stream)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream))
    (write-string final-string stream)
    (force-output stream)
    t)
  
  (defun clear-terminal (stream)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream))
    (write-string clear-string stream)
    (write-string home stream)
    (force-output stream)
    t))

(defparameter *color-change-tolerance* 3)

(let ((linefeed (format nil "~C[E" #\Esc))
      (home (format nil "~C[H" #\Esc))
      (byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 
  (defun write-terminal-buffer (stream terminal-buffer &optional (ymin 0) ymax (xmin 0) xmax)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream) (type terminal-buffer terminal-buffer))
    (macrolet ((write-terminal-color (type stream red green blue 
                                      last-red last-green last-blue)
                 `(when (or (null ,last-red) (null ,last-green) (null ,last-blue)
                            (let ((delta-red (the fixnum (abs (- ,red ,last-red))))
                                  (delta-green (the fixnum (abs (- ,green ,last-green))))
                                  (delta-blue (the fixnum (abs (- ,blue ,last-blue)))))
                              (or (> delta-red (the fixnum *color-change-tolerance*))
                                  (> delta-green (the fixnum *color-change-tolerance*))
                                  (> delta-blue (the fixnum *color-change-tolerance*))))) 
                    (setf ,last-red ,red ,last-green ,green ,last-blue ,blue)
                    (write-char #\Esc ,stream)
                    (write-string ,(ecase type (fg "[38;2;") (bg "[48;2;")) ,stream)
                    (write-string (aref byte-to-string ,red) ,stream)
                    (write-char #\; ,stream)
                    (write-string (aref byte-to-string ,green) ,stream)
                    (write-char #\; ,stream)
                    (write-string (aref byte-to-string ,blue) ,stream)
                    (write-char #\m ,stream))))
      (with-terminal-buffer-size terminal-buffer
        (let ((ymax (or ymax tb-size-y)) (xmax (or xmax tb-size-x))
              last-fg-red last-fg-green last-fg-blue last-bg-red last-bg-green last-bg-blue) 
          (declare (type fixnum ymin ymax xmin xmax))
          (loop :for y :of-type fixnum :from (max 0 ymin) :below (min ymax tb-size-y) :do
                (progn
                  (write-string (if (= y ymin) home linefeed) stream)
                  (loop :for x :of-type fixnum :from (max 0 xmin) :below (min xmax tb-size-x) :do
                        (with-terminal-buffer-element terminal-buffer x y
                          (write-terminal-color fg stream fg-red fg-green fg-blue
                                                last-fg-red last-fg-green last-fg-blue)
                          (write-terminal-color bg stream bg-red bg-green bg-blue
                                                last-bg-red last-bg-green last-bg-blue)
                          (write-char char stream))))))))
    (force-output stream)
    t))

