;;;; algorithm.lisp

(in-package #:cl-image2text)

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
                      bindings (list* `(,blue (pixel-buffer-color 
                                                blue ,pixel-buffer
                                                (the fixnum (+ ,x0 ,x)) 
                                                (the fixnum (+ ,y0 ,y))))
                                      `(,green (pixel-buffer-color 
                                                 green ,pixel-buffer
                                                 (the fixnum (+ ,x0 ,x))
                                                 (the fixnum (+ ,y0 ,y))))
                                      `(,red (pixel-buffer-color 
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
           (type pixel-buffer pixel-buffer)
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

(defun convert-pixels/single-thread (pixel-buffer terminal-buffer &optional pmin pmax)
  (declare (optimize (speed 3) (safety 0))
           (type pixel-buffer pixel-buffer)
           (type terminal-buffer terminal-buffer)
           (type (or fixnum null) pmin pmax))
  (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
    (let ((pmin (or pmin 0)) (pmax (or pmax (the fixnum (* tb-size-x tb-size-y)))))
      (loop :for p :of-type fixnum :from pmin :below pmax :do
            (let* ((py (the fixnum (floor p tb-size-x)))
                   (px (the fixnum (mod p tb-size-x))))
              (multiple-value-bind (char-value 
                                    fg-red-value fg-green-value fg-blue-value 
                                    bg-red-value bg-green-value bg-blue-value)
                (convert-cell-to-character pixel-buffer px py)
                (declare (type character char-value)
                         (type fixnum fg-red-value fg-green-value fg-blue-value 
                               bg-red-value bg-green-value bg-blue-value))
                (with-terminal-buffer-element terminal-buffer px py
                  (setf char char-value 
                        fg-red fg-red-value fg-green fg-green-value fg-blue fg-blue-value
                        bg-red bg-red-value bg-green bg-green-value bg-blue bg-blue-value))))))))

;;; PARALLEL PROCESSING OF A TERMINAL BUFFER

(defun convert-pixels (pixel-buffer terminal-buffer)
  (declare (optimize (speed 3) (safety 0))
           (type pixel-buffer pixel-buffer)
           (type terminal-buffer terminal-buffer))
  (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
    (let* ((number-of-threads #.(cl-cpus:get-number-of-processors))
           (threads (make-array (1- number-of-threads) :element-type '(or null bt:thread)
                                :initial-element nil))
           (tb-size (the fixnum (* tb-size-x tb-size-y))))
      (dotimes (thread-i (1- number-of-threads))
        (declare (type fixnum thread-i))
        (let* ((prod1 (the fixnum (* thread-i tb-size)))
               (pmin (the fixnum (floor prod1 number-of-threads)))
               (prod2 (the fixnum (+ prod1 tb-size)))
               (pmax (the fixnum (floor prod2 number-of-threads))))
          (setf (svref threads thread-i) 
                (bt:make-thread 
                  #'(lambda () 
                      (convert-pixels/single-thread pixel-buffer terminal-buffer pmin pmax))))))
      (prog1
        (let* ((prod1 (the fixnum (* (1- number-of-threads) tb-size)))
               (pmin (the fixnum (floor prod1 number-of-threads))) 
               (pmax tb-size))
          (convert-pixels/single-thread pixel-buffer terminal-buffer pmin pmax))
        (dotimes (thread-i (1- number-of-threads))
          (declare (type fixnum thread-i))
          (bt:join-thread (svref threads thread-i)))))))

(defun convert-image-to-text (pixel-buffer)
  (with-pixel-buffer-size (px py) pixel-buffer
    (let ((terminal-buffer (create-terminal-buffer (floor px +horz-ppc+)
                                                   (floor py +vert-ppc+))))
      (convert-pixels pixel-buffer terminal-buffer)
      terminal-buffer)))

