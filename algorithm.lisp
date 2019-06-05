;;;; algorithm.lisp

(in-package #:cl-image2text)

(defmacro character-designator (char)
  `(first ,char))

(defmacro character-itself (char)
  `(second ,char))

(defmacro character-parts (char)
  `(third ,char))

(defmacro character-pixels-count (char)
  `(fourth ,char))

(defmacro define-conversion (cell-horizontal-size cell-vertical-size cell-characters)
  (when (or (<= cell-horizontal-size 0) (<= cell-vertical-size 0)
            (/= (length cell-characters)
                (length (remove-duplicates cell-characters
                                           :key #'(lambda (chr) 
                                                    (character-designator chr)))))
            (some #'(lambda (chr) 
                      (find-if #'(lambda (i) 
                                   (or (minusp i) 
                                       (>= i (* cell-horizontal-size
                                                cell-vertical-size))))
                               (character-parts chr)))
                  cell-characters))
    (error "Incorrect image-to-text conversion specification."))

  `(progn
     (defconstant +horz-ppc+ ,cell-horizontal-size)
     (defconstant +vert-ppc+ ,cell-vertical-size)
     (defconstant +ppc+ ,(* cell-horizontal-size cell-vertical-size))
     (alexandria:define-constant 
       +characters+ 
       (quote 
         ,(let ((cell-characters 
                  (mapcar #'(lambda (chr)
                              (list (character-designator chr)
                                    (character-itself chr)
                                    (character-parts chr)
                                    (length (character-parts chr))))
                          (sort cell-characters #'< 
                                :key #'(lambda (chr)
                                         (length (character-parts chr)))))))
            (labels ((find-subchars (chr lst collected pixels)
                       (if (null lst)
                         (let ((ch (assoc (character-designator chr) cell-characters))) 
                           (when (< (+ (length collected) (length pixels))
                                    (length (character-parts ch)))
                             (setf (character-parts ch) (append collected pixels))))
                         (progn
                           (find-subchars chr (rest lst) collected pixels)
                           (when (and (not (eql (character-designator chr) 
                                                (character-designator (first lst))))
                                      (subsetp (character-pixels (first lst))
                                               (character-pixels chr) :test #'eql))
                             (find-subchars chr (rest lst) (cons (character-designator (first lst))
                                                                 collected)
                                            (set-difference pixels (character-pixels (first lst))
                                                            :test #'eql)))))))
              (let ((characters (copy-tree cell-characters)))
                (dolist (chr characters)
                  (find-subchars chr characters () (character-pixels chr)))))
            cell-characters)) 
       :test #'equal)))

; (define-conversion 2 4
;   ;; Numeration of cell pixels:
;   ;; 0 1
;   ;; 2 3
;   ;; 4 5
;   ;; 6 7
;   ((:mono #\Space ())
;    (:qu1 #\▝ (1 3)) (:qu2 #\▗ (5 7)) (:qu3 #\▖ (4 6)) (:qu4 #\▘ (0 2)) (:qu13 #\▞ (1 3 4 6)) 
;    (:lw14 #\▂ (6 7)) (:lw12 #\▄ (4 5 6 7)) (:lw34 #\▆ (2 3 4 5 6 7)) 
;    (:lf12 #\▌ (0 2 4 6))))
(define-conversion 4 8
  ;; Numeration of cell pixels:
  ;;  0  1  2  3
  ;;  4  5  6  7
  ;;  8  9 10 11
  ;; 12 13 14 15
  ;; 16 17 18 19
  ;; 20 21 22 23
  ;; 24 25 26 27
  ;; 28 29 30 31
  #. (let* ((qu1 `(2 3 6 7 10 11 14 15)) (qu2 `(18 19 22 23 26 27 30 31)) 
            (qu3 `(16 17 20 21 24 25 28 29)) (qu4 `(0 1 4 5 8 9 12 13)) 
            (qu13 `(,@qu1 ,@qu3)) 
            (lw18 `(28 29 30 31)) (lw14 `(24 25 26 27 ,@lw18)) 
            (lw38 `(20 21 22 23 ,@lw14)) (lw12 `(,@qu2 ,@qu3)) 
            (lw58 `(12 13 14 15 ,@lw12)) (lw34 `(8 9 10 11 ,@lw58))
            (lw78 `(4 5 6 7 ,@lw34))
            (lf14 `(0 4 8 12 16 20 24 28)) (lf12 `(,@qu3 ,@qu4))
            (lf34 `(,@lf12 2 6 10 14 18 22 26 30)))
       `((:mono #\Space ())
         (:qu1 #\▝ ,qu1) (:qu2 #\▗ ,qu2) (:qu3 #\▖ ,qu3) (:qu4 #\▘ ,qu4) 
         (:qu13 #\▞ ,qu13) 
         (:lw18 #\▁ ,lw18) (:lw14 #\▂ ,lw14) (:lw38 #\▃ ,lw38) (:lw12 #\▄ ,lw12) 
         (:lw58 #\▅ ,lw58) (:lw34 #\▆ ,lw34) (:lw78 #\▇ ,lw78)
         (:lf14 #\▎ ,lf14) (:lf12 #\▌ ,lf12) (:lf34 #\▊ ,lf34))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun total-sum-symbol (color power)
    (intern (concatenate 'string (symbol-name color) 
                         (if (= power 2) "-SQ" "") "-SUM")))

  (defun character-sum-symbol (designator color power bg)
    (intern (concatenate 'string (symbol-name designator) "-"
                         (symbol-name color) (if (= power 2) "-SQ" "")
                         "-SUM" (if bg "-BG" "-FG"))))

  (defun character-mean-symbol (designator color bg)
    (intern (concatenate 'string (symbol-name designator) "-"
                         (symbol-name color) "-MEAN" (if bg "-BG" "-FG"))))

  (defun character-score-symbol (designator)
    (intern (concatenate 'string (symbol-name designator) "-SCORE")))

  (defun character-variables-bindings ()
    (nconc
      (loop :for chr :in +characters+ :nconc 
            (nconc 
              (loop :for color :in '(:red :green :blue) :nconc
                    (loop :for power :in '(1 2) :nconc
                          (loop :for bg :in '(nil t) :collect
                                `(,(character-sum-symbol (character-designator chr)
                                                         color power bg) 0))))
              (list `(,(character-score-symbol (character-designator chr)) 0))))
      (loop :for chr :in +characters+ :nconc 
            (loop :for color :in '(:red :green :blue) :nconc
                  (loop :for bg :in '(nil t) :collect
                        (character-mean-symbol (character-designator chr) color bg))))
      (loop :for color :in '(:red :green :blue) :nconc
            (loop :for power :in '(1 2) :collect
                  `(,(total-sum-symbol color power) 0)))))

  (defun color-access-form (pixel-buffer px py index color power)
    (let ((form `(pixel-buffer-color 
                   ,color ,pixel-buffer 
                   (the fixnum (+ ,px ,(mod index +horz-ppc+)))
                   (the fixnum (+ ,py ,(floor index +horz-ppc+))))))
      (if (= power 2)
        `(let ((value ,form)) (the fixnum (* value value)))
        form)))

  (defun total-color-sums-calculation (pixel-buffer px py)
    (list
      `(dotimes (y ,+vert-ppc+)
         (dotimes (x ,+horz-ppc+)
           (let* ((red (pixel-buffer-color :red ,pixel-buffer (+ ,px x) (+ ,py y)))
                  (red-sq (the fixnum (* red red)))
                  (green (pixel-buffer-color :green ,pixel-buffer (+ ,px x) (+ ,py y)))
                  (green-sq (the fixnum (* green green)))
                  (blue (pixel-buffer-color :blue ,pixel-buffer (+ ,px x) (+ ,py y)))
                  (blue-sq (the fixnum (* blue blue))))
             (setf red-sum (the fixnum (+ red-sum red))
                   red-sq-sum (the fixnum (+ red-sq-sum red-sq))
                   green-sum (the fixnum (+ green-sum green))
                   green-sq-sum (the fixnum (+ green-sq-sum green-sq))
                   blue-sum (the fixnum (+ blue-sum blue))
                   blue-sq-sum (the fixnum (+ blue-sq-sum blue-sq))))))))

  (defun character-color-sums-calculation (pixel-buffer px py designator color power)
    (let ((sym (character-sum-symbol designator color power nil))) 
      (nconc (loop :for part :in (character-parts (assoc designator +characters+))
                   :collect `(setf ,sym 
                                   (the fixnum 
                                        (+ ,sym
                                           ,(if (integerp part)
                                              (color-access-form pixel-buffer px py part color power)
                                              (character-sum-symbol part color power nil))))))
             (list `(setf ,(character-sum-symbol designator color power t)
                          (the fixnum (- ,(total-sum-symbol color power) ,sym)))))))

  (defun character-scores-calculation (pixel-buffer px py)
    (loop :for chr :in +characters+ :nconc 
          (let ((designator (character-designator chr))) 
            (loop :for color :in '(:red :green :blue) :nconc
                  (nconc
                    (loop :for power :in '(1 2) :nconc
                          (character-color-sums-calculation pixel-buffer px py designator color power))
                    (list 
                      `(setf ,(character-mean-symbol designator color nil)
                             (the fixnum ,(if (zerop (character-pixels-count chr)) 0
                                            `(floor ,(character-sum-symbol designator color 1 nil)
                                                    ,(character-pixels-count chr))))
                             ,(character-mean-symbol designator color t)
                             (the fixnum ,(if (= (character-pixels-count chr) +ppc+) 0
                                            `(floor ,(character-sum-symbol designator color 1 t)
                                                    ,(- +ppc+ (character-pixels-count chr)))))
                             ,(character-score-symbol designator)
                             (let* ((sum-sq-fg 
                                      (the fixnum 
                                           (* ,(character-sum-symbol designator color 1 nil)
                                              ,(character-mean-symbol designator color nil))))
                                    (sum-sq-bg 
                                      (the fixnum 
                                           (* ,(character-sum-symbol designator color 1 t)
                                              ,(character-mean-symbol designator color t))))
                                    (score-delta-fg 
                                      (the fixnum 
                                           (- ,(character-sum-symbol designator color 2 nil)
                                              sum-sq-fg)))
                                    (score-delta-bg 
                                      (the fixnum 
                                           (- ,(character-sum-symbol designator color 2 t)
                                              sum-sq-bg)))
                                    (score-delta (the fixnum (+ score-delta-fg score-delta-bg))))
                               (the fixnum (+ ,(character-score-symbol designator) score-delta))))))))))

  (defun character-scores-comparison ()
    (list
      `(let ((best-score most-positive-fixnum))
         ,@(loop :for chr :in +characters+ :collect
                 `(when (< ,(character-score-symbol (character-designator chr))
                           best-score)
                    (setf best-score ,(character-score-symbol (character-designator chr))
                          best-char ,(character-itself chr)
                          best-fg-red ,(character-mean-symbol 
                                         (character-designator chr) :red nil) 
                          best-fg-green ,(character-mean-symbol 
                                           (character-designator chr) :green nil) 
                          best-fg-blue ,(character-mean-symbol 
                                          (character-designator chr) :blue nil) 
                          best-bg-red ,(character-mean-symbol 
                                         (character-designator chr) :red t) 
                          best-bg-green ,(character-mean-symbol 
                                           (character-designator chr) :green t) 
                          best-bg-blue ,(character-mean-symbol 
                                          (character-designator chr) :blue t)))))))

  (defmacro with-best-character (pixel-buffer px py &body body)
    `(let (best-char best-fg-red best-fg-green best-fg-blue best-bg-red best-bg-green best-bg-blue)
       (let (,@(character-variables-bindings))
         ,@(total-color-sums-calculation pixel-buffer px py)
         ,@(character-scores-calculation pixel-buffer px py)
         ,@(character-scores-comparison)
         ,@body)))

  (defun convert-cell-to-character (pixel-buffer px py)
    (declare (optimize (speed 3) (safety 0))
             (type pixel-buffer pixel-buffer)
             (type fixnum px py))
    (with-best-character pixel-buffer (the fixnum (* px +horz-ppc+)) (the fixnum (* py +vert-ppc+))
      (values best-char best-fg-red best-fg-green best-fg-blue
              best-bg-red best-bg-green best-bg-blue))))

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

