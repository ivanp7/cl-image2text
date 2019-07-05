;;;; algorithm.lisp

(in-package #:cl-image2text)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun total-sum-symbol (color power)
    (alexandria:symbolicate color (if (= power 2) "-SQ" "") "-SUM"))

  (defun character-sum-symbol (designator color power bg)
    (alexandria:symbolicate designator "-" color (if (= power 2) "-SQ" "")
                            "-SUM" (if bg "-BG" "-FG")))

  (defun character-mean-symbol (designator color bg)
    (alexandria:symbolicate designator "-" color "-MEAN" (if bg "-BG" "-FG")))

  (defun character-score-symbol (designator)
    (alexandria:symbolicate designator "-SCORE"))

  (defun character-variables-bindings ()
    (nconc
      (loop :for chr :in +characters+ :nconc 
            (nconc 
              (loop :for color :in '(:red :green :blue) :nconc
                    (loop :for power :in '(1 2) :nconc
                          (loop :for bg :in '(nil t) :collect
                                `(,(character-sum-symbol 
                                     (character-designator chr)
                                     color power bg) 0))))
              (list `(,(character-score-symbol 
                         (character-designator chr)) 0))))
      (loop :for chr :in +characters+ :nconc 
            (loop :for color :in '(:red :green :blue) :nconc
                  (loop :for bg :in '(nil t) :collect
                        `(,(character-mean-symbol 
                             (character-designator chr) color bg) 0))))
      (loop :for color :in '(:red :green :blue) :nconc
            (loop :for power :in '(1 2) :collect
                  `(,(total-sum-symbol color power) 0)))))

  (defun color-access-form (pixel-buffer px py index color power)
    (let ((form `(pixel-buffer-color 
                   ,color ,pixel-buffer 
                   (the fixnum (+ ,px ,(mod index +horz-ppc+)))
                   (the fixnum (+ ,py ,(floor index +horz-ppc+))))))
      (if (= power 2)
        `(let ((value ,form)) (the score (* value value)))
        form)))

  (defun total-color-sums-calculation (pixel-buffer px py)
    (list
      `(dotimes (y ,+vert-ppc+)
         (dotimes (x ,+horz-ppc+)
           (let* ((red (pixel-buffer-color :red ,pixel-buffer 
                                           (+ ,px x) (+ ,py y)))
                  (red-sq (the score (* red red)))
                  (green (pixel-buffer-color :green ,pixel-buffer 
                                             (+ ,px x) (+ ,py y)))
                  (green-sq (the score (* green green)))
                  (blue (pixel-buffer-color :blue ,pixel-buffer 
                                            (+ ,px x) (+ ,py y)))
                  (blue-sq (the score (* blue blue))))
             (setf red-sum (the score (+ red-sum red))
                   red-sq-sum (the score (+ red-sq-sum red-sq))
                   green-sum (the score (+ green-sum green))
                   green-sq-sum (the score (+ green-sq-sum green-sq))
                   blue-sum (the score (+ blue-sum blue))
                   blue-sq-sum (the score (+ blue-sq-sum blue-sq))))))))

  (defun character-color-sums-calculation (pixel-buffer px py 
                                           designator color power)
    (let ((sym (character-sum-symbol designator color power nil))) 
      (nconc (loop :for part :in (character-parts (assoc designator 
                                                         +characters+))
                   :collect 
                   `(setf ,sym 
                          (the score 
                               (+ ,sym
                                  ,(if (integerp part)
                                     (color-access-form pixel-buffer px py 
                                                        part color power)
                                     (character-sum-symbol 
                                       part color power nil))))))
             (list `(setf ,(character-sum-symbol designator color power t)
                          (the score (- ,(total-sum-symbol color power) 
                                        ,sym)))))))

  (defun character-scores-calculation (pixel-buffer px py)
    (loop :for chr :in +characters+ :nconc 
          (let ((designator (character-designator chr))) 
            (loop :for color :in '(:red :green :blue) :nconc
                  (nconc
                    (loop :for power :in '(1 2) :nconc
                          (character-color-sums-calculation 
                            pixel-buffer px py designator color power))
                    (when (plusp (character-pixels-count chr))
                      (list `(setf ,(character-mean-symbol 
                                      designator color nil)
                                   (the color 
                                        (floor ,(character-sum-symbol 
                                                  designator color 1 nil)
                                               ,(character-pixels-count 
                                                  chr))))))
                    (when (< (character-pixels-count chr) +ppc+)
                      (list `(setf ,(character-mean-symbol 
                                      designator color t)
                                   (the color 
                                        (floor ,(character-sum-symbol 
                                                  designator color 1 t)
                                               ,(- +ppc+ 
                                                   (character-pixels-count 
                                                     chr)))))))
                    (list `(setf ,(character-score-symbol designator)
                                 (let* ((sum-sq-fg 
                                          (the score 
                                               (* ,(character-sum-symbol 
                                                     designator color 1 nil)
                                                  ,(character-mean-symbol 
                                                     designator color nil))))
                                        (sum-sq-bg 
                                          (the score 
                                               (* ,(character-sum-symbol 
                                                     designator color 1 t)
                                                  ,(character-mean-symbol 
                                                     designator color t))))
                                        (score-delta-fg 
                                          (the score 
                                               (- ,(character-sum-symbol 
                                                     designator color 2 nil)
                                                  sum-sq-fg)))
                                        (score-delta-bg 
                                          (the score 
                                               (- ,(character-sum-symbol 
                                                     designator color 2 t)
                                                  sum-sq-bg)))
                                        (score-delta (the score 
                                                          (+ score-delta-fg 
                                                             score-delta-bg))))
                                   (the score (+ ,(character-score-symbol 
                                                    designator) 
                                                 score-delta))))))))))

  (defun character-scores-comparison ()
    (list
      `(let ((best-score most-positive-fixnum))
         ,@(loop :for chr :in +characters+ :collect
                 `(when (< ,(character-score-symbol (character-designator chr))
                           best-score)
                    (setf best-score ,(character-score-symbol 
                                        (character-designator chr))
                          best-char ,(character-itself chr)
                          best-fg-red ,(character-mean-symbol 
                                         (character-designator chr) 
                                         :red nil) 
                          best-fg-green ,(character-mean-symbol 
                                           (character-designator chr) 
                                           :green nil) 
                          best-fg-blue ,(character-mean-symbol 
                                          (character-designator chr) 
                                          :blue nil) 
                          best-bg-red ,(character-mean-symbol 
                                         (character-designator chr) 
                                         :red t) 
                          best-bg-green ,(character-mean-symbol 
                                           (character-designator chr) 
                                           :green t) 
                          best-bg-blue ,(character-mean-symbol 
                                          (character-designator chr) 
                                          :blue t)))))))

  (defmacro with-best-character (pixel-buffer px py &body body)
    `(let (best-char best-fg-red best-fg-green best-fg-blue 
           best-bg-red best-bg-green best-bg-blue)
       (let (,@(character-variables-bindings))
         ,@(total-color-sums-calculation pixel-buffer px py)
         ,@(character-scores-calculation pixel-buffer px py)
         ,@(character-scores-comparison)
         ,@body)))

  (defun convert-cell-to-character (pixel-buffer px py)
    (declare (optimize (speed 3) (safety 0))
             (type pixel-buffer pixel-buffer)
             (type fixnum px py))
    (with-best-character pixel-buffer (the fixnum (* px +horz-ppc+)) 
                         (the fixnum (* py +vert-ppc+))
      (values best-char best-fg-red best-fg-green best-fg-blue
              best-bg-red best-bg-green best-bg-blue))))

(defun convert-pixels/single-thread (pixel-buffer terminal-buffer 
                                     &optional pmin pmax)
  "Convert pixel buffer to terminal text buffer using a single thread."
  (declare (optimize (speed 3) (safety 0))
           (type pixel-buffer pixel-buffer)
           (type terminal-buffer terminal-buffer)
           (type (or fixnum null) pmin pmax))
  (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
    (let ((pmin (or pmin 0)) (pmax (or pmax (the fixnum (* tb-size-x 
                                                           tb-size-y)))))
      (loop :for p :of-type fixnum :from pmin :below pmax :do
            (let* ((py (the fixnum (floor p tb-size-x)))
                   (px (the fixnum (mod p tb-size-x))))
              (multiple-value-bind (char-value 
                                    fg-red-value fg-green-value fg-blue-value 
                                    bg-red-value bg-green-value bg-blue-value)
                (convert-cell-to-character pixel-buffer px py)
                (declare (type character char-value)
                         (type color fg-red-value fg-green-value fg-blue-value 
                               bg-red-value bg-green-value bg-blue-value))
                (with-terminal-buffer-element terminal-buffer px py
                  (setf char char-value 
                        fg-red fg-red-value fg-green fg-green-value 
                        fg-blue fg-blue-value bg-red bg-red-value 
                        bg-green bg-green-value bg-blue bg-blue-value))))))))

;;; PARALLEL PROCESSING OF A TERMINAL BUFFER

(defun convert-pixels (pixel-buffer terminal-buffer 
                       &optional number-of-threads)
  "Convert pixel buffer to terminal text buffer using multiple threads.
  If NUMBER-OF-THREADS is not given, use one thread per processor core."
  (declare (optimize (speed 3) (safety 0))
           (type pixel-buffer pixel-buffer)
           (type terminal-buffer terminal-buffer))
  (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
    (let* ((number-of-threads (or number-of-threads 
                                  #.(cl-cpus:get-number-of-processors)))
           (threads (make-array (1- number-of-threads) 
                                :element-type '(or null bt:thread)
                                :initial-element nil))
           (tb-size (the fixnum (* tb-size-x tb-size-y))))
      (declare (type fixnum number-of-threads))
      (dotimes (thread-i (1- number-of-threads))
        (declare (type fixnum thread-i))
        (let* ((prod1 (the fixnum (* thread-i tb-size)))
               (pmin (the fixnum (floor prod1 number-of-threads)))
               (prod2 (the fixnum (+ prod1 tb-size)))
               (pmax (the fixnum (floor prod2 number-of-threads))))
          (setf (svref threads thread-i) 
                (bt:make-thread 
                  (lambda () 
                    (convert-pixels/single-thread 
                      pixel-buffer terminal-buffer pmin pmax))))))
      (prog1
        (let* ((prod1 (the fixnum (* (1- number-of-threads) tb-size)))
               (pmin (the fixnum (floor prod1 number-of-threads))))
          (convert-pixels/single-thread pixel-buffer terminal-buffer pmin))
        (dotimes (thread-i (1- number-of-threads))
          (declare (type fixnum thread-i))
          (bt:join-thread (svref threads thread-i)))))))

(defun convert-image-to-text (pixel-buffer &optional number-of-threads)
  "Convert pixel buffer to the freshly created terminal text buffer using 
  multiple threads.
  If NUMBER-OF-THREADS is not given, use one thread per processor core."
  (with-pixel-buffer-size (px py) pixel-buffer
    (let ((terminal-buffer (create-terminal-buffer (floor px +horz-ppc+)
                                                   (floor py +vert-ppc+))))
      (convert-pixels pixel-buffer terminal-buffer number-of-threads)
      terminal-buffer)))

