;;;; constants.lisp

(in-package #:cl-image2text)

(defmacro character-designator (char)
  `(first ,char))

(defmacro character-itself (char)
  `(second ,char))

(defmacro character-parts (char)
  `(third ,char))

(defmacro character-pixels-count (char)
  `(fourth ,char))

(defmacro define-conversion (cell-horizontal-size cell-vertical-size 
                             cell-characters)
  "Define size of pixel area (cell) to be converted to a single character,
  specify used characters and theirs corresponding pixels within the area.

  CELL-CHARACTERS is a list of (KEYWORD CHARACTER (PIXEL-NO1 PIXEL-NO2 ...)),
  where pixels are numerated starting from zero, left-to-right, up-to-down."
  (when (or (<= cell-horizontal-size 0) (<= cell-vertical-size 0)
            (/= (length cell-characters)
                (length (remove-duplicates 
                          cell-characters
                          :key (lambda (chr) 
                                 (character-designator chr)))))
            (some (lambda (chr) 
                    (find-if (lambda (i) 
                               (or (minusp i) 
                                   (>= i (* cell-horizontal-size
                                            cell-vertical-size))))
                             (character-parts chr)))
                  cell-characters))
    (error "Incorrect image-to-text conversion specification."))

  `(progn
     (defconstant +horz-ppc+ ,cell-horizontal-size
                  "Width of a pixel cell.")
     (defconstant +vert-ppc+ ,cell-vertical-size
                  "Height of a pixel cell.")
     (defconstant +ppc+ ,(* cell-horizontal-size cell-vertical-size)
                  "Number of pixels in a pixel cell.")
     (alexandria:define-constant 
       +characters+ 
       (quote 
         ,(let* ((cell-characters 
                   (sort (mapcar (lambda (chr)
                                   (list (character-designator chr)
                                         (character-itself chr)
                                         (character-parts chr)
                                         (length (character-parts chr))))
                                 cell-characters)
                         #'< :key (lambda (chr)
                                    (character-pixels-count chr)))))
            (labels ((find-subchars (designator lst collected pixels)
                       (if (null lst)
                         (let ((ch (assoc designator cell-characters))) 
                           (when (< (+ (length collected) (length pixels))
                                    (length (character-parts ch)))
                             (setf (character-parts ch) 
                                   (append collected pixels))))
                         (progn
                           (find-subchars designator (rest lst) 
                                          collected pixels)
                           (when (and (not (eql designator 
                                                (character-designator 
                                                  (first lst))))
                                      (subsetp (character-parts (first lst))
                                               pixels :test #'eql))
                             (find-subchars designator (rest lst) 
                                            (cons (character-designator 
                                                    (first lst)) collected)
                                            (set-difference 
                                              pixels (character-parts 
                                                       (first lst))
                                              :test #'eql)))))))
              (let ((chars (copy-tree cell-characters)))
                (dolist (chr chars)
                  (find-subchars (character-designator chr) chars () 
                                 (character-parts chr)))))
            cell-characters)) 
       :test #'equal)))

