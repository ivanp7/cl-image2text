;;;; ansi.lisp

(in-package #:traytr)

;; switch to alt. buffer, clear screen, hide cursor, disable line wrap and move to (1,1)
(alexandria:define-constant +initial-string+ 
             (format nil "~C[?1049h~:*~C[2J~:*~C[?25l~:*~C[?7l~:*~C[H" #\Esc)
             :test #'equal)

;; switch to primary buffer, clear screen, enable line wrap and show cursor
(alexandria:define-constant +final-string+ 
             (format nil "~C[?1049l~:*~C[2J~:*~C[?7h~:*~C[?25h" #\Esc)
             :test #'equal)

(defun buffer-pair-element-to-string (char fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (format nil "~C[38;2;~A;~A;~Am~C[48;2;~A;~A;~Am~A" 
          #\Esc fg-red fg-green fg-blue #\Esc bg-red bg-green bg-blue char))

(defun buffer-pair-to-string (buffer-pair)
  (let ((size (array-dimensions (char-buffer buffer-pair)))
        (linefeed (format nil "~C[E" #\Esc))
        (home (format nil "~C[H" #\Esc))
        (result ""))
    (loop :for y :below (second size) :do 
          (setf result
                (concatenate 
                  'string
                  result
                  (let ((line (if (zerop y) home linefeed)))
                    (loop :for x :below (first size) :do
                          (setf line
                                (concatenate
                                  'string
                                  line
                                  (with-buffer-pair-element 
                                    buffer-pair x y
                                    (buffer-pair-element-to-string 
                                      char 
                                      fg-red fg-green fg-blue
                                      bg-red bg-green bg-blue)))))
                    line))))
    result))

