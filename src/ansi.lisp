;;;; ansi.lisp

(in-package #:traytr)

;; switch to alt. buffer, clear screen, hide cursor, disable line wrap and move to (1,1)
(alexandria:define-constant +ansi-initialization-string+ 
             (format nil "~C[?1049h~:*~C[2J~:*~C[?25l~:*~C[?7l~:*~C[H" #\Esc)
             :test #'equal)

;; switch to primary buffer, clear screen, enable line wrap and show cursor
(alexandria:define-constant +ansi-finalization-string+ 
             (format nil "~C[?1049l~:*~C[2J~:*~C[?7h~:*~C[?25h" #\Esc)
             :test #'equal)

(defun buffer-pair-element-to-string (char fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (format nil "~C[38;2;~A;~A;~Am~C[48;2;~A;~A;~Am~A" 
          #\Esc fg-red fg-green fg-blue #\Esc bg-red bg-green bg-blue char))

(defun buffer-pair-to-string (buffer-pair)
  (let ((size (array-dimensions (char-buffer buffer-pair)))
        (prefix (format nil "~C[H" #\Esc))
        (linefeed (format nil "~C[E")))
    ))
