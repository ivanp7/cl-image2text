;;;; ansi.lisp

(in-package #:traytr)

;; switch to alt. buffer, clear screen, hide cursor and move to (1,1)
(alexandria:define-constant +ansi-initialization-string+ 
             (format nil "~C[?1049h~:*~C[2J~:*~C[?25l~:*~C[H" #\Esc)
             :test #'equal)

;; switch to primary buffer and show cursor
(alexandria:define-constant +ansi-finalization-string+ 
             (format nil "~C[?1049l~:*~C[2J~:*~C[?25h" #\Esc)
             :test #'equal)

(defun cc-to-string (cc)
  (format nil "~C[38;2;~A;~A;~Am~C[48;2;~A;~A;~Am~A" 
          #\Esc 
          (red (fg-color cc)) (green (fg-color cc)) (blue (fg-color cc))
          #\Esc 
          (red (bg-color cc)) (green (bg-color cc)) (blue (bg-color cc))
          (chr cc)))

(defun cc-buffer-to-string (buffer)
  )
