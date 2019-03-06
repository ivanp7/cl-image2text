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

(defun ansi-cc-string (colchar)
  (format nil "~C[38;2;~A;~A;~Am~C[48;2;~A;~A;~Am~A" 
          #\Esc 
          (red (cc-fg-color colchar))
          (green (cc-fg-color colchar))
          (blue (cc-fg-color colchar))
          #\Esc 
          (red (cc-bg-color colchar))
          (green (cc-bg-color colchar))
          (blue (cc-bg-color colchar))
          (cc-char colchar)))

(defun ansi-set-position (point)
  (format nil "~C[~A;~AH" #\Esc (1+ (point-y point)) (1+ (point-x point))))

(defun ansi-shift-position (delta)
  (concatenate 'string 
               (if (zerop (point-x delta))
                 ""
                 (format nil "~C[~A~A"
                         #\Esc (point-x delta) 
                         (if (plusp (point-x delta)) #\C #\D)))
               (if (zerop (point-y delta))
                 ""
                 (format nil "~C[~A~A"
                         #\Esc (point-y delta) 
                         (if (plusp (point-y delta)) #\B #\A)))))

