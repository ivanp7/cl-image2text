;;;; ansi.lisp

(in-package #:traytr)

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

