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

(defmacro define-io-server-function/with-ansi (name &body body)
  `(define-io-server-function ,name
     (write-string +initial-string+ stream)
     (force-output stream)
     ,@body
     (write-string +final-string+ stream)
     (force-output stream)))

(defun buffer-pair-element-to-string (char fg-red fg-green fg-blue bg-red bg-green bg-blue)
  (format nil "~C[38;2;~A;~A;~Am~C[48;2;~A;~A;~Am~A" 
          #\Esc fg-red fg-green fg-blue #\Esc bg-red bg-green bg-blue char))

(defun write-buffer-pair (buffer-pair stream)
  (declare (type stream stream))
  (let ((size (array-dimensions (char-buffer buffer-pair)))
        (linefeed (format nil "~C[E" #\Esc))
        (home (format nil "~C[H" #\Esc)))
    (dotimes (y (second size))
      (write-string (if (zerop y) home linefeed) stream)
      (dotimes (x (first size))
        (write-string (with-buffer-pair-element buffer-pair x y
                        (buffer-pair-element-to-string 
                          char
                          fg-red fg-green fg-blue
                          bg-red bg-green bg-blue))
                      stream)))
    (force-output stream)
    t))

