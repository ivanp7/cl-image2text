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

(let ((byte-to-string 
        (make-array 256 :element-type 'string
                        :initial-contents
                        (loop :for value :below 256
                            :collect (format nil "~A" value))))) 
  (defun write-buffer-pair-element (stream char fg-red fg-green fg-blue bg-red bg-green bg-blue)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream)
             (type character char)
             (type (unsigned-byte 8) fg-red fg-green fg-blue bg-red bg-green bg-blue))
    (write-char #\Esc stream)
    (write-string "[38;2;" stream)
    (write-string (the string (aref byte-to-string fg-red)) stream)
    (write-char #\; stream)
    (write-string (the string (aref byte-to-string fg-green)) stream)
    (write-char #\; stream)
    (write-string (the string (aref byte-to-string fg-blue)) stream)
    (write-char #\m stream)
    (write-char #\Esc stream)
    (write-string "[48;2;" stream)
    (write-string (the string (aref byte-to-string bg-red)) stream)
    (write-char #\; stream)
    (write-string (the string (aref byte-to-string bg-green)) stream)
    (write-char #\; stream)
    (write-string (the string (aref byte-to-string bg-blue)) stream)
    (write-char #\m stream)
    (write-char char stream)))

(let ((linefeed (format nil "~C[E" #\Esc))
      (home (format nil "~C[H" #\Esc))) 
  (defun write-buffer-pair (buffer-pair stream)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream)
             (type cons buffer-pair))
    (let ((size (array-dimensions (char-buffer buffer-pair))))
      (dotimes (y (second size))
        (declare (type fixnum y))
        (write-string (if (zerop y) home linefeed) stream)
        (dotimes (x (first size))
          (declare (type fixnum x))
          (with-buffer-pair-element buffer-pair x y
            (write-buffer-pair-element 
              stream char
              fg-red fg-green fg-blue
              bg-red bg-green bg-blue))))
      (force-output stream)
      t)))

