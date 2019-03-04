;;;; traytr.lisp

(in-package #:traytr)

(defun point (x y)
  (cons x y))

(defvar *character-resolution* (point 8 8))

(defvar *characters* nil)

(defun define-terminal-character (chr kwrd &optional (char-resolution (point 1 1)))
  (setf *characters* (acons kwrd chr *characters*)))

(define-terminal-character #\Space :empty) ;U+0020
(define-terminal-character #\▁ :lower-1/8 (point 1 8)) ;U+2581
(define-terminal-character #\▂ :lower-2/8 (point 1 4)) ;U+2582
(define-terminal-character #\▃ :lower-3/8 (point 1 8)) ;U+2583
(define-terminal-character #\▄ :lower-4/8 (point 1 2)) ;U+2584
(define-terminal-character #\▅ :lower-5/8 (point 1 8)) ;U+2585
(define-terminal-character #\▆ :lower-6/8 (point 1 4)) ;U+2586
(define-terminal-character #\▇ :lower-7/8 (point 1 8)) ;U+2587
(define-terminal-character #\█ :full) ;U+2588
(define-terminal-character #\▉ :left-7/8 (point 8 1)) ;U+2589
(define-terminal-character #\▊ :left-6/8 (point 4 1)) ;U+258A
(define-terminal-character #\▋ :left-5/8 (point 8 1)) ;U+258B
(define-terminal-character #\▌ :left-4/8 (point 2 1)) ;U+258C
(define-terminal-character #\▍ :left-3/8 (point 8 1)) ;U+258D
(define-terminal-character #\▎ :left-2/8 (point 4 1)) ;U+258E
(define-terminal-character #\▏ :left-1/8 (point 1 1)) ;U+258F

(define-terminal-character #\▖ :lowerleft (point 2 2)) ;U+2596
(define-terminal-character #\▗ :lowerright (point 2 2)) ;U+2597
(define-terminal-character #\▘ :upperleft (point 2 2)) ;U+2598
(define-terminal-character #\▝ :upperright (point 2 2)) ;U+259D

(define-terminal-character #\▙ :inv-upperright (point 2 2)) ;U+2599
(define-terminal-character #\▛ :inv-lowerright (point 2 2)) ;U+259B
(define-terminal-character #\▜ :inv-lowerleft (point 2 2)) ;U+259C
(define-terminal-character #\▟ :inv-upperleft (point 2 2)) ;U+259F

(define-terminal-character #\▚ :upperleft-lowerright (point 2 2)) ;U+259A
(define-terminal-character #\▞ :lowerleft-upperright (point 2 2)) ;U+259E

(define-terminal-character #\░ :full-light) ;U+2591
(define-terminal-character #\▒ :full-medium) ;U+2592
(define-terminal-character #\▓ :full-dark) ;U+2593

