;;;; conversion.lisp

(in-package #:cl-image2text)

;; Much faster results
; (define-conversion 2 4
;   ;; Numeration of cell pixels:
;   ;; 0 1
;   ;; 2 3
;   ;; 4 5
;   ;; 6 7
;   ((:mono #\Space ())
;    (:qu1 #\▝ (1 3)) (:qu2 #\▗ (5 7)) (:qu3 #\▖ (4 6)) (:qu4 #\▘ (0 2)) 
;    (:qu13 #\▞ (1 3 4 6)) 
;    (:lw14 #\▂ (6 7)) (:lw12 #\▄ (4 5 6 7)) (:lw34 #\▆ (2 3 4 5 6 7)) 
;    (:lf12 #\▌ (0 2 4 6))))

;; A little better results, but slower
(define-conversion 4 8
  ;; Numeration of cell pixels:
  ;;  0  1  2  3
  ;;  4  5  6  7
  ;;  8  9 10 11
  ;; 12 13 14 15
  ;; 16 17 18 19
  ;; 20 21 22 23
  ;; 24 25 26 27
  ;; 28 29 30 31
  #.(let* ((qu1 `(2 3 6 7 10 11 14 15)) (qu2 `(18 19 22 23 26 27 30 31)) 
           (qu3 `(16 17 20 21 24 25 28 29)) (qu4 `(0 1 4 5 8 9 12 13)) 
           (qu13 `(,@qu1 ,@qu3)) 
           (lw18 `(28 29 30 31)) (lw14 `(24 25 26 27 ,@lw18)) 
           (lw38 `(20 21 22 23 ,@lw14)) (lw12 `(,@qu2 ,@qu3)) 
           (lw58 `(12 13 14 15 ,@lw12)) (lw34 `(8 9 10 11 ,@lw58))
           (lw78 `(4 5 6 7 ,@lw34))
           (lf14 `(0 4 8 12 16 20 24 28)) (lf12 `(,@qu3 ,@qu4))
           (lf34 `(,@lf12 2 6 10 14 18 22 26 30)))
      `((:mono #\Space ())
        (:qu1 #\▝ ,qu1) (:qu2 #\▗ ,qu2) (:qu3 #\▖ ,qu3) (:qu4 #\▘ ,qu4) 
        (:qu13 #\▞ ,qu13) 
        (:lw18 #\▁ ,lw18) (:lw14 #\▂ ,lw14) (:lw38 #\▃ ,lw38) (:lw12 #\▄ ,lw12) 
        (:lw58 #\▅ ,lw58) (:lw34 #\▆ ,lw34) (:lw78 #\▇ ,lw78)
        (:lf14 #\▎ ,lf14) (:lf12 #\▌ ,lf12) (:lf34 #\▊ ,lf34))))

