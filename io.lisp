;;;; io.lisp

(in-package #:cl-image2text)

(defun read-image (filename &key x y)
  (let ((img (opticl:read-image-file filename)))
    (opticl:with-image-bounds (height width) img
      (when (or x y)
        (setf width (or x width) height (or y height)
              img (opticl:resize-image img height width :interpolate :bilinear)))
      (let* ((pixel-buffer (create-color-buffer width height)))
        (dotimes (j width)
          (dotimes (i height)
            (with-color-buffer-element-colors px pixel-buffer j i
              (multiple-value-bind (r g b) (opticl:pixel img i j)
                (setf px-red r px-green g px-blue b)))))
        pixel-buffer))))

(defparameter *color-change-tolerance* 0)

(let ((linefeed (format nil "~C[0m~%" #\Esc))
      (byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 
  (defun write-terminal-buffer (terminal-buffer &optional (stream *standard-output*) 
                                                (ymin 0) ymax (xmin 0) xmax)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream) (type terminal-buffer terminal-buffer))
    (macrolet ((write-terminal-color (type stream red green blue 
                                      last-red last-green last-blue)
      `(when (or (null ,last-red) (null ,last-green) (null ,last-blue)
                 (let ((delta-red (the fixnum (abs (- ,red ,last-red))))
                       (delta-green (the fixnum (abs (- ,green ,last-green))))
                       (delta-blue (the fixnum (abs (- ,blue ,last-blue)))))
                   (or (> delta-red (the fixnum *color-change-tolerance*))
                       (> delta-green (the fixnum *color-change-tolerance*))
                       (> delta-blue (the fixnum *color-change-tolerance*))))) 
         (setf ,last-red ,red ,last-green ,green ,last-blue ,blue)
         (write-char #\Esc ,stream)
         (write-string ,(ecase type (fg "[38;2;") (bg "[48;2;")) ,stream)
         (write-string (aref byte-to-string ,red) ,stream)
         (write-char #\; ,stream)
         (write-string (aref byte-to-string ,green) ,stream)
         (write-char #\; ,stream)
         (write-string (aref byte-to-string ,blue) ,stream)
         (write-char #\m ,stream))))
      (with-terminal-buffer-size terminal-buffer
        (let ((ymax (or ymax tb-size-y)) (xmax (or xmax tb-size-x))
              last-fg-red last-fg-green last-fg-blue last-bg-red last-bg-green last-bg-blue) 
          (declare (type fixnum ymin ymax xmin xmax))
          (loop :for y :of-type fixnum :from (max 0 ymin) :below (min ymax tb-size-y) :do
                (progn
                  (loop :for x :of-type fixnum :from (max 0 xmin) :below (min xmax tb-size-x) :do
                        (with-terminal-buffer-element terminal-buffer x y
                          (write-terminal-color fg stream fg-red fg-green fg-blue
                                                last-fg-red last-fg-green last-fg-blue)
                          (write-terminal-color bg stream bg-red bg-green bg-blue
                                                last-bg-red last-bg-green last-bg-blue)
                          (write-char char stream)))
                  (write-string linefeed stream))))))
    (force-output stream)
    t))

