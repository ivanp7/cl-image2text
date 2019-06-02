;;;; io.lisp

(in-package #:cl-image2text)

(defun read-image (filename &key x y)
  (let ((img (opticl:coerce-image (opticl:read-image-file filename)
                                  +pixel-buffer-type+ :preserve-luminance t)))
    (when (or x y)
      (opticl:with-image-bounds (height width) img
        (let ((width (if x (* x +horz-ppc+) width)) (height (if y (* y +vert-ppc+) height)))
          (setf img (opticl:resize-image img height width :interpolate :bilinear)))))
    img))

(defparameter *color-change-tolerance* 0)

(let ((linefeed (format nil "~C[0m~%" #\Esc))
      (byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 
  (defun write-text (terminal-buffer &key (stream *standard-output*) xmin ymin xmax ymax)
    (declare (optimize (speed 3) (safety 0))
             (type stream stream) (type terminal-buffer terminal-buffer)
             (type (or null fixnum) xmin ymin xmax ymax))
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
      (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
        (let ((xmin (or xmin 0)) (ymin (or ymin 0)) 
              (xmax (or xmax tb-size-x)) (ymax (or ymax tb-size-y))
              last-fg-red last-fg-green last-fg-blue last-bg-red last-bg-green last-bg-blue) 
          (declare (type fixnum ymin ymax xmin xmax))
          (loop :for y :of-type fixnum :from (max 0 ymin) :below (min ymax tb-size-y) :do
                (progn
                  (setf last-fg-red nil last-fg-green nil last-fg-blue nil 
                        last-bg-red nil last-bg-green nil last-bg-blue nil)
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

