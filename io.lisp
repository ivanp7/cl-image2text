;;;; io.lisp

(in-package #:cl-image2text)

(defun read-image (filename &key format x y keep-ratio-p)
  "Read image from file (if FILENAME is non-NIL) or standard input.
  Assume input image format is FORMAT (if non-NIL), 
  otherwise derive it from FILENAME.
  FORMAT argument is mandatory when reading from standard input.
  If X, Y are specified, resize image respectively."
  (declare (optimize (speed 3) (safety 0))
           (type (or null string) filename format)
           (type (or null fixnum) x y))
  (flet ((process-input (img)
           (let ((img (opticl:coerce-image img +pixel-buffer-type+ 
                                           :preserve-luminance t)))
             (declare (type pixel-buffer img))
             (when (or x y)
               (opticl:with-image-bounds (height width) img
                 (declare (type fixnum width height))
                 (cond
                   ((and x y keep-ratio-p)
                    (let ((xheight (the fixnum (* x height)))
                          (ywidth (the fixnum (* y width))))
                      (if (>= ywidth xheight)
                        (setf y (round xheight width))
                        (setf x (round ywidth height)))))
                   ((and x (not y))
                    (setf y (round (the fixnum (* x height)) width)))
                   ((and y (not x))
                    (setf x (round (the fixnum (* y width)) height))))
                 (setf img (opticl:resize-image
                             img y x :interpolate :bilinear))))
             img)))
    (process-input 
      (if (or (null filename) (string= filename ""))
        (opticl:read-image-stream *standard-input* format)
        (if format
          (with-open-file (stream filename :element-type 'unsigned-byte)
            (opticl:read-image-stream stream format))
          (opticl:read-image-file filename))))))

(defparameter *color-change-tolerance* 0
  "Color ANSI codes are outputted only if color change distance is greater 
  than the tolerance value.")

(let ((default-linefeed (format nil "~C~A~%" #\Esc "[0m"))
      (byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 
  (defun write-text (terminal-buffer &key (stream *standard-output*) 
                     xmin ymin xmax ymax lineprefix linefeed)
    "Print terminal-buffer to STREAM using ANSI color codes.
    If XMIN, YMIN, XMAX, YMAX are given, narrow the outputted region 
    of the buffer. Custom line prefix and line feed sequences may be provided."
    (declare (optimize (speed 3) (safety 0))
             (type stream stream) (type terminal-buffer terminal-buffer)
             (type (or null fixnum) xmin ymin xmax ymax)
             (type (or null string) lineprefix linefeed))
    (macrolet ((write-terminal-color (type stream red green blue 
                                      last-red last-green last-blue)
      `(unless (and ,last-red ,last-green ,last-blue
                    (let ((delta-red (the fixnum 
                                          (abs (- ,red ,last-red))))
                          (delta-green (the fixnum 
                                            (abs (- ,green ,last-green))))
                          (delta-blue (the fixnum 
                                           (abs (- ,blue ,last-blue)))))
                      (<= (max delta-red delta-green delta-blue) 
                          (the fixnum *color-change-tolerance*)))) 
         (setf ,last-red ,red ,last-green ,green ,last-blue ,blue)
         (write-char #\Esc ,stream)
         (write-string ,(ecase type (:fg "[38;2;") (:bg "[48;2;")) ,stream)
         (write-string (aref byte-to-string ,red) ,stream)
         (write-char #\; ,stream)
         (write-string (aref byte-to-string ,green) ,stream)
         (write-char #\; ,stream)
         (write-string (aref byte-to-string ,blue) ,stream)
         (write-char #\m ,stream))))
      (with-terminal-buffer-size (tb-size-x tb-size-y) terminal-buffer
        (let ((xmin (or xmin 0)) (ymin (or ymin 0)) 
              (xmax (or xmax tb-size-x)) (ymax (or ymax tb-size-y))
              (lineprefix (or lineprefix "")) 
              (linefeed (or linefeed default-linefeed))
              last-fg-red last-fg-green last-fg-blue 
              last-bg-red last-bg-green last-bg-blue) 
          (declare (type fixnum ymin ymax xmin xmax))
          (loop :for y :of-type fixnum :from (max 0 ymin) 
                :below (min ymax tb-size-y) :do
                (progn
                  (setf last-fg-red nil last-fg-green nil last-fg-blue nil 
                        last-bg-red nil last-bg-green nil last-bg-blue nil)
                  (write-string lineprefix stream)
                  (loop :for x :of-type fixnum :from (max 0 xmin) 
                        :below (min xmax tb-size-x) :do
                        (with-terminal-buffer-element terminal-buffer x y
                          (write-terminal-color 
                            :fg stream fg-red fg-green fg-blue
                            last-fg-red last-fg-green last-fg-blue)
                          (write-terminal-color 
                            :bg stream bg-red bg-green bg-blue
                            last-bg-red last-bg-green last-bg-blue)
                          (write-char char stream)))
                  (write-string linefeed stream))))))
    (force-output stream)
    t))

