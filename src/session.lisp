;;;; session.lisp

(in-package #:traytr)

(defvar *master-up* nil)

(defconstant +converter-threads+ #.(cl-cpus:get-number-of-processors))
(defmacro define-converter-locks ()
  `(defvar *converter-locks* 
     (make-array ,+converter-threads+ :element-type 'bt:lock 
                 :initial-contents (list ,@(loop :for i :below +converter-threads+
                                                 :collect `(bt:make-lock))))))
(define-converter-locks)

(defvar *graphics-buffer-ready* (bt:make-condition-variable))
(defvar *terminal-buffer-ready* (bt:make-condition-variable))

(defvar *graphics-buffer*)
(defvar *terminal-buffer*)
(defvar *frame* 0)

(defmacro define-application-function (name &body body)
  `(defun ,name (stream graphics-buffer)
     (declare (type stream stream)
              (type (simple-array fixnum (* * 3)) graphics-buffer)
              (type fixnum *frame*)
              (ignorable stream graphics-buffer))
     ,@body))

(define-application-function dummy-application
  (declare (optimize (speed 3) (safety 0)))
  (with-color-buffer-size gb graphics-buffer
    (dotimes (y gb-size-y)
      (declare (type fixnum y))
      (dotimes (x gb-size-x)
        (declare (type fixnum x))
        (with-color-buffer-element-colors gb graphics-buffer x y
          (let* ((displ-x (the fixnum (mod (the fixnum (+ x *frame*)) gb-size-x)))
                 (displ-y (the fixnum (mod (the fixnum (+ y *frame*)) gb-size-y)))
                 (displ-xy (the fixnum (* displ-x gb-size-y)))
                 (displ-yx (the fixnum (* displ-y gb-size-x)))
                 (xyyx (the fixnum (+ displ-xy displ-yx)))
                 (xyyx255 (the fixnum (* 255 xyyx)))
                 (color (round xyyx255 (the fixnum (* 2 gb-size-x gb-size-y))))) 
            (setf gb-red color gb-green color gb-blue color))))))
  (let ((keypress (read-key stream #\q)))
    (when keypress
      (write-char keypress stream)
      (force-output stream))
    (not (eql keypress #\q))))

(defvar *application-function* #'dummy-application)
(defvar *application-postfunction* (constantly nil))

(defmacro condition-variable-loop (condition-variable &key initial-expr 
                                                      master-up-expr master-down-expr)
  `(let ((lock (the bt:lock (bt:make-lock))) (frame 0)) 
     (bt:with-lock-held (lock)
       (loop
         (progn
           ,initial-expr
           (if *master-up*
             (progn
               (bt:condition-wait ,condition-variable lock)
               (when (< frame (the fixnum *frame*))
                 (setf frame (the fixnum *frame*))
                 ,master-up-expr))
             ,master-down-expr))))))

(defun master-connection-handler (stream size-x size-y)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum *frame* size-x size-y)
           (type (function (stream (simple-array fixnum (* * 3))) boolean) *application-function*)
           (type (function (terminal-buffer)) *application-postfunction*))
  (if (not *master-up*)
    (unwind-protect
        (let* ((segment-size-y (the fixnum (ceiling (the (unsigned-byte 64) size-y) 
                                                    +converter-threads+))))
          (setf *graphics-buffer* (create-color-buffer (the fixnum (* size-x +horz-ppc+))
                                                       (the fixnum (* size-y +vert-ppc+)))
                *terminal-buffer* (create-terminal-buffer size-x size-y)
                *frame* 0)
          (setf *master-up* t)
          (dotimes (i +converter-threads+)
            (declare (type fixnum i))
            (let* ((ymin (the fixnum (* i segment-size-y)))
                   (ymax0 (the fixnum (+ ymin segment-size-y)))
                   (ymax (the fixnum (min ymax0 size-y)))
                   (segment-lock (svref *converter-locks* i)))
              (bt:make-thread
                #'(lambda ()
                    (condition-variable-loop 
                      *graphics-buffer-ready*
                      :master-up-expr
                      (progn
                        (bt:acquire-lock segment-lock t)
                        (convert-image-to-text *graphics-buffer* *terminal-buffer* ymin ymax)
                        (bt:release-lock segment-lock))
                      :master-down-expr (return)))
                :name "Picture-to-text converter thread")))
          (clear-terminal stream)
          (loop
            (if (funcall *application-function* stream *graphics-buffer*)
              (setf *frame* (the fixnum (1+ *frame*)))
              (return))
            (bt:condition-notify *graphics-buffer-ready*)
            (dotimes (i +converter-threads+)
              (bt:acquire-lock (svref *converter-locks* i) t))
            (dotimes (i +converter-threads+)
              (bt:release-lock (svref *converter-locks* i)))
            (funcall *application-postfunction* *terminal-buffer*)
            (bt:condition-notify *terminal-buffer-ready*)
            (sleep 0.1)))
      (setf *master-up* nil *frame* 0)
      (bt:condition-notify *graphics-buffer-ready*)
      (bt:condition-notify *terminal-buffer-ready*))
    (write-line "Connection refused: a master connection already exists." stream)))

(defun video-connection-handler (stream xmin ymin xmax ymax)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum xmin ymin xmax ymax))
  (initialize-terminal stream)
  (let (master-up (tb (create-terminal-buffer (the fixnum (- xmax xmin)) 
                                              (the fixnum (- ymax ymin)))))
    (declare (type boolean master-up))
    (condition-variable-loop 
      *terminal-buffer-ready*
      :initial-expr
      (when (eql (read-key stream #\q) #\q) (return))
      :master-up-expr
      (progn
        (setf master-up t)
        (clone-terminal-buffer tb *terminal-buffer* xmin ymin xmax ymax)
        (write-terminal-buffer stream tb ymin ymax xmin xmax))
      :master-down-expr
      (progn
        (when master-up
          (setf master-up nil)
          (clear-terminal stream))
        (sleep 0.5)
        (format stream "X"))))
  (finalize-terminal stream))

(defun audio-connection-handler (stream channel) ;; TODO
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type symbol channel))
  (declare (ignore stream))
  (ecase channel
    (sounds nil)
    (music nil)))

(define-io-server-function connection-handler
  (declare (optimize (speed 3) (safety 0)))
  (let ((channel-type (read-line stream)))
    (cond
      ((equalp channel-type "master") 
       (let ((size-x (read stream)) (size-y (read stream)))
         (check-type size-x fixnum)
         (check-type size-y fixnum)
         (assert (plusp size-x))
         (assert (plusp size-y))
         (master-connection-handler stream size-x size-y)))
      ((equalp channel-type "video") 
       (let ((xmin (read stream)) (ymin (read stream)) 
             (xmax (read stream)) (ymax (read stream)))
         (check-type xmin fixnum)
         (check-type ymin fixnum)
         (check-type xmax fixnum)
         (check-type ymax fixnum)
         (assert (>= xmin 0))
         (assert (>= ymin 0))
         (assert (> xmax xmin))
         (assert (> ymax ymin))
         (video-connection-handler stream xmin ymin xmax ymax)))
      ((equalp channel-type "audio") 
       (let ((channel (read stream)))
         (check-type channel symbol)
         (audio-connection-handler stream channel))))))

(setf *io-server-function* #'connection-handler)

