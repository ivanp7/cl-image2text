;;;; session.lisp

(in-package #:traytr)

(defvar *master-up* nil)

(defvar *graphics-buffer-ready* (bt:make-condition-variable))
(defvar *terminal-buffer-ready* (bt:make-condition-variable))

(defvar *graphics-buffer*)
(defvar *terminal-buffer-for-conversion*)
(defvar *terminal-buffer-for-display*)
(defvar *frame* (the fixnum 0))

(defmacro define-application-function (name &body body)
  `(defun ,name (stream graphics-buffer)
     (declare (type stream stream)
              (type (simple-array fixnum (* * 3)) graphics-buffer)
              (ignorable stream graphics-buffer))
     ,@body))

(define-application-function dummy-application
  (declare (optimize (speed 3) (safety 0)))
  (with-color-buffer-size gb graphics-buffer
    (dotimes (y gb-size-y)
      (dotimes (x gb-size-x)
        (with-color-buffer-element-colors gb graphics-buffer x y
          (let ((color (round (* 255 (+ (/ (mod (+ x *frame*) gb-size-x) 
                                           gb-size-x 2) 
                                        (/ (mod (+ y *frame*) gb-size-y) 
                                           gb-size-y 2)))))) 
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
        (let* ((number-of-segments (the fixnum (cl-cpus:get-number-of-processors)))
               (segment-locks (make-array number-of-segments :element-type '(or null bt:lock)
                                          :initial-element nil))
               (segment-size-y (the fixnum (ceiling size-y number-of-segments))))
          (setf *graphics-buffer* (create-color-buffer (the fixnum (* size-x +horz-ppc+))
                                                       (the fixnum (* size-y +vert-ppc+)))
                *terminal-buffer-for-conversion* (create-terminal-buffer size-x size-y)
                *terminal-buffer-for-display* (create-terminal-buffer size-x size-y)
                *frame* 0)
          (setf *master-up* t)
          (dotimes (i number-of-segments)
            (declare (type fixnum i))
            (setf (svref segment-locks i) (bt:make-lock))
            (bt:make-thread
              (let* ((ymin (the fixnum (* i segment-size-y)))
                     (ymax (the fixnum (+ ymin segment-size-y)))
                     (segment-lock (svref segment-locks i)))
                (setf ymax (the fixnum (min ymax size-y)))
                #'(lambda ()
                    (condition-variable-loop 
                      *graphics-buffer-ready*
                      :master-up-expr
                      (progn
                        (bt:acquire-lock segment-lock t)
                        (convert-image-to-text *graphics-buffer* 
                                               *terminal-buffer-for-conversion*
                                               ymin ymax)
                        (bt:release-lock segment-lock))
                      :master-down-expr (return))))))
          (clear-terminal stream)
          (loop
            (if (funcall *application-function* stream *graphics-buffer*)
              (setf *frame* (the fixnum (1+ *frame*)))
              (return))
            (bt:condition-notify *graphics-buffer-ready*)
            (dotimes (i number-of-segments)
              (bt:acquire-lock (svref segment-locks i) t)
              (bt:release-lock (svref segment-locks i)))
            (funcall *application-postfunction* *terminal-buffer-for-conversion*)
            (rotatef *terminal-buffer-for-conversion* *terminal-buffer-for-display*)
            (bt:condition-notify *terminal-buffer-ready*)))
      (setf *master-up* nil *frame* 0)
      (bt:condition-notify *graphics-buffer-ready*)
      (bt:condition-notify *terminal-buffer-ready*)
      (sleep 1))
    (write-line "Connection refused: a master connection already exists." stream)))

(defun video-connection-handler (stream xmin ymin xmax ymax)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum xmin ymin xmax ymax))
  (initialize-terminal stream)
  (let (master-up)
    (declare (type boolean master-up))
    (condition-variable-loop 
      *terminal-buffer-ready*
      :initial-expr
      (when (eql (read-key stream #\q) #\q) (return))
      :master-up-expr
      (progn
        (setf master-up t)
        (write-terminal-buffer stream *terminal-buffer-for-display* ymin ymax xmin xmax))
      :master-down-expr
      (progn
        (when master-up
          (setf master-up nil))
        (clear-terminal stream)
        (sleep 0.5))))
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

