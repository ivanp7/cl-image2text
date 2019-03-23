;;;; session.lisp

(in-package #:traytr)

(defvar *session-online* nil)

(defvar *video-locks* ())

(let ((video-locks-collection-lock (bt:make-lock "Video locks collection lock"))) 
  (defun add-video-lock (lock)
    (bt:with-lock-held (video-locks-collection-lock)
      (push lock *video-locks*)))

  (defun remove-video-lock (lock)
    (bt:with-lock-held (video-locks-collection-lock)
      (setf *video-locks* (delete lock *video-locks* :test #'eq))))

  (defun start-video-output ()
    (declare (optimize (speed 3) (safety 0)))
    (bt:with-lock-held (video-locks-collection-lock)
      (dolist (lock *video-locks*)
        (declare (type bt:lock lock))
        (bt:release-lock lock))))
  
  (defun wait-for-video-output-finish ()
    (declare (optimize (speed 3) (safety 0)))
    (bt:with-lock-held (video-locks-collection-lock)
      (dolist (lock *video-locks*)
        (declare (type bt:lock lock))
        (bt:acquire-lock lock t)))))

(defvar *graphics-buffer*)
(defvar *frame* 0)

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
      (declare (type fixnum y))
      (dotimes (x gb-size-x)
        (declare (type fixnum x))
        (with-color-buffer-element-colors gb graphics-buffer x y
          (let* ((displ-x (the fixnum (mod (the fixnum (+ x (the fixnum *frame*))) gb-size-x)))
                 (displ-y (the fixnum (mod (the fixnum (+ y (the fixnum *frame*))) gb-size-y)))
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

(defun session-handler (stream size-x size-y)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum *frame* size-x size-y)
           (type (function (stream (simple-array fixnum (* * 3))) boolean) *application-function*)
           (inline start-video-output wait-for-video-output-finish))
  (if (not *session-online*)
    (unwind-protect
        (progn
          (setf *graphics-buffer* (create-color-buffer (the fixnum (* size-x +horz-ppc+))
                                                       (the fixnum (* size-y +vert-ppc+)))
                *frame* 0)
          (wait-for-video-output-finish)
          (setf *session-online* t)
          (clear-terminal stream)
          (loop
            (if (funcall *application-function* stream *graphics-buffer*)
              (setf *frame* (the fixnum (1+ *frame*)))
              (return))
            (start-video-output)
            (wait-for-video-output-finish)))
      (setf *session-online* nil *frame* 0)
      (start-video-output))
    (write-line "Connection refused: a master connection already exists." stream)))

(defun video-handler (stream xmin ymin xmax ymax)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum xmin ymin xmax ymax))
  (initialize-terminal stream)
  (let (session-online-flag (tb (create-terminal-buffer (the fixnum (- xmax xmin)) 
                                                        (the fixnum (- ymax ymin))))
        (lock (bt:make-lock "Video lock")))
    (declare (type boolean session-online-flag)
             (type terminal-buffer tb)
             (type bt:lock lock))
    (add-video-lock lock)
    (loop
      (when (eql (read-char-no-hang stream nil #\q) #\q) (return))
      (if *session-online*
        (progn
          (setf session-online-flag t)
          (bt:acquire-lock lock t)
          (convert-image-to-text *graphics-buffer* tb xmin ymin xmax ymax)
          (bt:release-lock lock)
          (write-terminal-buffer stream tb))
        (progn
          (when session-online-flag
            (setf session-online-flag nil)
            (clear-terminal stream))
          (sleep 0.1))))
    (remove-video-lock lock))
  (finalize-terminal stream))

(defun audio-handler (stream channel) ;; TODO
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
      ((equalp channel-type "session") 
       (let ((size-x (read stream)) (size-y (read stream)))
         (check-type size-x fixnum)
         (check-type size-y fixnum)
         (assert (plusp size-x))
         (assert (plusp size-y))
         (session-handler stream size-x size-y)))
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
         (video-handler stream xmin ymin xmax ymax)))
      ((equalp channel-type "audio") 
       (let ((channel (read stream)))
         (check-type channel symbol)
         (audio-handler stream channel))))))

(setf *io-server-function* #'connection-handler)

