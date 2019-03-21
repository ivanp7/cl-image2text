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
  (let ((keypress (the (or null character) (read-char-no-hang stream nil #\q))))
    (not (eql keypress #\q))))

(defvar *application-function* #'dummy-application)

(defmacro condition-variable-loop (condition-variable &key initial-expr 
                                                      master-up-expr master-down-expr)
  `(let ((lock (the bt:lock (bt:make-lock))) (frame 0)) 
     (loop
       (progn
         ,initial-expr
         (if *master-up*
           (progn
             (bt:condition-wait ,condition-variable lock)
             (when (/= frame (the fixnum *frame*))
               (setf frame (the fixnum *frame*))
               ,master-up-expr))
           ,master-down-expr)))))

(defun master-connection-handler (stream size-x size-y)
  (declare (optimize (speed 3) (safety 0))
           (type stream stream)
           (type fixnum *frame* size-x size-y)
           (type (function (stream (simple-array fixnum (* * 3))) boolean) *application-function*))
  (if *master-up*
    (let* ((number-of-segments (the fixnum (cl-cpus:get-number-of-processors)))
           (segment-conditions (make-array number-of-segments))
           (segment-locks (make-array number-of-segments :element-type '(or null bt:lock)
                                      :initial-element nil))
           (segment-size-y (the fixnum (ceiling size-y number-of-segments))))
      (dotimes (i number-of-segments)
        (declare (type fixnum i))
        (setf (svref segment-conditions i) (bt:make-condition-variable))
        (setf (svref segment-locks i) (bt:make-lock))
        (bt:make-thread
          (let* ((ymin (the fixnum (* i segment-size-y)))
                 (ymax (the fixnum (+ ymin segment-size-y)))
                 (segment-condition (svref segment-conditions i)))
            (setf ymax (the fixnum (min ymax size-y)))
            #'(lambda ()
                (condition-variable-loop 
                  *graphics-buffer-ready*
                  :master-up-expr
                  (progn
                    (convert-image-to-text *graphics-buffer* 
                                           *terminal-buffer-for-conversion*
                                           ymin ymax)
                    (bt:condition-notify segment-condition))
                  :master-down-expr (return))))))
      (setf *graphics-buffer* (create-color-buffer (the fixnum (* size-x +horz-ppc+))
                                                   (the fixnum (* size-y +vert-ppc+)))
            *terminal-buffer-for-conversion* (create-terminal-buffer size-x size-y)
            *terminal-buffer-for-display* (create-terminal-buffer size-x size-y)
            *frame* 0)
      (setf *master-up* t)
      (loop
        (if (funcall *application-function* stream *graphics-buffer*)
          (setf *frame* (the fixnum (1+ *frame*)))
          (return))
        (bt:condition-notify *graphics-buffer-ready*)
        (dotimes (i number-of-segments)
          (bt:condition-wait (svref segment-conditions i) (svref segment-locks i)))
        (rotatef *terminal-buffer-for-conversion* *terminal-buffer-for-display*)
        (bt:condition-notify *terminal-buffer-ready*))
      (setf *master-up* nil)
      (bt:condition-notify *graphics-buffer-ready*)
      (bt:condition-notify *terminal-buffer-ready*))
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
      (let ((keypress (the (or null character) (read-char-no-hang stream nil #\q))))
        (when (eql keypress #\q) (return)))
      :master-up-expr
      (progn
        (setf master-up t)
        (write-terminal-buffer stream *terminal-buffer-for-display* ymin ymax xmin xmax))
      :master-down-expr
      (progn
        (when master-up
          (clear-terminal stream)
          (setf master-up nil))
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
  (let ((channel-type (read stream)))
    (ecase channel-type
      (master (let ((size-x (read stream)) (size-y (read stream)))
                (check-type size-x fixnum)
                (check-type size-y fixnum)
                (assert (plusp size-x))
                (assert (plusp size-y))
                (master-connection-handler stream size-x size-y)))
      (video (let ((xmin (read stream)) (ymin (read stream)) 
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
      (audio (let ((channel (read stream)))
               (check-type channel symbol)
               (audio-connection-handler stream channel))))))

(setf *io-server-function* #'connection-handler)

