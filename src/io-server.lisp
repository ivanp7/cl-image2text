;;;; io-server.lisp

(in-package #:traytr)

(defparameter *port* 50511)

(defvar *io-server* nil)
(defvar *io-server-thread* nil)
(defvar *io-server-stop-flag* t)

(defmacro define-io-server-function (name &body body)
  `(defun ,name (stream)
     (declare (type stream stream))
     ,@body))

(define-io-server-function echo-server
  (write-string (read-line stream) stream)
  (force-output stream))

(defvar *io-server-function* #'echo-server)

(defun start-io-server (&optional (port *port*) (host usocket:*wildcard-host*))
  (unless *io-server*
    (multiple-value-bind (thread socket) (usocket:socket-server host port *io-server-function* () 
                                                                :in-new-thread t 
                                                                :reuse-address t 
                                                                :multi-threading t)
      (setf *io-server* socket
            *io-server-thread* thread
            *io-server-stop-flag* nil)
      t))) 

(defun stop-io-server ()
  (when *io-server*
    (setf *io-server-stop-flag* t)
    (bt:destroy-thread *io-server-thread*)
    (usocket:socket-close *io-server*)
    (setf *io-server* nil
          *io-server-thread* nil)
    t))

(defmacro with-io-server ((&optional (port *port*) (host usocket:*wildcard-host*)) &body body)
  `(unwind-protect
     (when (start-io-server ,port ,host)
       ,@body)
     (stop-io-server)))

