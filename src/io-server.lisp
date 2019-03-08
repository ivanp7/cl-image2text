;;;; io-server.lisp

(in-package #:traytr)

(defparameter *port* 50511)

(defvar *io-server-function* 
  #'(lambda (stream)
      (declare (type stream stream))
      (write-string (read-line stream) stream)))

(defvar *io-server* nil)
(defvar *io-server-thread* nil)

(defun server-running-p ()
  (not (null *io-server*)))

(defun start-io-server (&optional (port *port*) (host usocket:*wildcard-host*))
  (unless *io-server*
    (multiple-value-bind (thread socket) (usocket:socket-server host port *io-server-function* () 
                                                                :in-new-thread t 
                                                                :reuse-address t 
                                                                :multi-threading t)
      (setf *io-server* socket
            *io-server-thread* thread)
      t))) 

(defun stop-io-server ()
  (when *io-server*
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

