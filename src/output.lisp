;;;; output.lisp

(in-package #:traytr)

(defparameter *port* 50511)

(defvar *output-server* nil)
(defvar *output-server-thread* nil)
(defvar *output-server-sockets* nil)

;; switch to alt. buffer, clear screen, hide cursor and move to (1,1)
(defconstant +initial-output-string+ 
             (format nil "~C[?1049h~:*~C[2J~:*~C[?25l~:*~C[H") #\Esc)

(let ((eof-value (gensym)))
  (defun socket-closed-p (socket)
    (let ((chr (read-char-no-hang (usocket:socket-stream socket) nil eof-value))) 
      (when chr
        (if (eq chr eof-value)
          t
          (progn
            (unread-char chr (usocket:socket-stream socket))
            nil))))))

(defun output (string socket)
  (unless (or (null *output-server*) (socket-closed-p socket))
    (write-string string (usocket:socket-stream socket))
    (force-output (usocket:socket-stream socket))
    t))

(defun output-broadcast (string)
  (unless (null *output-server*)
    (loop :while (and (not (null (first *output-server-sockets*)))
                      (socket-closed-p (first *output-server-sockets*)))
          :do (setf *output-server-sockets* (rest *output-server-sockets*)))
    (loop :for socket-sublist :on *output-server-sockets* 
          :do
          (progn
            (output string (first socket-sublist))
            (loop :while (and (not (null (second socket-sublist)))
                              (socket-closed-p (second socket-sublist)))
                  :do (setf (rest socket-sublist) (rest (rest socket-sublist))))))
    t))

(defun open-output-server (&optional (port *port*) (host usocket:*wildcard-host*))
  (unless *output-server*
    (setf *output-server* (usocket:socket-listen host port :reuse-address t))
    (setf *output-server-thread* 
          (bt:make-thread 
            #'(lambda () 
                (let (socket)
                  (loop
                    (setf socket (usocket:socket-accept *output-server*))
                    (output +initial-output-string+ socket)
                    (setf *output-server-sockets*
                          (cons socket *output-server-sockets*)))))
            :name "TRAYTR output server listener"))
    t))

(defun close-output-server ()
  (when *output-server*
    (bt:destroy-thread *output-server-thread*)
    (setf *output-server-thread* nil)
    (dolist (socket *output-server-sockets*)
      (unless (socket-closed-p socket)
        (usocket:socket-close socket)))
    (setf *output-server-sockets* nil)
    (usocket:socket-close *output-server*)
    (setf *output-server* nil)
    t))

(defmacro with-output-server ((&optional (port *port*) (host usocket:*wildcard-host*)) &body body)
  `(unwind-protect
       (progn
         (when (open-output-server ,port ,host)
           ,@body))
     (close-output-server)))

