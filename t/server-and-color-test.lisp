;;;; server-test.lisp

(in-package #:traytr-test)

(setf traytr::*initial-output-string* traytr::+ansi-initialization-string+)
(setf traytr::*final-output-string* traytr::+ansi-finalization-string+)

(defun broadcast-color-strip ()
  (traytr:with-output-server 
    ()
    (dotimes (j 5)
      (sleep 5)
      (traytr:output-broadcast 
        (loop :for i :from 0 :to 255 :with str := ""
              :do (setf str
                        (concatenate 
                          'string str
                          (traytr:ansi-cc-string 
                            (traytr:make-cc 
                              #\Space 
                              (traytr:make-color (- 255 i) i (- 255 i))
                              (traytr:make-color i (- 255 i) i)))))
              :finally (return str))))))

