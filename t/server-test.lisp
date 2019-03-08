;;;; server-test.lisp

(in-package #:traytr-test)

(defun set-io-server-function/color-test (&optional (size-x 238) (size-y 66))
  (traytr:define-io-server-function io-server-function/color-test
    (traytr:initialize-terminal stream)
    (let ((bp (traytr:make-terminal-buffer size-x size-y)))
    (dotimes (i 256)
      (dotimes (x size-x)
        (dotimes (y size-y)
          (traytr:set-down-block bp x y 2 i i i (- 255 i) (- 255 i) (- 255 i))))
      (traytr:write-terminal-buffer bp stream)))
    (traytr:finalize-terminal stream))

  (setf traytr:*io-server-function* #'io-server-function/color-test))

(defun set-io-server-function/smoothness-test (&optional (size-x 238) (size-y 66))
  (traytr:define-io-server-function io-server-function/smoothness-test
    (traytr:initialize-terminal stream)
    (let ((bp (traytr:make-terminal-buffer size-x size-y)))
      (dotimes (i size-x)
        (dotimes (x size-x)
            (dotimes (y size-y)
            (traytr:set-down-block bp x y (if (= x i) 8 0)
                                   255 255 255 0 0 0)))
        (traytr:write-terminal-buffer bp stream)))
    (traytr:finalize-terminal stream))

  (setf traytr:*io-server-function* #'io-server-function/smoothness-test))

