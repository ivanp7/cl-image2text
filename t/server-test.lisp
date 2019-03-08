;;;; server-test.lisp

(in-package #:traytr-test)

(defun set-io-server-function/color-test (&optional (size-x 238) (size-y 66))
  (traytr:define-io-server-function/with-ansi io-server-function/color-test
    (let ((bp (traytr:make-buffer-pair size-x size-y)))
    (dotimes (i 256)
      (dotimes (x size-x)
        (dotimes (y size-y)
          (traytr:set-down-block bp x y 2 i i i (- 255 i) (- 255 i) (- 255 i))))
      (traytr:write-buffer-pair bp stream))))

  (setf traytr:*io-server-function* #'io-server-function/color-test))

