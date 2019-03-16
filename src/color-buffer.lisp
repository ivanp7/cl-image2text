;;;; color-buffer.lisp

(in-package #:traytr)

(defun create-color-buffer (x y)
  (make-array `(,y ,x 3) :element-type 'fixnum :initial-element 0))

(defmacro with-color-buffer-size (var-prefix buffer &body body)
  (let* ((prefixstr (symbol-name var-prefix)) 
         (x (intern (concatenate 'string prefixstr "-SIZE-X")))
         (y (intern (concatenate 'string prefixstr "-SIZE-Y")))) 
    (alexandria:once-only (buffer)
      `(let ((,x (array-dimension ,buffer 1))
             (,y (array-dimension ,buffer 0)))
         ,@body))))

(defmacro color-buffer-element-color (color buffer x y)
  (ecase color
    (red `(aref ,buffer ,y ,x 0))
    (green `(aref ,buffer ,y ,x 1))
    (blue `(aref ,buffer ,y ,x 2))))

(defmacro with-color-buffer-element-colors (var-prefix buffer x y &body body)
  (let* ((prefixstr (symbol-name var-prefix)) 
         (red (intern (concatenate 'string prefixstr "-RED")))
         (green (intern (concatenate 'string prefixstr "-GREEN")))
         (blue (intern (concatenate 'string prefixstr "-BLUE")))) 
    (alexandria:once-only (buffer x y)
      `(symbol-macrolet ((,red (color-buffer-element-color red ,buffer ,x ,y))
                         (,green (color-buffer-element-color green ,buffer ,x ,y))
                         (,blue (color-buffer-element-color blue ,buffer ,x ,y)))
         ,@body))))

