;;;; color-buffer.lisp

(in-package #:traytr)

(defun create-color-buffer (x y)
  (make-array `(,y ,x 3) :element-type 'fixnum :initial-element 0))

(defmacro with-color-buffer-size (var-prefix buffer &body body)
  (let* ((prefixstr (symbol-name var-prefix)) 
         (x (intern (concatenate 'string prefixstr "-SIZE-X")))
         (y (intern (concatenate 'string prefixstr "-SIZE-Y")))) 
    (alexandria:once-only (buffer)
      `(let ((,x (the fixnum (array-dimension ,buffer 1)))
             (,y (the fixnum (array-dimension ,buffer 0))))
         ,@body))))

(defmacro color-buffer-element-color (color buffer x y)
  (ecase color
    (red `(the fixnum (aref ,buffer ,y ,x 0)))
    (green `(the fixnum (aref ,buffer ,y ,x 1)))
    (blue `(the fixnum (aref ,buffer ,y ,x 2)))))

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

(defmacro modify-places (operation result-type (&rest places) (&rest arglists) &environment env)
  (if (/= (length places) (length arglists))
    (error "Number of arglists must correspond to the number of places")
    (flet ((generate-setf (setf-exp arglist)
             (unless (listp arglist)
               (setf arglist (list arglist)))
             (let ((vars (first setf-exp)) (forms (second setf-exp)) (var (car (third setf-exp)))
                   (set (fourth setf-exp)) (access (fifth setf-exp)))
               `(let (,@(mapcar #'list vars forms))
                  (let ((,var (the ,result-type (,operation ,access ,@arglist))))
                    ,set)))))
      `(progn ,@(mapcar #'generate-setf 
                        (mapcar #'(lambda (place)
                                    (multiple-value-list 
                                      (get-setf-expansion place env)))
                                places)
                        arglists)))))

