;;;; buffers.lisp

(in-package #:cl-image2text)

;;; COLOR BUFFER

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
    (alexandria:with-gensyms (bufferg xg yg)
      `(let ((,bufferg ,buffer) (,xg ,x) (,yg ,y))
         (declare (ignorable ,bufferg ,xg ,yg))
         (symbol-macrolet ((,red (color-buffer-element-color red ,bufferg ,xg ,yg))
                           (,green (color-buffer-element-color green ,bufferg ,xg ,yg))
                           (,blue (color-buffer-element-color blue ,bufferg ,xg ,yg)))
                          (declare (ignorable ,red ,green ,blue))
                          ,@body)))))

(defmacro modify-places (result-type operation (&rest places) (&rest arglists) &environment env)
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

;;; TERMINAL BUFFER

(defstruct terminal-buffer
  (char-array nil :type (simple-array character (* *)))
  (fg-color-array nil :type (simple-array fixnum (* * 3)))
  (bg-color-array nil :type (simple-array fixnum (* * 3))))

(defun create-terminal-buffer (x y)
  (make-terminal-buffer 
    :char-array (make-array `(,y ,x) :element-type 'character :initial-element #\Space)
    :fg-color-array (create-color-buffer x y)
    :bg-color-array (create-color-buffer x y)))

(defmacro with-terminal-buffer-size (terminal-buffer &body body)
  `(with-color-buffer-size tb (terminal-buffer-fg-color-array ,terminal-buffer)
     ,@body))

(defmacro with-terminal-buffer-element (terminal-buffer x y &body body)
  (alexandria:once-only (terminal-buffer x y) 
    (alexandria:with-gensyms (fg-col-buf bg-col-buf char-buf)
      `(let ((,fg-col-buf (the (simple-array fixnum (* * 3))
                               (terminal-buffer-fg-color-array ,terminal-buffer)))
             (,bg-col-buf (the (simple-array fixnum (* * 3))
                               (terminal-buffer-bg-color-array ,terminal-buffer)))
             (,char-buf (the (simple-array character (* *))
                             (terminal-buffer-char-array ,terminal-buffer)))) 
         (declare (ignorable ,char-buf))
         (with-color-buffer-element-colors fg ,fg-col-buf ,x ,y
           (with-color-buffer-element-colors bg ,bg-col-buf ,x ,y
             (symbol-macrolet ((char (aref ,char-buf ,y ,x)))
               (declare (ignorable char))
               ,@body)))))))

